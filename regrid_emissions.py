import os
import glob
import itertools
import time
import xarray as xr
import pandas as pd
import numpy as np

try:
    import xesmf as xe
except ImportError:
    raise ImportError(
        "The 'xesmf' library is required for regridding. "
        "Please install it using: conda install -c conda-forge xesmf"
    )

def _normalize_bounds(ds: xr.Dataset) -> xr.Dataset:
    """
    Helper function to normalize coordinate boundary names and shapes for xesmf.
    xesmf looks for 'lat_b' and 'lon_b' (corners of the grid cells).
    CF conventions typically store these as 'lat_bnds'/'lon_bnds' with shapes 
    (y, x, 4) for 2D grids or (x, 2) for 1D grids.
    """
    ds = ds.copy()
    rename_dict = {}
    
    for bnd_name, new_name in [('lat_bnds', 'lat_b'), ('lon_bnds', 'lon_b')]:
        if bnd_name in ds:
            bnd_da = ds[bnd_name]
            
            # CF 2D Grid Bounds: (y, x, 4 vertices) -> xesmf (y+1, x+1 corners)
            if bnd_da.ndim == 3 and bnd_da.shape[-1] == 4:
                y_size, x_size = bnd_da.shape[0], bnd_da.shape[1]
                corners = np.zeros((y_size + 1, x_size + 1))
                
                # Standard CF counter-clockwise ordering: BL, BR, TR, TL
                corners[:-1, :-1] = bnd_da.values[:, :, 0] # Bottom-Left
                corners[:-1, 1:]  = bnd_da.values[:, :, 1] # Bottom-Right
                corners[1:, 1:]   = bnd_da.values[:, :, 2] # Top-Right
                corners[1:, :-1]  = bnd_da.values[:, :, 3] # Top-Left
                
                y_dim, x_dim = bnd_da.dims[0], bnd_da.dims[1]
                ds[new_name] = xr.DataArray(corners, dims=(f"{y_dim}_b", f"{x_dim}_b"))
                ds = ds.drop_vars(bnd_name)
            
            # CF 1D Grid Bounds: (x, 2 bounds) -> xesmf (x+1 corners)
            elif bnd_da.ndim == 2 and bnd_da.shape[-1] == 2:
                n_size = bnd_da.shape[0]
                corners = np.zeros(n_size + 1)
                
                corners[:-1] = bnd_da.values[:, 0]
                corners[-1]  = bnd_da.values[-1, 1]
                
                dim = bnd_da.dims[0]
                ds[new_name] = xr.DataArray(corners, dims=(f"{dim}_b",))
                ds = ds.drop_vars(bnd_name)
                
            else:
                # Fallback to simple rename if shape is already appropriate or unknown
                rename_dict[bnd_name] = new_name
                
    if rename_dict:
        ds = ds.rename(rename_dict)
        
    return ds

def setup_encoding(ds: xr.Dataset) -> dict:
    """
    Generates an optimized encoding dictionary for a netCDF dataset.
    Prioritizes zlib compression with balanced chunk sizes.
    """
    encoding = {}
    for var in ds.data_vars:
        # Defaults for compression
        var_encoding = {
            'zlib': True,
            'complevel': 4, # Balance between compression speed and size reduction
        }
        
        # Setup chunking based on dimensions
        # Time and Level are usually small per file, spatial dims can be chunked
        chunks = []
        for dim in ds[var].dims:
            if dim == 'time':
                chunks.append(1) # Chunk by single time step
            elif dim == 'level':
                chunks.append(ds.sizes.get(dim, 1)) # Full level block
            else:
                # Spatial dimensions (y, x, lat, lon)
                chunks.append(min(ds.sizes.get(dim, 1), 256)) # Chunk spatial up to 256
        
        if chunks:
            var_encoding['chunksizes'] = tuple(chunks)
            
        encoding[var] = var_encoding
    return encoding

def regrid_daily_emissions(
    base_dir: str,
    output_dir: str,
    grid_info_file: str,
    target_grid,
    sectors: list,
    years: list,
    months: list,
    days_of_week: list,
    variables: list = None,
    regrid_method: str = 'conservative',
    sum_dims: list = None,
):
    """
    Iterates over emissions netCDF files, regrids them to a target grid, 
    and saves them in an identical directory structure in a new location.
    
    Args:
        base_dir: Base directory path up to the GRA2PESv2.0beta_data folder
        output_dir: Target base directory for the regridded files
        grid_info_file: Path to the netCDF file containing input grid bounds
        target_grid: Path to a CF-1.8 netCDF file OR a dictionary describing the target grid 
                     (must contain lat, lon, and optionally lat_bnds, lon_bnds for conservative)
        sectors: List of sector strings (e.g., ['total', 'WASTE'])
        years: List of year strings (e.g., ['2021'])
        months: List of month strings (e.g., ['01', '02'])
        days_of_week: List of day of week strings (e.g., ['weekdy', 'satdy'])
        variables: Optional list of variable names to regrid. If None, regrids all variables.
        regrid_method: xesmf regridding algorithm ('conservative', 'bilinear', 'nearest_s2d', etc.)
        sum_dims: Optional list of dimensions to sum over prior to regridding (e.g., ['level']).
    """
    start_time = time.time()
    
    # 1. Load Input Grid Info
    print(f"Loading input grid info from {grid_info_file}...")
    ds_grid_in = xr.open_dataset(grid_info_file, cache=False)
    ds_grid_in = _normalize_bounds(ds_grid_in)
    
    # 2. Process Target Grid
    print("Preparing target grid...")
    
    # Track CF grid mapping variables from the target grid
    target_grid_mapping_vars = set()
    
    if isinstance(target_grid, str):
        ds_grid_out = xr.open_dataset(target_grid, cache=False)
        
        # Identify grid mapping variables by the CF 'grid_mapping_name' attribute
        for var_name, var in ds_grid_out.variables.items():
            if 'grid_mapping_name' in var.attrs:
                target_grid_mapping_vars.add(var_name)
                
        # Identify grid mapping variables by reference from data variables
        for var in ds_grid_out.data_vars.values():
            if 'grid_mapping' in var.attrs:
                target_grid_mapping_vars.add(var.attrs['grid_mapping'])
                
        target_grid_mapping_vars = [v for v in target_grid_mapping_vars if v in ds_grid_out.variables]
        
        if target_grid_mapping_vars:
            print(f"Found target grid mapping variable(s): {target_grid_mapping_vars}")
            
    elif isinstance(target_grid, dict):
        ds_grid_out = xr.Dataset(target_grid)
    else:
        raise TypeError("target_grid must be a file path (str) or a dictionary.")
    
    ds_grid_out = _normalize_bounds(ds_grid_out)
    
    # Validate conservative bounds check
    if regrid_method == 'conservative':
        if 'lat_b' not in ds_grid_in or 'lon_b' not in ds_grid_in:
            raise ValueError("Input grid info must contain bounds (lat_bnds/lon_bnds) for conservative regridding.")
        if 'lat_b' not in ds_grid_out or 'lon_b' not in ds_grid_out:
            raise ValueError("Target grid must contain bounds (lat_bnds/lon_bnds) for conservative regridding.")

    # 3. Build the Regridder 
    # (Doing this once outside the loop saves significant computation time)
    print(f"Building regridder object (Method: {regrid_method})...")
    regridder = xe.Regridder(
        ds_grid_in, 
        ds_grid_out, 
        method=regrid_method, 
        unmapped_to_nan=False, # Handles values outside grid coverage
    )
    print("Regridder initialized successfully.")

    # 4. Generate combinations and iterate
    combinations = list(itertools.product(sectors, years, months, days_of_week))
    total_iters = len(combinations)
    print(f"Starting processing for {total_iters} combinations...")
    
    for idx, (sector, year, month, day) in enumerate(combinations, 1):
        # Construct path string
        folder_path = os.path.join(
            base_dir#, 
            #f"GRA2PESv2.0beta_{sector}", 
            # f"{year}{month}", 
            # day
        )
        file_pattern = os.path.join(
            folder_path, 
            #f"GRA2PESv2.0beta_{sector}_{year}{month}_{day}_*.nc"
            f"GRA2PESv1.1_{sector}_{year}{month}_{day}_*.nc"
        )
        
        files = glob.glob(file_pattern)
        if not files:
            print(f"[{idx}/{total_iters}] Missing files for pattern: {file_pattern} - Skipping.")
            continue
            
        print(f"[{idx}/{total_iters}] Processing {len(files)} files for {sector} | {year}-{month} | {day}...")
        
        # Prepare identical output directory structure
        rel_folder = os.path.relpath(folder_path, base_dir)
        target_folder = os.path.join(output_dir, rel_folder)
        os.makedirs(target_folder, exist_ok=True)
        
        for file in files:
            try:
                # Open with chunking for dask performance, cache=False for memory limits
                # If we are summing over a dimension like 'level', it's faster to load that whole dimension into the chunk
                chunks = {'time': 1}
                if sum_dims is None or 'level' not in sum_dims:
                    chunks['level'] = 1
                    
                ds_in = xr.open_dataset(file, chunks=chunks, cache=False)
                
                # Subset variables if specified
                if variables is not None:
                    # Keep variables requested + dimension coordinates
                    vars_to_keep = variables + list(ds_in.coords.keys())
                    vars_to_keep = [v for v in vars_to_keep if v in ds_in.variables]
                    ds_in = ds_in[vars_to_keep]
                
                # Sum over specified dimensions BEFORE regridding for maximum speed
                if sum_dims:
                    valid_sum_dims = [d for d in sum_dims if d in ds_in.dims]
                    if valid_sum_dims:
                        # keep_attrs=True preserves units, grid_mapping, and other metadata
                        ds_in = ds_in.sum(dim=valid_sum_dims, keep_attrs=True)

                # Regrid the dataset
                # Dask delays computation until to_netcdf is called, saving memory
                ds_out = regridder(ds_in, keep_attrs=True)
                
                # Update Grid Mapping
                # 1. Remove old grid_mapping attributes that might have carried over from ds_in
                for var in ds_out.data_vars:
                    if 'grid_mapping' in ds_out[var].attrs:
                        del ds_out[var].attrs['grid_mapping']
                        
                # 2. Add target grid mapping variables and attributes if they exist
                if target_grid_mapping_vars:
                    for mapping_var in target_grid_mapping_vars:
                        ds_out[mapping_var] = ds_grid_out[mapping_var]
                    
                    # Apply primary mapping to variables with spatial dimensions
                    primary_mapping = target_grid_mapping_vars[0]
                    for var in ds_out.data_vars:
                        if var not in target_grid_mapping_vars:
                            # Apply only if the variable is spatial
                            if any(dim in ds_out[var].dims for dim in ['lat', 'lon', 'y', 'x']):
                                ds_out[var].attrs['grid_mapping'] = primary_mapping
                
                # Ensure spatial variables are officially registered as coordinates
                coords_to_set = [c for c in ['lat', 'lon', 'lat_b', 'lon_b'] if c in ds_out.variables]
                if coords_to_set:
                    ds_out = ds_out.set_coords(coords_to_set)
                
                # Enforce CF-1.8 formatting and attributes for lat and lon
                if 'lat' in ds_out.coords:
                    ds_out['lat'].attrs['standard_name'] = 'latitude'
                    ds_out['lat'].attrs['long_name'] = 'latitude coordinate'
                    ds_out['lat'].attrs['units'] = 'degrees_north'
                    ds_out['lat'].attrs['axis'] = 'Y'
                    if 'lat_b' in ds_out.variables:
                        ds_out['lat'].attrs['bounds'] = 'lat_b'
                        
                if 'lon' in ds_out.coords:
                    ds_out['lon'].attrs['standard_name'] = 'longitude'
                    ds_out['lon'].attrs['long_name'] = 'longitude coordinate'
                    ds_out['lon'].attrs['units'] = 'degrees_east'
                    ds_out['lon'].attrs['axis'] = 'X'
                    if 'lon_b' in ds_out.variables:
                        ds_out['lon'].attrs['bounds'] = 'lon_b'
                
                # Update CF Compliance metadata
                ds_out.attrs['Conventions'] = 'CF-1.8'
                ds_out.attrs['history'] = f"Regridded using xesmf ({regrid_method})" + \
                                          ds_out.attrs.get('history', '')
                
                # Format Output Path
                filename = os.path.basename(file)
                output_file = os.path.join(target_folder, filename)
                
                # Generate encoding for optimal chunking and compression
                encoding = setup_encoding(ds_out)
                
                # Save to disk, triggering the Dask computation chunk-by-chunk
                ds_out.to_netcdf(output_file, encoding=encoding, engine='netcdf4')
                
                # Cleanup to release memory immediately
                ds_in.close()
                ds_out.close()
                
            except Exception as e:
                print(f"  -> Error processing {os.path.basename(file)}: {e}")

    elapsed = time.time() - start_time
    print(f"\nProcessing complete in {elapsed:.2f} seconds!")
    print(f"Regridded files are located in {output_dir}")


if __name__ == "__main__":
    # ==========================================
    # USER CONFIGURATION
    # ==========================================
    
    BASE_DIR = "/no_backup/erwh/GRA2PES/CO_surface"
    GRID_INFO_FILE = "/no_backup/erwh/GRA2PES/GRA2PESv2.0_CONUS4km_grid_info.nc" # grid info file for GRA2PES,
    # downloaded July 14 2026 from https://csl.noaa.gov/groups/csl4/gra2pes/datasets/
    OUTPUT_DIR = "/no_backup/erwh/GRA2PES/CO_surface_regridded"
    
    
    # Example: Defining target grid via dictionary (can also be a file path containing a CF formatted grid (rectilinear or curvelinear))
    # Using 1-degree global grid as a placeholder
    TARGET_GRID = {
        # 'lat': (['lat'], np.arange(-89.5, 90, 1.0)),
        # 'lon': (['lon'], np.arange(-179.5, 180, 1.0)),
        # 'lat_bnds': (['lat', 'bnds'], np.array([[l-0.5, l+0.5] for l in np.arange(-89.5, 90, 1.0)])),
        # 'lon_bnds': (['lon', 'bnds'], np.array([[l-0.5, l+0.5] for l in np.arange(-179.5, 180, 1.0)])),
        'lat': (['lat'], np.arange(-89.95, 90, 0.1)),
        'lon': (['lon'], np.arange(-179.95, 180, 0.1)),
        'lat_bnds': (['lat', 'bnds'], np.array([[l-0.05, l+0.05] for l in np.arange(-89.95, 90, 0.1)])),
        'lon_bnds': (['lon', 'bnds'], np.array([[l-0.05, l+0.05] for l in np.arange(-179.95, 180, 0.1)])),
    }
    #TARGET_GRID='/wrk/users/charkins/emissions/CAMSv6.2/CAMS-GLOB-ANT_Glb_0.1x0.1_anthro_co2_excl_short-cycle_org_C_v6.2_monthly.nc'
    #TARGET_GRID ="/no_backup/erwh/EDGAR/co/monthly_sector_specific_gridmaps/v8.1/v8.1_FT2022_AP_CO_2022_bkl_WASTE_emi.nc"
    
    SECTORS = ["total"] 
    YEARS = ["2021", "2022", "2023"]
    MONTHS = ["01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"]
    DAYS_OF_WEEK = ["weekdy", "sundy", "satdy"]
    
    # Process only specific variables to save time/memory, or set to None for all
    VARIABLES_TO_REGRID = ["CO"] 
    
    REGRID_METHOD = "conservative"
    
    # Optional list of dimensions to sum over (e.g., collapsing vertical levels into a single column)
    SUM_DIMS = ["level"]
    
    # ==========================================
    # EXECUTION
    # ==========================================
    regrid_daily_emissions(
        base_dir=BASE_DIR,
        output_dir=OUTPUT_DIR,
        grid_info_file=GRID_INFO_FILE,
        target_grid=TARGET_GRID,
        sectors=SECTORS,
        years=YEARS,
        months=MONTHS,
        days_of_week=DAYS_OF_WEEK,
        variables=VARIABLES_TO_REGRID,
        regrid_method=REGRID_METHOD,
        sum_dims=SUM_DIMS
    )
