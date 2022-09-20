# kwb.dwd 0.2.0 (2022-09-20)

* add contributors Michael Rustler, Lukas Guericke
* use GitHub actions instead of Travis and Appveyor
* add tests
* update dataset [dwd_files](../reference/dwd_files.html)
* modify functions
    - `get_radolan_url()`: add arg "ftp_root"
    - `get_radolan_urls()`: add args "..."
    - `list_url()`: 
        * remove args "depth", "curl", add arg "full_names"
        * reimplement using `kwb.utils::listToDepth()`
* add new functions
    - `calculate_masked_grid_stats()`
    - `check_or_download_shapes_germany()`
    - `get_berlin_dwd_mask()`
    - `get_example_grid_germany()`
    - `get_shapes_of_germany()`
    - `load_monthly_variable_for_region()`
    - `load_potential_evaporation_berlin()`
    - `load_potential_evaporation_berlin_2()`
    - `load_precipitation_berlin()`
    - `open_description()`
    - `read_asc_gz_file()`
    - `read_daily_data_over_shape()`
    - `read_monthly_data_over_shape()`
    - `unzip_asc_gz_file()`
* export functions that were private before 
    - `coordinates_to_EPSG_4326()`
    - `extract_radolan_zip_files()`
    - `get_radolan_extension`()`
    - `get_radolan_projection_string()`
    - `radolan_raw_to_raster()`
    - `write_raster_to_file()`
    
# kwb.dwd 0.1.1 (2019-12-17)

* use `kwb.utils::assertFinalSlash()`

# kwb.dwd 0.0.0.9000

* add a `NEWS.md` file to track changes to the package.
* see https://style.tidyverse.org/news.html for writing a good `NEWS.md`
