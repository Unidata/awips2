This directory contains all the scripts needed to upgrade from grib to the new
"Unified" grid storage format.

The following commands will need to run to upgrade postgres and hdf5 files:
bash register_grid_coverage.sh
bash create_grid_tables.sh
python convert_grib_data.py

The following commands will need to run to upgrade localization files:
bash copy_grib_purge_rules.sh
bash update_D2D_bundles.sh
bash update_D2D_procedures.sh
bash update_FFMP_Source.sh

convert_grib_data.py can take a significant amount of time, as much as 2 hours.
If you don't need your data and you want to upgrade faster you can purge all grib data.
If there is no grib data to convert there is no need to run convert_grib_data.py.

After convert_grib_data.py has run, it may not convert all models(perhaps skipping
alaskan, hawaiin, or other models.) If this is the case there will be data left over
in /awips2/edex/data/hdf5/grib. This data can be used if you need to convert additional
models or if a rollback is necessary. The system is not set up to purge this data, so
after a successful upgrade when new data is arriving this directory will need to be deleted.

The format of the data in /awips2/edex/data/hdf5/topo/modelStaticTopo.h5 has changed. When the
ingestGrib edex is started it will attempt to regenerate this file. This is a very time and
memory intensive process. To save time when upgrading an operational system it is recommended
that you copy a modelStaticTopo.h5 file from a testbed or other system that has already generated it.

The update_saved_display.sh script can be used if there are any saved displays that are
saved outside of localization.

If for some reason the upgrade fails or you want to roll back. Install an old 
version of edex and run the scripts in the unified_grid_rollback directory.
The postgres and hdf5 scripts in that directory will need to be run in the opposite order:
python convert_grib_data.py
bash create_grid_tables.sh
bash register_grid_coverage.sh