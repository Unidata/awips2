#!/bin/bash

# This script relocates the LDAD files ldad15*.spi, ldad15*.goodness, ldad15*.primary
# from the cave_static SITE level to the common_static site level.
# 


echo "INFO: Running delta script for DCS 20569..."

for path in /awips2/edex/data/utility/cave_static/site/*/; do 
	if [[ -d "$path" ]]; then
		site_id="${path%/}"
		site_id="${site_id##*/}"

		src_path="/awips2/edex/data/utility/cave_static/site/$site_id"
		dest_path="/awips2/edex/data/utility/common_static/configured/$site_id"
		mkdir -p "$dest_path/basemaps/"

		if [[ -f "$src_path/basemaps/ldad15.spi" ]]; then
			cp -pv "$src_path/basemaps/ldad15.spi" "$dest_path/basemaps/ldad15.spi"
			rm -vf "$src_path/basemaps/ldad15.spi" "$src_path/basemaps/ldad15.spi.md5"
		fi
		
		if [[ -f "$src_path/basemaps/ldad15.goodness" ]]; then
			cp -pv "$src_path/basemaps/ldad15.goodness" "$dest_path/basemaps/ldad15.goodness"
			rm -vf "$src_path/basemaps/ldad15.goodness" "$src_path/basemaps/ldad15.goodness.md5"
		fi
		
		if [[ -f "$src_path/basemaps/ldad15.primary" ]]; then
			cp -pv "$src_path/basemaps/ldad15.primary" "$dest_path/basemaps/ldad15.primary"
			rm -vf "$src_path/basemaps/ldad15.primary" "$src_path/basemaps/ldad15.primary.md5"
		fi

		if [[ -f "$src_path/basemaps/ldad15prcp.spi" ]]; then
			cp -pv "$src_path/basemaps/ldad15prcp.spi" "$dest_path/basemaps/ldad15prcp.spi"
			rm -vf "$src_path/basemaps/ldad15prcp.spi" "$src_path/basemaps/ldad15prcp.spi.md5"
		fi
		
		if [[ -f "$src_path/basemaps/ldad15prcp.goodness" ]]; then
			cp -pv "$src_path/basemaps/ldad15prcp.goodness" "$dest_path/basemaps/ldad15prcp.goodness"
			rm -vf "$src_path/basemaps/ldad15prcp.goodness" "$src_path/basemaps/ldad15prcp.goodness.md5"
		fi
		
		if [[ -f "$src_path/basemaps/ldad15prcp.primary" ]]; then
			cp -pv "$src_path/basemaps/ldad15prcp.primary" "$dest_path/basemaps/ldad15prcp.primary"
			rm -vf "$src_path/basemaps/ldad15prcp.primary" "$src_path/basemaps/ldad15prcp.primary.md5"
		fi
	fi
done


echo "INFO: Delta script complete."

