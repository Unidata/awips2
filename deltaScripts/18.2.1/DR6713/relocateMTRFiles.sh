#!/bin/bash

# This script relocates the NDM files MTR.goodness, MTR.primary, MTR.spi
# from the SITE level to the CONFIGURED level.
# 


echo "INFO: Running delta script for DR #6713..."

for path in /awips2/edex/data/utility/common_static/site/*/; do 
	if [[ -d "$path" ]]; then
		site_id="${path%/}"
		site_id="${site_id##*/}"

		src_path="/awips2/edex/data/utility/common_static/site/$site_id"
		dest_path="/awips2/edex/data/utility/common_static/configured/$site_id"
		mkdir -p "$dest_path/basemaps/"

		if [[ -f "$src_path/basemaps/MTR.spi" ]]; then
			cp -v "$src_path/basemaps/MTR.spi" "$dest_path/basemaps/MTR.spi"
			rm -vf "$src_path/basemaps/MTR.spi" "$src_path/basemaps/MTR.spi.md5"
		fi
		
		if [[ -f "$src_path/basemaps/MTR.goodness" ]]; then
			cp -v "$src_path/basemaps/MTR.goodness" "$dest_path/basemaps/MTR.goodness"
			rm -vf "$src_path/basemaps/MTR.goodness" "$src_path/basemaps/MTR.goodness.md5"
		fi
		
		if [[ -f "$src_path/basemaps/MTR.primary" ]]; then
			cp -v "$src_path/basemaps/MTR.primary" "$dest_path/basemaps/MTR.primary"
			rm -vf "$src_path/basemaps/MTR.primary" "$src_path/basemaps/MTR.primary.md5"
		fi
	fi
done


echo "INFO: Delta script complete."

