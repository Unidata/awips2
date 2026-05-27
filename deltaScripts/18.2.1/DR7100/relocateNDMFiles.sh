#!/bin/bash

# This script relocates the files generated from the NDM file
# modelBufrStationInfo.txt from the SITE level to the CONFIGURED level.
# 


echo "INFO: Running delta script for DR #7100..."

for path in /awips2/edex/data/utility/common_static/site/*/; do 
	if [[ -d "$path" ]]; then
		site_id="${path%/}"
		site_id="${site_id##*/}"

		src_path="/awips2/edex/data/utility/common_static/site/$site_id"
		dest_path="/awips2/edex/data/utility/common_static/configured/$site_id"

		if [[ -f "$src_path/basemaps/modelBufr.spi" ]]; then
			mkdir -p "$dest_path/basemaps/"
			cp -v "$src_path/basemaps/modelBufr.spi" "$dest_path/basemaps/modelBufr.spi"
			rm -vf "$src_path/basemaps/modelBufr.spi" "$src_path/basemaps/modelBufr.spi.md5"
		fi

		if [[ -f "$src_path/modelsounding/modelBufrStationList.txt" ]]; then
			mkdir -p "$dest_path/modelsounding/"
			cp -v "$src_path/modelsounding/modelBufrStationList.txt" "$dest_path/modelsounding/modelBufrStationList.txt"
			rm -vf "$src_path/modelsounding/modelBufrStationList.txt" "$src_path/modelsounding/modelBufrStationList.txt.md5"
		fi
	fi
done


echo "INFO: Delta script complete."

