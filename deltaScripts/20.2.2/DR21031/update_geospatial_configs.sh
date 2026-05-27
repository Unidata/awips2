#!/bin/bash
#
# This is a delta script for DR #21031. It updates the four warngen geospatialConfig
# files at the site localization level.

# This delta script should be run on dx3 or dx4 (or dx5 or dx6 at National Centers) as user awips or root
set -e 

county_text='
<!-- UNCOMMENT to enable coastal extension functionality. Note: DO NOT change 
the value of the type property - doing this will cause the generation of the 
text product itself to fail -->
<areaSource variable="extensionAreas">
    <areaSource>MarineZones</areaSource>
    <parentAreaSource>MarineZones</parentAreaSource>
    <type>INTERSECT</type>
    <areaField>NAME</areaField>
    <parentAreaField>NAME</parentAreaField>
    <areaNotationField>NAME</areaNotationField>
    <fipsField>ID</fipsField>
</areaSource>
'
zone_text='
<!-- UNCOMMENT to enable coastal extension functionality. Note: DO NOT change 
the value of the type property - doing this will cause the generation of the 
text product itself to fail. If your WFO is in Alaska, you may need to change 
areaSource and parentAreaSource to "alaska_marine" -->
<areaSource variable="extensionAreas">
    <areaSource>MarineZones</areaSource>
    <parentAreaSource>MarineZones</parentAreaSource>
    <type>INTERSECT</type>
    <areaField>NAME</areaField>
    <parentAreaField>NAME</parentAreaField>
    <areaNotationField>NAME</areaNotationField>
    <fipsField>ID</fipsField>
</areaSource>
'
marine_text='
<!-- UNCOMMENT to enable coastal extension functionality. Note: DO NOT change 
the value of the type property - doing this will cause the generation of the 
text product itself to fail -->
<areaSource variable="extensionAreas">
    <areaSource>County</areaSource>
    <parentAreaSource>States</parentAreaSource>
    <type>INTERSECT</type>
    <areaField>COUNTYNAME</areaField>
    <parentAreaField>NAME</parentAreaField>
    <areaNotationField>STATE</areaNotationField>
    <fipsField>FIPS</fipsField>
</areaSource>
'
alaska_marine_text='
<!-- UNCOMMENT to enable coastal extension functionality. Note: DO NOT change 
the value of the type property - doing this will cause the generation of the 
text product itself to fail -->
<areaSource variable="extensionAreas">
    <areaSource>Zone</areaSource>
    <parentAreaSource>States</parentAreaSource>
    <type>INTERSECT</type>
    <areaField>NAME</areaField>
    <parentAreaField>NAME</parentAreaField>
    <areaNotationField>STATE</areaNotationField>
    <fipsField>STATE_ZONE</fipsField>
</areaSource>
'

echo "Delta script ${0} started"

site_dir="/awips2/edex/data/utility/common_static/site"

for file in "${site_dir}"/*/warngen/geospatialConfig_COUNTY.xml
do
    backup_file="${file}.backup"
    tmp_file="${file}.tmp"
    cp -p "${file}" "${backup_file}"
    cp -p "${file}" "${tmp_file}"
    awk -v text="${county_text}" '/<geospatialConfig>/,/<\/geospatialConfig>/ {if ($0~/<parentAreaSource>/) {next}} 
         /areaSource\s+variable\s*=\s*"areas"/ || /areaSource\s+variable\s*=\s*"affectedZones"/ {print $0; print "    <parentAreaSource>States</parentAreaSource>"; next}
         {if ($0~/^\s*<extensionArea/)
              {print $0; print text; next};
          if ($0~/<!--\s*extensionArea/ || $0~/<!--\s*<extensionArea/)
              {i=text; gsub("\n<areaSource", "\n<!-- areaSource", i); gsub("\n</areaSource>", "\n</areaSource -->", i); print $0; print i; next}
         }
         {print}' "${file}" > "${tmp_file}" && mv -f "${tmp_file}" "${file}"
    echo "Updated ${file}, saved a backup at ${backup_file}"
done

for file in "${site_dir}"/*/warngen/geospatialConfig_ZONE.xml
do
    backup_file="${file}.backup"
    tmp_file="${file}.tmp"
    cp -p "${file}" "${backup_file}"
    cp -p "${file}" "${tmp_file}"
    awk -v text="${zone_text}" '/<geospatialConfig>/,/<\/geospatialConfig>/ {if ($0~/<parentAreaSource>/) {next}} 
         /areaSource\s+variable\s*=\s*"areas"/ || /areaSource\s+variable\s*=\s*"affectedCounties"/ {print $0; print "    <parentAreaSource>States</parentAreaSource>"; next}
         {if ($0~/^\s*<extensionArea/)
              {print $0; print text; next};
          if ($0~/<!--\s*extensionArea/ || $0~/<!--\s*<extensionArea/)
              {i=text; gsub("\n<areaSource", "\n<!-- areaSource", i); gsub("\n</areaSource>", "\n</areaSource -->", i); print $0; print i; next}
         }
         {print}' "${file}" > "${tmp_file}" && mv -f "${tmp_file}" "${file}"
    echo "Updated ${file}, saved a backup at ${backup_file}"
done

for file in "${site_dir}"/*/warngen/geospatialConfig_MARINE.xml
do
    backup_file="${file}.backup"
    tmp_file="${file}.tmp"
    cp -p "${file}" "${backup_file}"
    cp -p "${file}" "${tmp_file}"
    awk -v text="${marine_text}" '/<geospatialConfig>/,/<\/geospatialConfig>/ {if ($0~/<parentAreaSource>/) {next}} 
         /areaSource\s+variable\s*=\s*"areas"/ {print $0; print "    <parentAreaSource>MarineZones</parentAreaSource>"; next}
         {if ($0~/^\s*<extensionArea/)
              {print $0; print text; next};
          if ($0~/<!--\s*extensionArea/ || $0~/<!--\s*<extensionArea/)
              {i=text; gsub("\n<areaSource", "\n<!-- areaSource", i); gsub("\n</areaSource>", "\n</areaSource -->", i); print $0; print i; next}
         }
         {print}' "${file}" > "${tmp_file}" && mv -f "${tmp_file}" "${file}" 
    echo "Updated ${file}, saved a backup at ${backup_file}"
done

for file in "${site_dir}"/*/warngen/geospatialConfig_ALASKA_MARINE.xml
do
    backup_file="${file}.backup"
    tmp_file="${file}.tmp"
    cp -p "${file}" "${backup_file}"
    cp -p "${file}" "${tmp_file}"
    awk -v text="${alaska_marine_text}" '/<geospatialConfig>/,/<\/geospatialConfig>/ {if ($0~/<parentAreaSource>/) {next}} 
         /areaSource\s+variable\s*=\s*"areas"/ {print $0; print "    <parentAreaSource>alaska_marine</parentAreaSource>"; next}
         {if ($0~/^\s*<extensionArea/)
              {print $0; print text; next};
          if ($0~/<!--\s*extensionArea/ || $0~/<!--\s*<extensionArea/)
              {i=text; gsub("\n<areaSource", "\n<!-- areaSource", i); gsub("\n</areaSource>", "\n</areaSource -->", i); print $0; print i; next}
         }
         {print}' "${file}" > "${tmp_file}" && mv -f "${tmp_file}" "${file}"
    echo "Updated ${file}, saved a backup at ${backup_file}"
done


echo "Delta script ${0} completed"
