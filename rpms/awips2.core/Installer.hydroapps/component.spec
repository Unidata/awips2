%define _component_name           awips2-hydroapps
%define _component_project_dir    awips2.core/Installer.hydroapps
#
# AWIPS II Hydroapps Spec File
#
Name: %{_component_name}
Summary: AWIPS II Hydroapps Distribution
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: /tmp
URL: N/A
License: N/A
Distribution: N/A
Vendor: %{_build_vendor}
Packager: %{_build_site}

AutoReq: no
Provides: awips2-hydroapps
Requires: awips2-edex
Requires: awips2-edex-base

%description
AWIPS II Hydroapps Distribution - Includes applications, configurations, and
filesystems for Hydro.

# disable jar repacking
%global __os_install_post %(echo '%{__os_install_post}' | sed -e 's!/usr/lib[^[:space:]]*/brp-java-repack-jars[[:space:]].*$!!g')

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi

%build

%install
mkdir -p ${RPM_BUILD_ROOT}/awips2/edex/data
if [ $? -ne 0 ]; then
   exit 1
fi

FILES_NATIVE="%{_baseline_workspace}/files.native"

/bin/cp -rf ${FILES_NATIVE}/awipsShare \
   %{_build_root}/awips2/edex/data/share
if [ $? -ne 0 ]; then
   exit 1
fi

/usr/bin/find %{_build_root}/awips2/edex/data/share -name .gitignore -exec rm -rf {} \;
if [ $? -ne 0 ]; then
   exit 1
fi

%pre
%post

%preun
%postun

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(770,awips,fxalpha,770)
%dir /awips2/edex/data/share/hydroapps
%dir /awips2/edex/data/share/hydroapps/bin
/awips2/edex/data/share/hydroapps/bin/*
/awips2/edex/data/share/hydroapps/check_app_context
%dir /awips2/edex/data/share/hydroapps/geo_data
%config(noreplace) /awips2/edex/data/share/hydroapps/geo_data/host/ascii/coord_host.dat
%config(noreplace) /awips2/edex/data/share/hydroapps/geo_data/host/ascii/county.dat
%config(noreplace) /awips2/edex/data/share/hydroapps/geo_data/host/ascii/cwaus.dat
%config(noreplace) /awips2/edex/data/share/hydroapps/geo_data/host/ascii/fg_basin.dat
%config(noreplace) /awips2/edex/data/share/hydroapps/geo_data/host/ascii/flights.dat
%config(noreplace) /awips2/edex/data/share/hydroapps/geo_data/host/ascii/forecastpt.dat
%config(noreplace) /awips2/edex/data/share/hydroapps/geo_data/host/ascii/map_basin.dat
%config(noreplace) /awips2/edex/data/share/hydroapps/geo_data/host/ascii/rfc_boundary.dat
%config(noreplace) /awips2/edex/data/share/hydroapps/geo_data/host/ascii/river.dat
%config(noreplace) /awips2/edex/data/share/hydroapps/geo_data/host/ascii/state.dat
%config(noreplace) /awips2/edex/data/share/hydroapps/geo_data/host/ascii/town.dat
%config(noreplace) /awips2/edex/data/share/hydroapps/geo_data/ofstest/ascii/coord_ofstest.dat
%config(noreplace) /awips2/edex/data/share/hydroapps/geo_data/util/run_create_bas_bound
%dir /awips2/edex/data/share/hydroapps/lib
/awips2/edex/data/share/hydroapps/lib/*
%dir /awips2/edex/data/share/hydroapps/precip_proc
%dir /awips2/edex/data/share/hydroapps/precip_proc/bin
/awips2/edex/data/share/hydroapps/precip_proc/bin/*
%dir /awips2/edex/data/share/hydroapps/precip_proc/local
%dir /awips2/edex/data/share/hydroapps/precip_proc/local/bin
/awips2/edex/data/share/hydroapps/precip_proc/local/bin/*
%dir /awips2/edex/data/share/hydroapps/precip_proc/local/data
%dir /awips2/edex/data/share/hydroapps/precip_proc/local/data/app
%dir /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/hpe
/awips2/edex/data/share/hydroapps/precip_proc/local/data/app/hpe/*
%dir /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/nc2grib
/awips2/edex/data/share/hydroapps/precip_proc/local/data/app/nc2grib/*
%dir /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe
%dir /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/nc2grib
/awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/nc2grib/*
%dir /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_max_temp_oax_ann
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_max_temp_oax_apr
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_max_temp_oax_aug
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_max_temp_oax_dec
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_max_temp_oax_feb
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_max_temp_oax_jan
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_max_temp_oax_jul
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_max_temp_oax_jun
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_max_temp_oax_mar
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_max_temp_oax_may
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_max_temp_oax_nov
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_max_temp_oax_oct
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_max_temp_oax_sep
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_mean_precip_oax_ann
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_mean_precip_oax_apr
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_mean_precip_oax_aug
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_mean_precip_oax_dec
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_mean_precip_oax_feb
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_mean_precip_oax_jan
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_mean_precip_oax_jul
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_mean_precip_oax_jun
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_mean_precip_oax_mar
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_mean_precip_oax_may
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_mean_precip_oax_nov
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_mean_precip_oax_oct
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_mean_precip_oax_sep
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_min_temp_oax_ann
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_min_temp_oax_apr
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_min_temp_oax_aug
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_min_temp_oax_dec
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_min_temp_oax_feb
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_min_temp_oax_jan
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_min_temp_oax_jul
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_min_temp_oax_jun
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_min_temp_oax_mar
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_min_temp_oax_may
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_min_temp_oax_nov
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_min_temp_oax_oct
%config(noreplace) /awips2/edex/data/share/hydroapps/precip_proc/local/data/app/mpe/prism/prism_min_temp_oax_sep
%dir /awips2/edex/data/share/hydroapps/public
/awips2/edex/data/share/hydroapps/public/*
%dir /awips2/edex/data/share/hydroapps/rfc
/awips2/edex/data/share/hydroapps/rfc/*
/awips2/edex/data/share/hydroapps/set_hydro_env
%dir /awips2/edex/data/share/hydroapps/whfs
%dir /awips2/edex/data/share/hydroapps/whfs/bin
/awips2/edex/data/share/hydroapps/whfs/bin/*
%dir /awips2/edex/data/share/hydroapps/whfs/local
%dir /awips2/edex/data/share/hydroapps/whfs/local/bin
/awips2/edex/data/share/hydroapps/whfs/local/bin/*
%dir /awips2/edex/data/share/hydroapps/whfs/local/data
%dir /awips2/edex/data/share/hydroapps/whfs/local/data/app
/awips2/edex/data/share/hydroapps/whfs/local/data/app/*
%dir /awips2/edex/data/share/hydroapps/whfs/local/data/geo
%config(noreplace) /awips2/edex/data/share/hydroapps/whfs/local/data/geo/basins.dat
%config(noreplace) /awips2/edex/data/share/hydroapps/whfs/local/data/geo/basins.dat.OAX
%config(noreplace) /awips2/edex/data/share/hydroapps/whfs/local/data/geo/topography
