function installCAVERepository([string]$feature_group, [string]$repo_zip)
{
    .\cave.exe -nosplash -application org.eclipse.equinox.p2.director `
        -i $feature_group -repository "jar:file:///${A2_PREPARE_CAVE_DIR}\$repo_zip!" | Out-Host
    if ($? -ne $true) { echo "ERROR: Unable to install feature: $feature_group."; echo "FATAL: Build Failed!"; EXIT 1; } 
    
    echo "`n"
}

pushd .
cd ${A2_PREPARE_CAVE_DIR}\cave

echo "`n"
installCAVERepository -feature_group "com.raytheon.uf.viz.cots.feature.feature.group" `
    -repo_zip "com.raytheon.uf.viz.cots.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.uf.viz.common.core.feature.feature.group" `
    -repo_zip "com.raytheon.uf.viz.common.core.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.uf.viz.core.feature.feature.group" `
    -repo_zip "com.raytheon.uf.viz.core.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.uf.viz.d2d.core.feature.feature.group" `
    -repo_zip "com.raytheon.uf.viz.d2d.core.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.viz.radar.feature.feature.group" `
    -repo_zip "com.raytheon.viz.radar.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.viz.grib.feature.feature.group" `
    -repo_zip "com.raytheon.viz.grib.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.uf.viz.sounding.feature.feature.group" `
    -repo_zip "com.raytheon.uf.viz.sounding.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.uf.viz.dataplugins.feature.feature.group" `
    -repo_zip "com.raytheon.uf.viz.dataplugins.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.uf.viz.dataplugin.obs.feature.feature.group" `
    -repo_zip "com.raytheon.uf.viz.dataplugin.obs.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.uf.viz.displays.feature.feature.group" `
    -repo_zip "com.raytheon.uf.viz.displays.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.viz.text.feature.feature.group" `
    -repo_zip "com.raytheon.viz.text.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.viz.satellite.feature.feature.group" `
    -repo_zip "com.raytheon.viz.satellite.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.uf.viz.ncep.core.feature.feature.group" `
    -repo_zip "com.raytheon.uf.viz.ncep.core.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.uf.viz.d2d.xy.feature.feature.group" `
    -repo_zip "com.raytheon.uf.viz.d2d.xy.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.uf.viz.core.maps.feature.feature.group" `
    -repo_zip "com.raytheon.uf.viz.core.maps.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.viz.gfe.feature.feature.group" `
    -repo_zip "com.raytheon.viz.gfe.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.viz.avnfps.feature.feature.group" `
    -repo_zip "com.raytheon.viz.avnfps.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.viz.hydro.feature.feature.group" `
    -repo_zip "com.raytheon.viz.hydro.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.uf.viz.d2d.skewt.feature.feature.group" `
    -repo_zip "com.raytheon.uf.viz.d2d.skewt.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.uf.viz.ncep.dataplugins.feature.feature.group" `
    -repo_zip "com.raytheon.uf.viz.ncep.dataplugins.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.uf.viz.ncep.displays.feature.feature.group" `
    -repo_zip "com.raytheon.uf.viz.ncep.displays.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.uf.viz.ncep.nsharp.feature.feature.group" `
    -repo_zip "com.raytheon.uf.viz.ncep.nsharp.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.uf.viz.d2d.nsharp.feature.feature.group" `
    -repo_zip "com.raytheon.uf.viz.d2d.nsharp.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.viz.volumebrowser.feature.feature.group" `
    -repo_zip "com.raytheon.viz.volumebrowser.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.uf.viz.d2d.gfe.feature.feature.group" `
    -repo_zip "com.raytheon.uf.viz.d2d.gfe.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.viz.warngen.feature.feature.group" `
    -repo_zip "com.raytheon.viz.warngen.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.uf.viz.ncep.perspective.feature.feature.group" `
    -repo_zip "com.raytheon.uf.viz.ncep.perspective.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.uf.viz.nwsauth.feature.feature.group" `
    -repo_zip "com.raytheon.uf.viz.nwsauth.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.uf.viz.thinclient.feature.feature.group" `
    -repo_zip "com.raytheon.uf.viz.thinclient.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.uf.viz.dat.feature.feature.group" `
    -repo_zip "com.raytheon.uf.viz.dat.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.uf.viz.localization.perspective.feature.feature.group" `
    -repo_zip "com.raytheon.uf.viz.localization.perspective.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.uf.viz.alertviz.localization.feature.feature.group" `
    -repo_zip "com.raytheon.uf.viz.alertviz.localization.feature-repo-win32.x86.zip"
installCAVERepository -feature_group "com.raytheon.uf.viz.npp.feature.feature.group" `
    -repo_zip "com.raytheon.uf.viz.npp.feature-repo-win32.x86.zip"

popd
EXIT 0