function installCAVERepository([string]$feature_group, [string]$repo)
{
    $repo_zip = $repo + ".zip"
    New-Item -path ${A2_PREPARE_CAVE_DIR} `
        -name $repo -type directory | Out-Null
    if ($? -ne $true) { EXIT 1; }    
    pushd .
    cd ${A2_PREPARE_CAVE_DIR}/$repo
    jar xvf ${A2_PREPARE_CAVE_DIR}/$repo_zip
    popd

    .\cave.exe -nosplash -application org.eclipse.equinox.p2.director `
        -i $feature_group -repository "file:${A2_PREPARE_CAVE_DIR}/$repo" | Out-Host
    if ($? -ne $true) { echo "ERROR: Unable to install feature: $feature_group."; echo "FATAL: Build Failed!"; EXIT 1; }
    
    Remove-Item -recurse -force ${A2_PREPARE_CAVE_DIR}/$repo
    if ($? -ne $true) { EXIT 1; }
    
    pushd .
    cd plugins
    dir -rec | where { if (($_.fullname.split("\")).count -eq 7 -and `
        $_.name.equals("localization")){$_}} | ForEach-Object -process `
        { $localizationDirectory = $_.fullname; `
        echo $localizationDirectory; `
        Copy-Item -force -recurse -path "${localizationDirectory}\*" -destination ${A2_PREPARE_CAVE_DIR}\cave\etc; `
        if ($? -ne $true) { EXIT 1; }; `
        Remove-Item -force -recurse ${localizationDirectory}; `
        if ($? -ne $true) { EXIT 1; } }
    if ($? -ne $true) { EXIT 1; }
    popd 
    
    echo "`n"
}

pushd .

cd ${A2_PREPARE_CAVE_DIR}\cave

New-Item -path ${A2_PREPARE_CAVE_DIR}\cave `
    -name etc -type directory | Out-Null
if ($? -ne $true) { EXIT 1; }

$repo_suffix = "-repo-win32.x86"
if ("${AWIPS2_BUILD_ARCHITECTURE}" -eq "x64")
{
    $repo_suffix = "-repo-win32.x86_64"
}

echo "`n"
$feature_list = Get-Content "${A2_PREPARE_CAVE_DIR}\features.txt"
Write-Host Feature Count = $feature_list.count
foreach ($feature in $feature_list)
{
    Write-Host Installing Feature: $feature
    $feature_group = $feature + ".feature.group"
    $repo = $feature + $repo_suffix
    
    installCAVERepository -feature_group "$feature_group" `
        -repo "$repo" 
}
popd
EXIT 0