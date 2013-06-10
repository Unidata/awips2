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
    
    echo "`n"
}

pushd .

cd ${A2_PREPARE_CAVE_DIR}\cave

echo "`n"
$feature_list = Get-Content "${A2_PREPARE_CAVE_DIR}\features.txt"
Write-Host Feature Count = $feature_list.count
foreach ($feature in $feature_list)
{
    Write-Host Installing Feature: $feature
    $feature_group = $feature + ".feature.group"
    $repo = $feature + "-repo-win32.x86"
    
    installCAVERepository -feature_group "$feature_group" `
        -repo "$repo" 
}
popd
EXIT 0