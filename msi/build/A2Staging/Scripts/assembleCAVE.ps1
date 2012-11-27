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
$feature_list = Get-Content "${A2_PREPARE_CAVE_DIR}\features.txt"
Write-Host Feature Count = $feature_list.count
foreach ($feature in $feature_list)
{
    Write-Host Installing Feature: $feature
    $feature_group = $feature + ".feature.group"
    $repo_zip = $feature + "-repo-win32.x86.zip"
    
    installCAVERepository -feature_group "$feature_group" `
        -repo_zip "$repo_zip" 
}
popd
EXIT 0