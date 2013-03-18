#!/bin/sh 
#
# name: updateGFELoc.sh
# desc: Move localization files from cave_static to common_static for GFE ifpServerText
#
# CHANGELOG
# 06-March-2010 - jzeng  Initial creation

#

usage(){
  echo "    Default: "
  echo "        updateGfeLoc.sh "
  echo "        update all site and user level GFE;"
  echo "    Use updateGfeLoc.sh -s|--site to update site level GFE; "
  echo "    Use updateGfeLoc.sh -u|--user to update user level GFE;"
  echo "    Use updateGfeLoc.sh -c|--configured to update configured level GFE;"
  echo "    Use updateGfeLoc.sh -w|--workstation to update workstation level GFE;"
  echo "    Use updateGfeLoc.sh -a|--all to update all levels of GFE localization files;"
  exit
}

updateGfeLoc(){
    LOC_LEVEL=$@
    LOCALIZATION_DICT=('gfe/userPython/gfeConfig 
			gfe/userPython/smartTools 
			gfe/userPython/procedures 
			gfe/userPython/textProducts 
			gfe/userPython/textUtilities
			gfe/userPython/utilities 
			gfe/weGroups 
			gfe/combinations 
			colormaps 
			')

    for loc in $LOCALIZATION_DICT
    do
        commonDir=/awips2/edex/data/utility/common_static/${LOC_LEVEL}/${loc}
        caveDir=/awips2/edex/data/utility/cave_static/${LOC_LEVEL}/${loc}
        if [ ! -d $commonDir ]
        then
	    mkdir -p $commonDir
        fi
        if [ -d ${caveDir} ]
        then
	    cp -r $caveDir/* $commonDir
            rm -rf ${caveDir}
        fi
    done  
}    

updateAll(){
  dirPaths=`ls /awips2/edex/data/utility/cave_static`
  for dirPath in $dirPaths
  do
    echo "Updating $dirPath"
    levelPaths=`ls /awips2/edex/data/utility/cave_static/${dirPath}`
    for level in $levelPaths
    do
        updateGfeLoc ${dirPath}/${levelPath}
    done
  done
}

updateSite(){
    sitePaths=`ls /awips2/edex/data/utility/cave_static/site`
    for site in $sitePaths
    do
        updateGfeLoc site/$site
    done
}

updateUser(){
    userPaths=`ls /awips2/edex/data/utility/cave_static/user`
    for user in $userPaths
    do
        updateGfeLoc user/$user
    done
}

updateConfigured(){
    confPaths=`ls /awips2/edex/data/utility/cave_static/configured`
    for conf in $confPaths
    do
        updateGfeLoc configured/$conf
    done
}

updateWorkstation(){
    wtPaths=`ls /awips2/edex/data/utility/cave_static/workstation`
    for wt in $wtPaths
    do
        updateGfeLoc workstation/$wt
    done
}

if [ $# == 0 ] 
then
    echo "This script will relocate all site and user level GFE files from cave_static to common_static: "
    updateAll
else 
    for arg in $@
    do
        case $arg in
            -s|--site)
                updateSite
                ;;
            -u|--user)
                updateUser
                ;;
            -c|--configured)
                updateConfigured
                ;;
            -w|--workstation)
                updateWorkstation
                ;;
            -a|--all)
                updateAll
                ;;
            -h|--help)
                echo "Usage:"
	        usage;;
            *)  echo "$arg is an invalid application!"
                usage;;
        esac
   done
fi

