#!/bin/bash
#----------------------------------------------------------------------
# AWIPS II auto installer script.
# Copies the installer files from the CM archive to the target server.
#
#----------------------------------------------------------------------
# args:
#   $1 :: remote directory (/awipscm/installers        required
#   $2 :: local directory  (/installer)                required
#   $3 :: bundle to copy   (awips-server)              required
#----------------------------------------------------------------------
# The script assumes a source directory structure similar to
#    <<remote>>
#          |-- <<bundle>>-components.txt
#          `-- install_files
#              |-- <<bundle>>-installer.jar
#              `-- components
#                  |-- ade-<<component-1>>-installer.jar
#                  `-- ade-<<component-n>>-installer.jar
#
# The required files are copied to a matching structure under <<local>>
#
#----------------------------------------------------------------------
# Limitations:
#    1) this script should be run as root.
#----------------------------------------------------------------------
remote_dir=${1}
local_dir=${2}
bundle_pre=${3}

if [ $# -lt 3 ];then
   filename=`basename $0`
   echo ""
   echo "usage:"
   echo "  ${filename} remote-directory local-directory deploy-token"
   echo ""
   echo "example:"
   echo "  ${filename} /awipscm/installers /installers awips-server"
   echo ""
   exit 0
fi

# subdirectory containing bundle files
bundle_dir=install_files
# subdirectory containing component files
component_dir=${bundle_dir}/components
# name of the bundle installer file
bundle_installer="${bundle_pre}-installer.jar"
# name of the bundle list file
bundle_list="${bundle_pre}-components.txt"
# full paths to source files 
src_file="${remote_dir}/${bundle_dir}/${bundle_installer}"
src_list="${remote_dir}/${bundle_dir}/${bundle_list}"

# make sure the bundle installer exists
if [ ! -e ${src_file} ]; then
   echo "${bundle_installer} not found -- unable to continue, exiting..."
   exit 1
elif [ ! -e ${src_list} ]; then
   echo "${bundle_list} not found -- unable to continue, exiting..."
   exit 2
fi

echo "**************************************************************"
echo "copying installer files to ${local_dir}"
echo "**************************************************************"
# copy the bundler files to the local directory
echo ""
echo "creating destination directory ${local_dir}/${deployer_dir}"
echo ""
mkdir -p ${local_dir}/${bundle_dir}
echo "copying bundle installer ${bundle_installer}..."
echo "  copying ${bundle_installer} to ${local_dir}/${deployer_dir}"
cp ${src_file} ${local_dir}/${bundle_dir}/${bundle_installer}

echo ""
echo "creating destination directory ${local_dir}/${component_dir}"
echo ""
mkdir -p ${local_dir}/${component_dir}

echo "copying component installers..."
. ${src_list}
echo "  component list: ${components}"

for a in ${components};do
   filename="ade-${a}-installer.jar"
   echo "    copying ${filename}"
   src_path="${remote_dir}/${component_dir}/${filename}"
   dst_path="${local_dir}/${component_dir}/${filename}"
   cp ${src_path} ${dst_path}
done
echo ""
echo "**************************************************************"
echo "installer file copy complete"
echo "**************************************************************"
