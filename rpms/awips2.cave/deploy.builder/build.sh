#!/bin/bash
# This script will build the AWIPS II Viz RPMs.

# Build Variables:
# -----------------------------------------------------------------------------
VAR_AWIPSII_TOP_DIR="/home/bkowal/rpmbuild"
VAR_WORKSPACE="/common/bkowal/git/thunder/baseline"
VAR_AWIPSII_BUILD_ROOT="/tmp/awips-component"
VAR_AWIPSII_VERSION=""
VAR_AWIPSII_RELEASE=""
VAR_UFRAME_ECLIPSE="/opt/uframe-eclipse"
VAR_AWIPSCM_SHARE="/awipscm"
VAR_REPO_DEST="/tmp/repo"
# -----------------------------------------------------------------------------

if [ "${AWIPSII_TOP_DIR}" = "" ] &&
   [ "${VAR_AWIPSII_TOP_DIR}" = "" ]; then
   echo "ERROR: You Must Set the AWIPSII_TOP_DIR Environment Variable."
   echo "Unable to Continue ... Terminating."
   exit 1
fi

function prepareBuildEnvironment()
{
   if [ "${AWIPSII_TOP_DIR}" = "" ]; then
      export AWIPSII_TOP_DIR="${VAR_AWIPSII_TOP_DIR}"
   fi

   if [ "${WORKSPACE}" = "" ]; then
      export WORKSPACE="${VAR_WORKSPACE}"
   fi

   if [ "${AWIPSII_BUILD_ROOT}" = "" ]; then
      export AWIPSII_BUILD_ROOT="${VAR_AWIPSII_BUILD_ROOT}"
   fi

   if [ "${AWIPSII_VERSION}" = "" ]; then
      # Determine if we need to use the default version.
      if [ "${VAR_AWIPSII_VERSION}" = "" ]; then
         VAR_AWIPSII_VERSION=`cat ${WORKSPACE}/rpms/version.txt`
      fi
      export AWIPSII_VERSION="${VAR_AWIPSII_VERSION}"
   fi

   if [ "${AWIPSII_RELEASE}" = "" ]; then
      # Determine if we need to use the default release.
      if [ "${VAR_AWIPSII_RELEASE}" = "" ]; then
         VAR_AWIPSII_RELEASE=`date +"%Y%m%d"`
      fi
      export AWIPSII_RELEASE="${VAR_AWIPSII_RELEASE}"
   fi

   if [ "${UFRAME_ECLIPSE}" = "" ]; then
      export UFRAME_ECLIPSE="${VAR_UFRAME_ECLIPSE}"
   fi

   if [ "${AWIPSCM_SHARE}" = "" ]; then
      export AWIPSCM_SHARE="${VAR_AWIPSCM_SHARE}"
   fi

   if [ "${REPO_DEST}" = "" ]; then
      export REPO_DEST="${VAR_REPO_DEST}"
   fi
}

function setTargetArchitecture()
{
   # Set the target build architecture for the rpms based on the CAVE build
   # architecture.
   export TARGET_BUILD_ARCH="${CAVE_BUILD_ARCH}"
   export CAVE_BUILD_BITS="64"
   if [ "${CAVE_BUILD_ARCH}" = "x86" ]; then
      export TARGET_BUILD_ARCH="i386"
      export CAVE_BUILD_BITS=""
   fi
}

export TARGET_BUILD_ARCH=
# If the architecture has not been specified, default to 32-bit.
if [ "${CAVE_BUILD_ARCH}" = "" ]; then
   export CAVE_BUILD_ARCH="x86"
   echo "The Build Architecture was not specified ... defaulting to x86."
else
   echo "Building for architecture ... ${CAVE_BUILD_ARCH}."
fi

# Prepare
prepareBuildEnvironment
setTargetArchitecture

if [ ! -d ${WORKSPACE}/rpms/awips2.cave/setup/dist ]; then
   mkdir -p ${WORKSPACE}/rpms/awips2.cave/setup/dist
   if [ $? -ne 0 ]; then
      exit 0 
   fi
fi

# prepare to complete the RCP builds.
build_project_dir=${WORKSPACE}/build
pde_base_dir=${build_project_dir}/cave
pde_build_dir=${pde_base_dir}/tmp
prepare_dir=${pde_base_dir}/prepare
awips_product=com.raytheon.viz.product.awips/awips.product

if [ ${prepare_dir} ]; then
    rm -rf ${prepare_dir}
fi
mkdir ${prepare_dir}

# First, we need to build the dependency utility.
pushd . > /dev/null 2>&1
cd ${WORKSPACE}/awips.dependency.evaluator
if [ ! -d bin ]; then
    mkdir bin 
fi
/awips2/ant/bin/ant -f build.xml -Dbaseline.dir=${WORKSPACE} \
    -Declipse.dir=${UFRAME_ECLIPSE} -Ddest.dir=${prepare_dir}
if [ $? -ne 0 ]; then
    exit 1
fi

_context_qualifier=`date +"%Y%m%d%H"`

cd ${prepare_dir}
# Next, stage the plugins and determine what needs to be built.
# In another scenario that jar utility could be ran again with only the subset of features
# that need to be built prior to the repository build.
/awips2/java/bin/java -jar -DbaseLocation=${UFRAME_ECLIPSE} \
    -DbuildDirectory=${pde_build_dir} -DstagingDirectory=${WORKSPACE} -DbuildFeatures=* \
    -DexcludeFeatures=com.raytheon.viz.feature.awips.developer,com.raytheon.uf.viz.feature.alertviz \
    AwipsDependencyEvaluator.jar
if [ $? -ne 0 ]; then
    exit 1
fi
	
_pde_launcher_jar=${UFRAME_ECLIPSE}/plugins/org.eclipse.equinox.launcher_1.3.0.v20120522-1813.jar
_pde_product_xml=${UFRAME_ECLIPSE}/plugins/org.eclipse.pde.build_3.8.2.v20121114-140810/scripts/productBuild/productBuild.xml

# Complete the CAVE RCP build.
/awips2/java/bin/java -jar ${_pde_launcher_jar} -application org.eclipse.ant.core.antRunner \
    -buildfile ${_pde_product_xml} -DbaseLocation=${UFRAME_ECLIPSE} \
    -Dbuilder=${pde_base_dir} -DbuildDirectory=${pde_build_dir} \
    -DforceContextQualifier=${_context_qualifier} \
    -Dbase=${pde_base_dir} -Dproduct=${WORKSPACE}/${awips_product}
if [ $? -ne 0 ]; then
    exit 1
fi

# Copy the CAVE binary to the location expected by the RPM build
cp ${pde_build_dir}/I.CAVE/CAVE-linux.gtk.x86_64.zip ${WORKSPACE}/rpms/awips2.cave/setup/dist/

# Prepare for the CAVE repository build. Need to create more resuse for a single build
# properties file so that it can be used for both product and feature builds.
pde_build_dir=${pde_base_dir}/p2
pde_base_dir=${pde_base_dir}/p2
/awips2/java/bin/java -jar -DbaseLocation=${UFRAME_ECLIPSE} \
    -DbuildDirectory=${pde_build_dir} -DstagingDirectory=${WORKSPACE} -DbuildFeatures=* \
    -DexcludeFeatures=com.raytheon.viz.feature.awips.developer,com.raytheon.uf.viz.feature.alertviz \
    AwipsDependencyEvaluator.jar
if [ $? -ne 0 ]; then
    exit 1
fi

_pde_build_xml=${UFRAME_ECLIPSE}/plugins/org.eclipse.pde.build_3.8.2.v20121114-140810/scripts/build.xml
repo_dist_dir=${pde_build_dir}/dist

mkdir -p ${pde_base_dir}
repo_dist_dir=${pde_build_dir}/dist
p2_repo_dir=${pde_build_dir}/repository
if [ -d ${REPO_DEST} ]; then
   rm -rf ${REPO_DEST}
fi
mkdir -p ${REPO_DEST}

cp -v ${build_project_dir}/build.properties.p2 ${pde_base_dir}/build.properties
for feature in `cat ${build_project_dir}/features.txt`; do
/awips2/java/bin/java -jar ${_pde_launcher_jar} -application org.eclipse.ant.core.antRunner \
    -buildfile ${_pde_build_xml} -DbaseLocation=${UFRAME_ECLIPSE} \
    -Dbuilder=${pde_base_dir} -DbuildDirectory=${pde_build_dir} \
    -DtopLevelElementType=feature -DforceContextQualifier=${_context_qualifier} \
    -Dbase=${pde_base_dir} -DtopLevelElementId=${feature} \
    -Dconfigs=linux,gtk,x86_64
    if [ $? -ne 0 ]; then
        exit 1
    fi  

    pushd . > /dev/null 2>&1    
    # zip the built repository.
    cd ${p2_repo_dir}
    zip -r ${REPO_DEST}/${feature}.zip *
    if [ $? -ne 0 ]; then
        exit 1
    fi
    popd > /dev/null 2>&1

    # cleanup the repository contents
    rm -rf ${p2_repo_dir}/*
    if [ $? -ne 0 ]; then
        exit 1
    fi
done

popd > /dev/null 2>&1

# Arguments
#	${1} == The Directory With The Specs File And Possibly Other Custom
#               Scripts That May Need To Be Merged Into A Component.
function buildRPM()
{
   local COMPONENT_DIR=${1}
   local COMPONENT_SPECS=${COMPONENT_DIR}/component.spec

   if [ -d ${BUILDROOT_DIR} ]; then
      rm -rf ${BUILDROOT_DIR}
   fi

   # Build The RPM.
   rpmbuild -ba --target=${TARGET_BUILD_ARCH} \
      --define '_topdir %(echo ${AWIPSII_TOP_DIR})' \
      --define '_component_version %(echo ${AWIPSII_VERSION})' \
      --define '_component_release %(echo ${AWIPSII_RELEASE})' \
      --define '_baseline_workspace %(echo ${WORKSPACE})' \
      --define '_build_arch %(echo ${CAVE_BUILD_ARCH})' \
      --define '_build_bits %(echo ${CAVE_BUILD_BITS})' \
      --buildroot ${AWIPSII_BUILD_ROOT} ${COMPONENT_SPECS}
   # If We Are Unable To Build An RPM, Fail The Build:
   if [ $? -ne 0 ]; then
      exit 1
   fi
}

# Adjust Our Execution Position.
cd ../

# Only Build The RPMs That May Have Changed - AWIPS II-Specific Components.
buildRPM "Installer.cave"
buildRPM "Installer.cave-wrapper"
