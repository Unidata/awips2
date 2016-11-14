#!/bin/bash
# This script will build the AWIPS II Viz RPMs.

# Build Variables:
# -----------------------------------------------------------------------------
VAR_AWIPSII_BUILD_ROOT="/tmp/awips-component"
VAR_AWIPSII_VERSION=""
VAR_AWIPSII_RELEASE=""
VAR_UFRAME_ECLIPSE="/awips2/eclipse"
VAR_REPO_DEST="/tmp/repo"
# -----------------------------------------------------------------------------

if [ "${AWIPSII_TOP_DIR}" = "" ]; then
   echo "ERROR: You Must Set the AWIPSII_TOP_DIR Environment Variable."
   echo "Unable to Continue ... Terminating."
   exit 1
fi

if [ "${WORKSPACE}" = "" ]; then
   echo "ERROR: You Must Set the WORKSPACE Environment Variable."
   echo "Unable to Continue ... Terminating."
   exit 1
fi

function prepareBuildEnvironment()
{
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

   if [ "${REPO_DEST}" = "" ]; then
      export REPO_DEST="${VAR_REPO_DEST}"
   fi
}

# Build CAVE as 64 bit only.
architecture=`uname -i`
if [ ! "${architecture}" = "x86_64" ]; then
   echo "ERROR: This build can only be performed on a 64-bit Operating System."
   echo "Unable to Continue ... Terminating."
   exit 1
fi

source ${WORKSPACE}/rpms/build/x86_64/rpms.sh
if [ $? -ne 0 ]; then
   echo "ERROR: Unable to source the RPM functions."
   exit 1
fi

# Prepare
prepareBuildEnvironment

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
_pde_launcher_jar=${UFRAME_ECLIPSE}/plugins/org.eclipse.equinox.launcher_1.3.0.v20120522-1813.jar
_pde_product_xml=${UFRAME_ECLIPSE}/plugins/org.eclipse.pde.build_3.8.2.v20121114-140810/scripts/productBuild/productBuild.xml

cd ${prepare_dir}
# Prepare for the CAVE repository build. Need to create more resuse for a single build
# properties file so that it can be used for both product and feature builds.
pde2_build_dir=${pde_base_dir}/p2
pde2_base_dir=${pde_base_dir}/p2
/awips2/java/bin/java -jar -DbaseLocation=${UFRAME_ECLIPSE} \
    -DbuildDirectory=${pde2_build_dir} -DstagingDirectory=${WORKSPACE} -DbuildFeatures=* \
    -DexcludeFeatures=com.raytheon.viz.feature.awips.developer,com.raytheon.uf.viz.feature.alertviz \
    AwipsDependencyEvaluator.jar
if [ $? -ne 0 ]; then
    exit 1
fi

_pde_build_xml=${UFRAME_ECLIPSE}/plugins/org.eclipse.pde.build_3.8.2.v20121114-140810/scripts/build.xml
repo_dist_dir=${pde2_build_dir}/dist

mkdir -p ${pde2_base_dir}
repo_dist_dir=${pde2_build_dir}/dist
p2_repo_dir=${pde2_build_dir}/repository
if [ -d ${REPO_DEST} ]; then
   rm -rf ${REPO_DEST}
fi
mkdir -p ${REPO_DEST}

cp -v ${build_project_dir}/build.properties.p2 ${pde2_base_dir}/build.properties
for feature in `cat ${build_project_dir}/features.txt`; do
/awips2/java/bin/java -jar ${_pde_launcher_jar} -application org.eclipse.ant.core.antRunner \
    -buildfile ${_pde_build_xml} -DbaseLocation=${UFRAME_ECLIPSE} \
    -Dbuilder=${pde2_base_dir} -DbuildDirectory=${pde2_build_dir} \
    -DtopLevelElementType=feature -DforceContextQualifier=${_context_qualifier} \
    -Dbase=${pde2_base_dir} -DtopLevelElementId=${feature} \
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

cd ${prepare_dir}
# Next, stage the plugins and determine what needs to be built.
/awips2/java/bin/java -jar -DbaseLocation=${UFRAME_ECLIPSE} \
    -DbuildDirectory=${pde_build_dir} -DstagingDirectory=${WORKSPACE} -DbuildFeatures=* \
    -DexcludeFeatures=com.raytheon.viz.feature.awips.developer,com.raytheon.uf.viz.feature.alertviz \
    AwipsDependencyEvaluator.jar
if [ $? -ne 0 ]; then
    exit 1
fi

cd ${prepare_dir}
# Next, stage the plugins and determine what needs to be built.
# In another scenario that jar utility could be ran again with only the subset of features
# that need to be built prior to the repository build.
/awips2/java/bin/java -jar -DbaseLocation=${UFRAME_ECLIPSE} \
    -DbuildDirectory=${pde_build_dir} -DstagingDirectory=${WORKSPACE} \
    -DbuildProduct=${awips_product} -DbaseUpgrade=com.raytheon.viz.feature.awips \
    -DoutputFile=${prepare_dir}/awipsInstall.txt \
    -DexcludeFeatures=com.raytheon.viz.feature.awips.developer,com.raytheon.uf.viz.feature.alertviz \
    AwipsDependencyEvaluator.jar
if [ $? -ne 0 ]; then
    exit 1
fi

# Complete the CAVE RCP build.
/awips2/java/bin/java -jar ${_pde_launcher_jar} -application org.eclipse.ant.core.antRunner \
    -buildfile ${_pde_product_xml} -DbaseLocation=${UFRAME_ECLIPSE} \
    -Dbuilder=${pde_base_dir} -DbuildDirectory=${pde_build_dir} \
    -DforceContextQualifier=${_context_qualifier} \
    -Dbase=${pde_base_dir} -Dproduct=${WORKSPACE}/${awips_product}
if [ $? -ne 0 ]; then
    exit 1
fi

_repo_install_staging=${prepare_dir}/repo-staging

unzip ${pde_build_dir}/I.CAVE/CAVE-linux.gtk.x86_64.zip -d ${pde_build_dir}/I.CAVE 

for feature in `cat ${prepare_dir}/awipsInstall.txt`; do
    if [ -d ${_repo_install_staging} ]; then
        rm -rf ${_repo_install_staging}
    fi
    mkdir ${_repo_install_staging}
    
    unzip ${REPO_DEST}/${feature}.zip -d ${_repo_install_staging}
    
    CAVE_EXE="${pde_build_dir}/I.CAVE/cave/cave"
    NOSPLASH_ARG="-nosplash"
    DIRECTOR_APP="-application org.eclipse.equinox.p2.director"
    #DESTINATION_ARG="-destination ${pde_build_dir}/I.CAVE/cave"
    INSTALL_ARG="-i ${feature}.feature.group"
    UNINSTALL_ARG="-u ${feature}.feature.group"
    # Used to ensure that the awips2-java is used.
    VM_ARG=/awips2/java/bin/java
    REPO="-repository file:${_repo_install_staging}"
    
    COMMON_CMD="${CAVE_EXE} -vm ${VM_ARG} ${NOSPLASH_ARG} ${DIRECTOR_APP} ${DESTINATION_ARG}"
    INSTALL_CMD="${COMMON_CMD} ${INSTALL_ARG} ${REPO}"
    ${INSTALL_CMD}
    if [ $? -ne 0 ]; then
        exit 1
    fi 
done

rm -fv ${pde_build_dir}/I.CAVE/CAVE-linux.gtk.x86_64.zip
pushd . > /dev/null 2>&1
cd ${pde_build_dir}/I.CAVE
zip -r CAVE-linux.gtk.x86_64.zip cave 
popd > /dev/null
cp ${pde_build_dir}/I.CAVE/CAVE-linux.gtk.x86_64.zip ${WORKSPACE}/rpms/awips2.cave/setup/dist/

cd ${WORKSPACE}/rpms/awips2.cave
buildRPMExec "Installer.cave" "awips2-cave"
buildRPMExec "Installer.cave-wrapper" ""
