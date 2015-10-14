#!/bin/bash

# This is the initial version of the new script that will perform
# the CAVE PDE builds. This script will produce both the CAVE RCP
# applications for products in the baseline as well as the Eclipse
# p2 repositories for features in the baseline.
#
# Note: this script is not yet fully integrated. Another changeset is
# waiting in review that the full integration is dependent on.
VAR_WORKSPACE="workspace"
VAR_UFRAME_ECLIPSE="/opt/eclipse"

# Initially, we will demonstrate building an RCP application for awips.product 
# with all features found in the main baseline as well as p2 repositories for each
# feature.
if [ "${UFRAME_ECLIPSE}" = "" ]; then
	export UFRAME_ECLIPSE="${VAR_UFRAME_ECLIPSE}"
fi

if [ "${WORKSPACE}" = "" ]; then
	export WORKSPACE="${VAR_WORKSPACE}"
fi

build_project_dir=${WORKSPACE}/build
pde_base_dir=${build_project_dir}/cave
pde_build_dir=${pde_base_dir}/tmp
prepare_dir=${pde_base_dir}/prepare
awips_product=com.raytheon.viz.product.awips/awips.product

if [ ! -d ${prepare_dir} ]; then
	mkdir ${prepare_dir}
fi

# First, we need to build the dependency utility.
pushd . > /dev/null 2>&1
cd ${WORKSPACE}/awips.dependency.evaluator
/awips2/ant/bin/ant -f build.xml -Dbaseline.dir=${WORKSPACE} \
    -Declipse.dir=${UFRAME_ECLIPSE} -Ddest.dir=${prepare_dir}
if [ $? -ne 0 ]; then
    exit 1
fi

cd ${prepare_dir}
# Next, stage the plugins and determine what needs to be built.
# In another scenario that jar utility could be ran again with only the subset of features
# that need to be built prior to the repository build.
/awips2/java/bin/java -jar -DbaseLocation=${UFRAME_ECLIPSE} \
    -DbuildDirectory=${pde_build_dir} -DstagingDirectory=${WORKSPACE} -DbuildFeatures=* \
    -DbuildProduct=${awips_product} AwipsDependencyEvaluator.jar
if [ $? -ne 0 ]; then
    exit 1
fi
	
_pde_launcher_jar=${UFRAME_ECLIPSE}/plugins/org.eclipse.equinox.launcher_1.3.0.v20120522-1813.jar
_pde_product_xml=${UFRAME_ECLIPSE}/plugins/org.eclipse.pde.build_3.8.2.v20121114-140810/scripts/productBuild/productBuild.xml

# Complete the CAVE RCP build.
/awips2/java/bin/java -jar ${_pde_launcher_jar} -application org.eclipse.ant.core.antRunner \
    -buildfile ${_pde_product_xml} -DbaseLocation=${UFRAME_ECLIPSE} \
    -Dbuilder=${pde_base_dir} -DbuildDirectory=${pde_build_dir} \
    -Dbase=${pde_base_dir} -DproductFile=awips.product
if [ $? -ne 0 ]; then
    exit 1
fi

# Prepare for the CAVE repository build. Need to create more resuse for a single build
# properties file so that it can be used for both product and feature builds.
pde_build_dir=${pde_base_dir}/p2
pde_base_dir=${pde_base_dir}/p2
/awips2/java/bin/java -jar -DbaseLocation=${UFRAME_ECLIPSE} \
    -DbuildDirectory=${pde_build_dir} -DstagingDirectory=${WORKSPACE} -DbuildFeatures=* \
    -DoutputFile=${prepare_dir}/repositoriesToBuild.txt \
    AwipsDependencyEvaluator.jar
if [ $? -ne 0 ]; then
    exit 1
fi

_pde_build_xml=${UFRAME_ECLIPSE}/plugins/org.eclipse.pde.build_3.8.2.v20121114-140810/scripts/build.xml

mkdir -p ${pde_base_dir}
cp -v ${build_project_dir}/build.properties.p2 ${pde_base_dir}/build.properties
for feature in `cat ${prepare_dir}/repositoriesToBuild.txt`; do
/awips2/java/bin/java -jar ${_pde_launcher_jar} -application org.eclipse.ant.core.antRunner \
    -buildfile ${_pde_build_xml} -DbaseLocation=${UFRAME_ECLIPSE} \
    -Dbuilder=${pde_base_dir} -DbuildDirectory=${pde_build_dir} \
    -Dbase=${pde_base_dir} -DtopLevelElementId=${feature} \
    -Dconfigs=linux,gtk,x86_64
    if [ $? -ne 0 ]; then
        exit 1
    fi  
    
    # At this point, we will also store the repository that was produced in a zip archive
    # for distribution.
done

popd > /dev/null 2>&1