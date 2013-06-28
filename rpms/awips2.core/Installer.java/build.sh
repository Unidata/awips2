# verify that the workspace, rpm top directory, and build root are available in the environment

if [ -z ${WORKSPACE} ]; then
   echo "Error: the location of the baseline workspace must be specified using the WORKSPACE environment variable."
   exit 1
fi
if [ -z ${AWIPSII_TOP_DIR} ]; then
   echo "Error: the location of the rpm top directory must be specified using the AWIPSII_TOP_DIR environment variable."
   exit 1
fi
if [ -z ${AWIPSII_BUILD_ROOT} ]; then
   echo "Error: the location of the AWIPS II build root must be specified using the AWIPSII_BUILD_ROOT environment variable."
   exit 1
fi

function buildRPM()
{
   SPECS=${1}

   /usr/bin/rpmbuild -ba \
      --define "_topdir ${AWIPSII_TOP_DIR}" \
      --define "_build_root ${AWIPSII_BUILD_ROOT}" \
      --define "_baseline_workspace ${WORKSPACE}" \
      --buildroot ${AWIPSII_BUILD_ROOT} \
      ${SPECS}
   if [ $? -ne 0 ]; then
      return 1
   fi
}

# build java 1.6
SPECS=1.6/component.spec
buildRPM ${SPECS}
if [ $? -ne 0 ]; then
   exit 1
fi

# build java 1.7
SPECS=1.7/component.spec
buildRPM ${SPECS}
if [ $? -ne 0 ]; then
   exit 1
fi
