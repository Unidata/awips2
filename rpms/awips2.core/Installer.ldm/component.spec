#
# AWIPS II LDM Spec File
#
%define __prelink_undo_cmd %{nil}
Name: awips2-ldm
Summary: AWIPS II LDM Distribution
Version: 6.8.1
Release: 24
Group: AWIPSII
BuildRoot: /tmp
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: Bryan Kowal

AutoReq: no
provides: awips2-ldm
provides: awips2-base-component

%description
AWIPS II LDM Distribution - Contains AWIPS II LDM.

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "${RPM_BUILD_ROOT}" = "/tmp" ]
then
   echo "An Actual BuildRoot Must Be Specified. Use The --buildroot Parameter."
   echo "Unable To Continue ... Terminating"
   exit 1
fi
LDM_BUILD_DIR="/tmp/awips2-${USER}/ldm-build"
LDM_TAR_DIR="rpms/awips2.core/Installer.ldm/src"
LDM_TAR_FILE="ldm-6.8.1.tar.gz"

if [ -d ${RPM_BUILD_ROOT} ]; then
   rm -rf ${RPM_BUILD_ROOT}
   if [ $? -ne 0 ]; then
      exit 1
   fi
fi
if [ -d ${LDM_BUILD_DIR} ]; then
   rm -rf ${LDM_BUILD_DIR}
fi
mkdir -p ${LDM_BUILD_DIR}
if [ -d ${RPM_BUILD_ROOT}/usr/local/ldm]; then
   rm -rf ${RPM_BUILD_ROOT}/usr/local/ldm-6.8.1/ldm-6.8.1
fi
mkdir -p ${RPM_BUILD_ROOT}/usr/local/ldm-6.8.1/ldm-6.8.1
# We cannot safely build ldm on a machine awips2-ldm
# is already installed on.
if rpm -q awips2-ldm
then
   echo "ERROR: the awips2-ldm rpm must not be built"
   echo "       on a machine with an awips2-ldm"
   echo "       installation."
   echo "Unable To Continue ... Terminating."
   exit 1
fi
if [ -d /usr/local/ldm-6.8.1 ]; then
   rm -rf /usr/local/ldm-6.8.1
fi
mkdir -p /usr/local/ldm-6.8.1

# Copy the src to the build directory.
cp %{_baseline_workspace}/${LDM_TAR_DIR}/${LDM_TAR_FILE} \
   ${LDM_BUILD_DIR}
# Copy patch0 to the build directory.
cp %{_baseline_workspace}/${LDM_TAR_DIR}/ldm-6.8.1.patch0 \
   ${LDM_BUILD_DIR}
# Copy patch1 to the build directory.
cp %{_baseline_workspace}/${LDM_TAR_DIR}/ldm-6.8.1.patch1 \
   ${LDM_BUILD_DIR}
# Copy patch2 to the build directory.
cp %{_baseline_workspace}/${LDM_TAR_DIR}/ldm-6.8.1.patch2 \
   ${LDM_BUILD_DIR}
cd ${LDM_BUILD_DIR}
tar -xvf ${LDM_TAR_FILE}
# remove the copied tar file
rm -f ${LDM_TAR_FILE}

cd ldm-6.8.1
patch -p1 < ../ldm-6.8.1.patch0

# remove the patch file
rm -f ../ldm-6.8.1.patch0

patch -p1 < ../ldm-6.8.1.patch1

# remove the patch file
rm -f ../ldm-6.8.1.patch1

patch -p1 < ../ldm-6.8.1.patch2

# remove the patch file.
rm -f ../ldm-6.8.1.patch2

%build
LDM_BUILD_DIR="/tmp/awips2-${USER}/ldm-build"
LDM_TAR_DIR="rpms/awips2.core/Installer.ldm/src"
LDM_TAR_FILE="ldm-6.8.1.tar.gz"

# go to the ldm src directory.
cd ${LDM_BUILD_DIR}/ldm-6.8.1/src
# set LDMHOME
export LDMHOME=/usr/local/ldm-6.8.1
# run the configure script

./configure \
   --prefix=${RPM_BUILD_ROOT}/usr/local/ldm-6.8.1/ldm-6.8.1
RC="$?"
if [ ! "${RC}" = "0" ]; then
   echo "ERROR: configure of ldm-6.8.1 has failed."
   echo "Unable To Continue ... Terminating."
   exit 1
fi
# make ldm
make clean
make
RC="$?"
if [ ! "${RC}" = "0" ]; then
   echo "ERROR: make of ldm-6.8.1 has failed."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

%install
LDM_BUILD_DIR="/tmp/awips2-${USER}/ldm-build"

cd ${LDM_BUILD_DIR}/ldm-6.8.1/src
make install
RC="$?"
if [ ! "${RC}" = "0" ]; then
   echo "ERROR: install of ldm-6.8.1 has failed."
   echo "Unable To Continue ... Terminating."
   exit 1
fi

# copy the ldm src to the installation
cd ${LDM_BUILD_DIR}/ldm-6.8.1
cp -r src ${RPM_BUILD_ROOT}/usr/local/ldm-6.8.1/ldm-6.8.1

# remove the build directory.
rm -rf ${LDM_BUILD_DIR}

# re-locate the etc directory in the install
cd /usr/local/ldm-6.8.1
mv etc ${RPM_BUILD_ROOT}/usr/local/ldm-6.8.1

# create links to the ldm directories
cd ${RPM_BUILD_ROOT}/usr/local/ldm-6.8.1
ln -s ldm-6.8.1 runtime
ln -s runtime/bin bin
ln -s runtime/include include
ln -s runtime/lib lib
ln -s runtime/man man
ln -s runtime/src src

# create our standard directories
mkdir .ssh
touch .ssh/known_hosts
mkdir data
mkdir decoders
mkdir logs

# create our ld.so.conf.d file.
mkdir -p ${RPM_BUILD_ROOT}/etc/ld.so.conf.d
touch ${RPM_BUILD_ROOT}/etc/ld.so.conf.d/awips2-i386.conf
echo "/usr/local/ldm-6.8.1/lib" >> \
   ${RPM_BUILD_ROOT}/etc/ld.so.conf.d/awips2-i386.conf

# install our "patches"
PATCH_DIR="rpms/awips2.core/Installer.ldm/patch"
# Copy the hidden files.
hidden_files=( '.bash_profile' '.bashrc' '.cshrc' \
   '.lesshst' '.viminfo' )
for hiddenFile in ${hidden_files[*]}; do
   cp %{_baseline_workspace}/${PATCH_DIR}/${hiddenFile} \
      ${RPM_BUILD_ROOT}/usr/local/ldm-6.8.1
done
# Copy the contents of the bin directory.
cp -f %{_baseline_workspace}/${PATCH_DIR}/bin/* \
   ${RPM_BUILD_ROOT}/usr/local/ldm-6.8.1/bin
# Copy the contents of the decoder directory.
cp %{_baseline_workspace}/${PATCH_DIR}/decoders/* \
   ${RPM_BUILD_ROOT}/usr/local/ldm-6.8.1/decoders
# Copy the contents of the lib directory.
cp -P %{_baseline_workspace}/${PATCH_DIR}/lib/* \
   ${RPM_BUILD_ROOT}/usr/local/ldm-6.8.1/lib
# Copy the contents of the etc directory.
cp -f %{_baseline_workspace}/${PATCH_DIR}/etc/* \
   ${RPM_BUILD_ROOT}/usr/local/ldm-6.8.1/etc
   
# Merge pqact.conf.oax and pqact.conf.template to create
# our pqact.conf file.
pushd . > /dev/null 2>&1
cd ${RPM_BUILD_ROOT}/usr/local/ldm-6.8.1/etc
if [ ! -f pqact.conf.template ]; then
   echo "ERROR: pqact.conf.template does not exist."
   exit 1
fi
if [ ! -f pqact.conf.oax ]; then
   echo "ERROR: pqact.conf.oax does not exist."
   exit 1
fi

cp pqact.conf.template pqact.conf
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: Unable to create the pqact.conf file."
   exit 1
fi
cat pqact.conf.oax >> pqact.conf
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: Unable to merge pqact.conf.oax and pqact.conf."
   exit 1
fi
popd > /dev/null 2>&1

# Move our profile.d script to its final location.
mkdir -p ${RPM_BUILD_ROOT}/etc/profile.d
cp %{_baseline_workspace}/${PATCH_DIR}/profile.d/awipsLDM.csh \
   ${RPM_BUILD_ROOT}/etc/profile.d
   
%pre
if [ -d /tmp/ldm ]; then
   rm -rf /tmp/ldm
fi
mkdir -p /tmp/ldm
for dir in etc .ssh;
do
   if [ -d /usr/local/ldm-6.8.1/${dir} ]; then
      scp -qrp /usr/local/ldm-6.8.1/${dir} /tmp/ldm
   fi
done

%post
cd /usr/local/ldm-6.8.1/src
export LDMHOME=/usr/local/ldm-6.8.1/ldm-6.8.1
./configure --prefix=/usr/local/ldm-6.8.1/ldm-6.8.1
make clean
make
make install_setuids
RC="$?"
if [ ! "${RC}" = "0" ]; then
   echo "WARNING: 'make install_setuids' has failed."
fi
for dir in etc .ssh;
do
   if [ -d /tmp/ldm/${dir} ]; then
      scp -qrp /tmp/ldm/${dir} /usr/local/ldm-6.8.1
   fi
done

#if a remote CP site, copy over the filtered data configuration
case $SITE_IDENTIFIER in gum|hfo|pbp|vrh)
		echo -e "\nInstalling ldmd.conf for $SITE_IDENTIFIER."
        if ! scp /usr/local/ldm-6.8.1/etc/ldmd.conf.$SITE_IDENTIFIER cpsbn1:/usr/local/ldm/etc/ldmd.conf
        then
            echo "ERROR: Failed copy of ldmd.conf to cpsbn1"
        fi

        if ! scp /usr/local/ldm-6.8.1/etc/ldmd.conf.$SITE_IDENTIFIER cpsbn2:/usr/local/ldm/etc/ldmd.conf
        then
            echo "ERROR: Failed copy of ldmd.conf to cpsbn2"
        fi
        ;;
esac

#remove the extra configuration files
rm -f /usr/local/ldm-6.8.1/etc/ldmd.conf.*

/sbin/ldconfig

rm -rf /tmp/ldm

%postun
/sbin/ldconfig

%clean
rm -rf ${RPM_BUILD_ROOT}
rm -rf /usr/local/ldm-6.8.1
rm -rf /tmp/awips2-${USER}/ldm-build

%files
%defattr(-,ldm,fxalpha,-)
%attr(644,root,root) /etc/ld.so.conf.d/awips2-i386.conf
%dir /usr/local/ldm-6.8.1
/usr/local/ldm-6.8.1/.bash_profile
/usr/local/ldm-6.8.1/.bashrc
/usr/local/ldm-6.8.1/.cshrc
/usr/local/ldm-6.8.1/.lesshst
%dir /usr/local/ldm-6.8.1/.ssh
%config(noreplace) /usr/local/ldm-6.8.1/.ssh/known_hosts
/usr/local/ldm-6.8.1/.viminfo

/usr/local/ldm-6.8.1/bin
/usr/local/ldm-6.8.1/data
%dir /usr/local/ldm-6.8.1/decoders
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/decoders/binaryWriter
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/decoders/binWriter
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/decoders/cruft
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/decoders/decrypt_file
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/decoders/keygen
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/decoders/metarWriter
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/decoders/purge.sh
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/decoders/textWriter
/usr/local/ldm-6.8.1/etc
/usr/local/ldm-6.8.1/include
/usr/local/ldm-6.8.1/lib
%dir /usr/local/ldm-6.8.1/logs
/usr/local/ldm-6.8.1/man
/usr/local/ldm-6.8.1/runtime
/usr/local/ldm-6.8.1/src

%dir /usr/local/ldm-6.8.1/ldm-6.8.1
%dir /usr/local/ldm-6.8.1/ldm-6.8.1/bin
%attr(777,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/afos
%attr(777,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/ddplus
%attr(777,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/dds
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/edexBridge
%attr(777,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/feedme
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/feedtest
%attr(777,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/hds
%attr(777,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/hrs
%attr(777,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/hupsyslog
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/ids
%attr(777,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/ldmadmin
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/ldmcheck
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/ldmfail
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/ldmping
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/ldmsend
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/monitor_data_store.sh
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/netcheck
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/newlog
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/notifyme
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/plotMetrics
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/pps
%attr(777,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/pqact
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/pqcat
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/pqcheck
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/pqcreate
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/pqexpire
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/pqing
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/pqinsert
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/pqmon
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/pqsend
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/pqsurf
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/pqutil
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/regex
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/rpc.ldmd
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/rtstats
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/scour
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/scriptconfig
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/bin/syscheck
/usr/local/ldm-6.8.1/ldm-6.8.1/include
%dir /usr/local/ldm-6.8.1/ldm-6.8.1/lib
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_date_time.so
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_date_time.so.1.33.1
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_date_time.so.2
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_filesystem.so
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_filesystem.so.1.33.1
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_filesystem.so.2
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_iostreams.so
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_iostreams.so.1.33.1
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_iostreams.so.2
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_prg_exec_monitor.so
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_prg_exec_monitor.so.1.33.1
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_prg_exec_monitor.so.2
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_program_options.so
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_program_options.so.1.33.1
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_program_options.so.2
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_python.so
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_python.so.1.33.1
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_python.so.2
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_regex.so
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_regex.so.1.33.1
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_regex.so.2
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_serialization.so
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_serialization.so.1.33.1
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_serialization.so.2
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_signals.so
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_signals.so.1.33.1
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_signals.so.2
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_test_exec_monitor.so
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_test_exec_monitor.so.1.33.1
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_test_exec_monitor.so.2
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_thread.so
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_thread.so.1.33.1
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_thread.so.2
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_unit_test_framework.so
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_unit_test_framework.so.1.33.1
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_unit_test_framework.so.2
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_wserialization.so
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_wserialization.so.1.33.1
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libboost_wserialization.so.2
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libcoroipcc.a
%attr(777,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libcoroipcc.so
%attr(777,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libcoroipcc.so.4
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libcoroipcc.so.4.0.0
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libcoroipcs.a
%attr(777,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libcoroipcs.so
%attr(777,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libcoroipcs.so.4
%attr(755,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libcoroipcs.so.4.0.0
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libldm.a
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libqpidclient.so
%attr(777,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libqpidclient.so.4
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libqpidclient.so.4.0.0
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libqpidcommon.so
%attr(777,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libqpidcommon.so.4
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libqpidcommon.so.4.0.0
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libqpidmessaging.so
%attr(777,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libqpidmessaging.so.3
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libqpidmessaging.so.3.0.2
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libqpidtypes.so
%attr(777,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libqpidtypes.so.1
%attr(644,ldm,fxalpha) /usr/local/ldm-6.8.1/ldm-6.8.1/lib/libqpidtypes.so.1.1.1
%docdir /usr/local/ldm-6.8.1/ldm-6.8.1/man
/usr/local/ldm-6.8.1/ldm-6.8.1/man
/usr/local/ldm-6.8.1/ldm-6.8.1/src
%attr(755,root,root) /etc/profile.d/awipsLDM.csh
