Name: awips2-edex-enableservices
Summary: AWIPS II Edex Enable Services
Version: %{_component_version}
Release: %{_component_release}
Group: AWIPSII
BuildRoot: %{_build_root}
URL: N/A
License: N/A
Distribution: N/A
Vendor: Raytheon
Packager: %{_build_site}
requires: awips2-edex

%description
AWIPS II Edex Enable Installation - Enables EDEX JVMs with systemd.

%prep
# Verify That The User Has Specified A BuildRoot.
if [ "%{_build_root}" = "" ]
then
   echo "ERROR: The RPM Build Root has not been specified."
   exit 1
fi

if [ -d %{_build_root} ]; then
   rm --recursive --force %{_build_root}
fi

%build

%install

%pre

%post

#determine whether to disable ingestDat
disableIngestDat=false

if [ -f /data/fxa/INSTALL/awips2/scripts/.global ]; then
   source /data/fxa/INSTALL/awips2/scripts/.global

   #".global" has arrays defined with the '|' separater, so set
   # IFS temporarily in order to process them.
   IFS='|'

   thisSite=$(echo $SITE_IDENTIFIER | tr '[:upper:]' '[:lower:]')
   for site in ${ncepCaseArray[*]};
      do
         # After separation, first/last elements on ncepCaseArray may have
         # extra "+(" or ")" on them, e.g "+(opcn"
         if [[ $thisSite != '' ]] && [[ $site =~ ^(\+\()?$thisSite\)?$ ]]; then
            disableIngestDat=true
         fi
   done

   unset IFS
fi

#update edexServiceList on install
if [ "${1}" = "1" ]; then

  #add services to the edex service list
  LIST_FILE=/awips2/edex/conf/edexServiceList
  BASE_SERVICES=(ingest ingestGrib request)

  if [ -f $LIST_FILE ]; then
     source $LIST_FILE

     for service in ${BASE_SERVICES[*]}; do
        addService=true;
        for index in ${!SERVICES[@]};
        do
           if [ ${SERVICES[$index]} = $service ]; then
              addService=false;
           fi
        done
        if $addService; then
           SERVICES=(${SERVICES[@]} $service)
        fi
     done
  else
     SERVICES=${BASE_SERVICES[@]}
  fi

  if [ "$disableIngestDat" = true ]; then
     for index in ${!SERVICES[@]}
        do
           if [ ${SERVICES[$index]} = ingestDat ]; then
              unset SERVICES[$index]
           fi
     done
  fi

  echo "#generated on $(date)" > $LIST_FILE
  echo "export SERVICES=(${SERVICES[@]})" >> $LIST_FILE

  # enable the standard edex modes
  if [ -f %{_unitdir}/edex_camel@.service ]; then
     for item in "${BASE_SERVICES[@]}";
     do
        if [ "$item" = ingestDat ] && [ "$disableIngestDat" = true ]; then
            continue
        fi
        /bin/systemctl enable --quiet edex_camel@"${item}"
     done
  fi
fi

if [ "${1}" = "2" ]; then

  #add services to the edex service list
  LIST_FILE=/awips2/edex/conf/edexServiceList
  BASE_SERVICES=(ingest ingestDat ingestGrib request)

  if [ -f $LIST_FILE ]; then
     source $LIST_FILE

     for service in ${BASE_SERVICES[*]}; do
        addService=true;
        for index in ${!SERVICES[@]};
        do
           if [ ${SERVICES[$index]} = $service ]; then
              addService=false;
           fi
        done
        if $addService; then
           SERVICES=(${SERVICES[@]} $service)
        fi
     done
  else
     SERVICES=${BASE_SERVICES[@]}
  fi

  if [ "$disableIngestDat" = true ]; then
     for index in ${!SERVICES[@]}
        do
           if [ ${SERVICES[$index]} = ingestDat ]; then
              unset SERVICES[$index]
           fi
     done
  fi

  echo "#generated on $(date)" > $LIST_FILE
  echo "export SERVICES=(${SERVICES[@]})" >> $LIST_FILE

  # reenable the standard edex modes
  if [ -f %{_unitdir}/edex_camel@.service ]; then
     for item in "${BASE_SERVICES[@]}";
     do
        if [ "$item" = ingestDat ] && [ "$disableIngestDat" = true ]; then
            continue
        fi
        /bin/systemctl reenable --quiet edex_camel@"${item}"
     done
  fi
fi


%preun
if [ "${1}" = "1" ]; then
   exit 0
fi

LIST_FILE=/awips2/edex/conf/edexServiceList
BASE_SERVICES=(ingest ingestGrib request)

if [ -f $LIST_FILE ]; then
   source $LIST_FILE

   for service in ${BASE_SERVICES[*]}; do
       for index in ${!SERVICES[@]}
       do
           if [ ${SERVICES[$index]} = $service ]; then
              unset SERVICES[$index]
           fi
       done
   done

   echo "#generated on $(date)" > $LIST_FILE
   echo "export SERVICES=(${SERVICES[@]})" >> $LIST_FILE
fi

# shut down and disable the standard modes
for item in "${BASE_SERVICES[@]}";
do
   /bin/systemctl disable --now --quiet edex_camel@"${item}"
done

%postun

%clean
rm --recursive --force %{_build_root}

%files

