function WA_rpm_build()
{
	echo "Scanning the Baseline for WA-Contributed RPMs ..."
	
	files=(${WORKSPACE}/rpms-*)
	if [ ! -d "${files[0]}" ]; then
		echo "No WA-Contributed RPMs were found!"
		return
	fi
		
	wa_contrib_count=`ls -1d ${WORKSPACE}/rpms-* | wc -l`
	
	echo "Found ${wa_contrib_count} WA-Contributed RPMs."
	for rpm_contribution in `ls -1d ${WORKSPACE}/rpms-*`; do
		echo "Processing WA-Contributed RPMs ... ${rpm_contribution}"
		
		_build_dir="${rpm_contribution}/build"
		_lookup_script="${_build_dir}/lookupWA_RPM.sh"
		_contribution_txt="${_build_dir}/wa-rpms.txt"
		
		source "${_lookup_script}"
		if [ $? -ne 0 ]; then
			echo "ERROR: unable to access the expected WA lookup script - ${_lookup_script}"
			exit 1	
		fi
		
		if [ ! -f "${_contribution_txt}" ]; then
			echo "ERROR: unable to access the expected contribution text - ${_contribution_txt}"
			exit 1
		fi
		
		for contribution in `cat ${_contribution_txt}`; do
			lookupWA_RPM "${contribution}" "${rpm_contribution}"
			if [ $? -ne 0 ]; then
    			echo "ERROR: '${contribution}' is not a recognized AWIPS II RPM."
    			exit 1
			fi			
			
			buildRPMExec "${RPM_SPECIFICATION}"
		done
		
		return 0 
	done
}