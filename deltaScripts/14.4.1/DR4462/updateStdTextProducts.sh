#!/bin/bash
##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##

# Issue: #4462 Update script to update stdtextprodcts entries with blank site values.
#
# This script will modify table stdtextproducts in the fxatext.public schema.
# 
PSQL="/awips2/psql/bin/psql"

if [ ${#1} != 4 ] ; then
	echo "ERROR: First argument must be local site to use in the stdtextproducts table. Example: KOAX"
	exit 1
else
	siteId=${1}
	siteLtr=${siteId:0:1}
fi

if [ ! -f ${PSQL} ];
then
	echo "ERROR: The PSQL executable does not exist - ${PSQL}."
	echo "FATAL: Update Failed!"
	exit 1
fi

UPDATE_BY_XXX="update public.stdtextproducts set site = '${siteLtr}' || xxxid where site='' and xxxid not like '% ';"
UPDATE_LOC_SITE="update public.stdtextproducts set site='${siteId}' where site='' and xxxid like '% ';"

function updateXXXentries
{
	echo "INFO: Updating stdtextproducts using xxxid"
${PSQL} -U awips -d fxatext -a -c "${UPDATE_BY_XXX}"
	if [ $? -ne 0 ];
	then
		echo "FATAL: Update by xxxid Failed!"
		exit 1	
	fi
	echo "INFO: Completed updating stdtextproducts using xxxid."
}

function updateSITEentries
{
	echo "INFO: Updating stdtextproduct table's site using local site."
${PSQL} -U awips -d fxatext -a -c "${UPDATE_LOC_SITE}" 
	if [ $? -ne 0 ];
	then
		echo "FATAL: unable to update stdtextproducts using local site."
		exit 1
	fi
	echo "INFO: Finish updating stdtextproducts using local site."
}

echo "INFO: start update stdtextproducts"
updateXXXentries
updateSITEentries
echo "INFO: finish update stdtextproducts"
exit 0


