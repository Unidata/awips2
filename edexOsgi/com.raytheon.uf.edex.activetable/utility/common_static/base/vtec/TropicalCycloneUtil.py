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

##
# Utility module for dealing with tropical cyclone hazards in the activetable.
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/20/16        #5709         dgilling       Initial creation.
#
##


TROPICAL_PHENS = ['TR','TY','HU']

def get_tropical_storm_basin(record):
    if record['phen'] not in TROPICAL_PHENS:
        msg = "Active table record is not for tropical hazard phensig: {}.{}".format(record['phen']. record['sig'])
        raise ValueError(msg)

    if int(record['etn']) in range(1001, 1080) or \
       int(record['etn']) in range(1090, 2000):
        return "AT"
    elif int(record['etn']) in range(1080, 1090):
        return "TEST"
    elif int(record['etn']) in range(2001, 3000):
        return "EP"
    elif int(record['etn']) in range(3001, 4000):
        return "CP"
    elif int(record['etn']) in range(4001, 5000):
        return "WP"
    else:
        msg = "Tropical hazard has ETN outside any known basin ETN range: {}".format(int(record['etn']))
        raise ValueError(msg)



