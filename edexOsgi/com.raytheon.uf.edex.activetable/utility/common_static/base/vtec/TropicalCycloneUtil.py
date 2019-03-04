##
##

##
# Utility module for dealing with tropical cyclone hazards in the activetable.
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/20/16        #5709         dgilling       Initial creation.
#    06/07/17        #6312         dgilling       Add SS to TROPICAL_PHENS.
#
##

##
# This is a base file that is not intended to be overridden.
##



TROPICAL_PHENS = ['TR','TY','HU', 'SS']

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



