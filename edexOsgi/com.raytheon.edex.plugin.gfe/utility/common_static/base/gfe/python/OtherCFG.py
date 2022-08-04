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
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# Apr 22, 2020  8151     randerso  Added NHP, NHZ, HAK, HUS, and NWC
#
##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

##
# Contains information about products, regions, etc. for non-WFO GFE sites.
# region= two-letter regional identifier, mainly used for installation of
#         text product templates
SiteInfo = {
    'NH1': {
        'region': 'NC',
        'fullStationID': 'KNHC',
        'wfoCityState': 'National Hurricane Center Miami FL',
        'wfoCity': 'Miami',
        'state': 'Florida',
    },
    'NH2': {
        'region': 'NC',
        'fullStationID': 'KNHC',
        'wfoCityState': 'National Hurricane Center Miami FL',
        'wfoCity': 'Miami',
        'state': 'Florida',
    },
    'NHA': {
        'region': 'NC',
        'fullStationID': 'KNHC',
        'wfoCityState': 'National Hurricane Center Miami FL',
        'wfoCity': 'Miami',
        'state': 'Florida',
    },
    'NHP': {
        'region': 'NC',
        'fullStationID': 'KNHC',
        'wfoCityState': 'National Hurricane Center Miami FL',
        'wfoCity': 'Miami',
        'state': 'Florida',
    },
    'NHZ': {
        'region': 'NC',
        'fullStationID': 'KNHC',
        'wfoCityState': 'National Hurricane Center Miami FL',
        'wfoCity': 'Miami',
        'state': 'Florida',
    },
    'ONA': {
        'region': 'NC',
        'fullStationID': 'KWBC',
        'wfoCityState': 'Ocean Prediction Center Washington DC',
        'wfoCity': 'Washington DC',
        'state': '',
    },
    'ONP': {
        'region': 'NC',
        'fullStationID': 'KWBC',
        'wfoCityState': 'Ocean Prediction Center Washington DC',
        'wfoCity': 'Washington DC',
        'state': '',
    },
    'HUS': {
        'region': 'NC',
        'fullStationID': 'KWNH',
        'wfoCityState': 'Weather Prediction Center College Park MD',
        'wfoCity': 'College Park',
        'state': 'Maryland',
    },
    'HAK': {
        'region': 'NC',
        'fullStationID': 'KWNH',
        'wfoCityState': 'Weather Prediction Center College Park MD',
        'wfoCity': 'College Park',
        'state': 'Maryland',
    },
    'NWC': {
        'region': 'NC',
        'fullStationID': 'KNWC',
        'wfoCityState': 'National Water Center Tuscaloosa AL',
        'wfoCity': 'Tuscaloosa',
        'state': 'Alabama',
    },
}
