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
# Jul 16, 2025  2039004 dkingfield Added AAWU, AWC, CPC, NTWC, PTWC, SPC, SWPC
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
    'AAWU': {
        'region': 'AR',
        'fullStationID': 'PAWU',
        'wfoCityState': 'Alaska Aviation Weather Unit Anchorage AK',
        'wfoCity': 'Anchorage',
        'state': 'Alaska',
    },
    'AWC': {
        'region': 'NC',
        'fullStationID': 'KKCI',
        'wfoCityState': 'Aviation Weather Center Kansas City MO',
        'wfoCity': 'Kansas City',
        'state': 'Missouri',
    },
    'CPC': {
        'region': 'NC',
        'fullStationID': 'KWNC',
        'wfoCityState': 'Climate Prediction Center College Park MD',
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
    'HUS': {
        'region': 'NC',
        'fullStationID': 'KWNH',
        'wfoCityState': 'Weather Prediction Center College Park MD',
        'wfoCity': 'College Park',
        'state': 'Maryland',
    },
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
    'NTWC': {
        'region': 'NC',
        'fullStationID': 'NTWC',
        'wfoCityState': 'National Tsunami Warning Center Palmer AK',
        'wfoCity': 'Palmer',
        'state': 'Alaska',
    },
    'NWC': {
        'region': 'NC',
        'fullStationID': 'KNWC',
        'wfoCityState': 'National Water Center Tuscaloosa AL',
        'wfoCity': 'Tuscaloosa',
        'state': 'Alabama',
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
    'PTWC': {
        'region': 'NC',
        'fullStationID': 'PTWC',
        'wfoCityState': 'Pacific Tsunami Warning Center Honolulu HI',
        'wfoCity': 'Honolulu',
        'state': 'Hawaii',
    },
    'SPC': {
        'region': 'NC',
        'fullStationID': 'KWNS',
        'wfoCityState': 'Storm Prediction Center Norman OK',
        'wfoCity': 'Norman',
        'state': 'Oklahoma',
    },
    'SWPC': {
        'region': 'NC',
        'fullStationID': 'KWNP',
        'wfoCityState': 'Space Weather Prediction Center Boulder CO',
        'wfoCity': 'Boulder',
        'state': 'Colorado',
    },
}
