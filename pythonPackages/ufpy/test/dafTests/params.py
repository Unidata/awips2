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


#
# Site-specific parameters for DAF tests
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/07/16        5981          tgurney        Initial creation
#    12/15/16        5981          tgurney        Add ENVELOPE
#    04/14/22        8845          njensen        Add POINT
#
#

from shapely.geometry import box, Point

AIRPORT = 'OMA'
OBS_STATION = 'KOMA'
SITE_ID = 'OAX'
STATION_ID = '72558'
RADAR = 'KOAX'
POINT = Point(-96.25, 41.16)
SAMPLE_AREA = (-97.0, 41.0, -96.0, 42.0)

ENVELOPE = box(*SAMPLE_AREA)