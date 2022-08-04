# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# RecommendWindWWConfig
#
# SOFTWARE HISTORY
#
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- ------------------------------------------
# May  9, 2019 21020      tlefebvr    Original version
# May 16, 2019 21020      tlefebvr    Code review changes
# Aug 22, 2019 21020      tlefebvr    Minor Code Review changes
#
################################################################################

import SmartScript

# These values define the percentage of grid points that must meet the criteria
# defined below before a hazard can be assigned to the specified zone type.
AreaThresholds = {
                  "Coastal" : 10,
                  "Inland" : 10,
                  }

WSPThresholds = {
                 # These numbers define the forecast hour of the guidance.
                 # TR uses 34 kt and HU uses 64 kt probabilities.
                 ("FcstTime", "TR.A") : 54,
                 ("FcstTime", "TR.W") : 42,
                 ("FcstTime", "HU.A") : 60,
                 ("FcstTime", "HU.W") : 48,

                 # These numbers define the threshold at or above which the
                 # given hazard meets that hazard's criteria. If enough points
                 # meet the criteria (Area Thresholds), that hazard is assigned
                 # to that zone.
                 ("CoastalAdd", "TR.A") : 30,
                 ("CoastalAdd", "TR.W") : 15,
                 ("CoastalAdd", "HU.A") : 25,
                 ("CoastalAdd", "HU.W") : 10,

                 ("InlandAdd", "TR.A") : 40,
                 ("InlandAdd", "TR.W") : 25,
                 ("InlandAdd", "HU.A") : 35,
                 ("InlandAdd", "HU.W") : 20,

                 # These are the thresholds for removing a hazard previously
                 # in effect. So, 0 means that any points above 0 are considered.
                 # If the number above zero is below the area threshold, remove
                 # this zone.
                 ("CoastalRemove", "TR.A") : 0,
                 ("CoastalRemove", "TR.W") : 0,
                 ("CoastalRemove", "HU.A") : 0,
                 ("CoastalRemove", "HU.W") : 0,

                 ("InlandRemove", "TR.A") : 5,
                 ("InlandRemove", "TR.W") : 5,
                 ("InlandRemove", "HU.A") : 5,
                 ("InlandRemove", "HU.W") : 5,

                 }


