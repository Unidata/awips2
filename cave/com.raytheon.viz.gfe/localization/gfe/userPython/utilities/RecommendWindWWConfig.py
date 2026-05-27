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
# Jul 24, 2020            Psantos     Updated thresholds based on DeMaria/Franklin validation based on 2019 WSP model.
# Aug  2, 2020            Psantos     Updated thresholds.
# Sep 15, 2022            Psantos     Updated thresholds based on HWTs
################################################################################

import SmartScript

# These values define the percentage of grid points that must meet the criteria
# defined below before a hazard can be assigned to the specified zone type.
AreaThresholds = {
                  "Coastal" : 50,
                  "Inland" : 50,
                  }

WSPThresholds = {
                 # These numbers define the forecast hour of the guidance.
                 # TR uses 34 kt and HU uses 64 kt probabilities.
                 ("FcstTime", "TR.A") : 48,
                 ("FcstTime", "TR.W") : 36,
                 ("FcstTime", "HU.A") : 60,
                 ("FcstTime", "HU.W") : 48,

                 # These numbers define the threshold at or above which the
                 # given hazard meets that hazard's criteria. If enough points
                 # meet the criteria (Area Thresholds), that hazard is assigned
                 # to that zone.
                 ("CoastalAdd", "TR.A") : 23,
                 ("CoastalAdd", "TR.W") : 33,
                 ("CoastalAdd", "HU.A") : 4,
                 ("CoastalAdd", "HU.W") : 9,

                 ("InlandAdd", "TR.A") : 43,
                 ("InlandAdd", "TR.W") : 53,
                 ("InlandAdd", "HU.A") : 24,
                 ("InlandAdd", "HU.W") : 29,

                 # These are the thresholds for removing a hazard previously
                 # in effect. So, 0 means that any points above 0 are considered.
                 # If the number above zero is below the area threshold, remove
                 # this zone.
                 ("CoastalRemove", "TR.A") : 0,
                 ("CoastalRemove", "TR.W") : 0,
                 ("CoastalRemove", "HU.A") : 0,
                 ("CoastalRemove", "HU.W") : 0,

                 ("InlandRemove", "TR.A") : 0,
                 ("InlandRemove", "TR.W") : 0,
                 ("InlandRemove", "HU.A") : 0,
                 ("InlandRemove", "HU.W") : 0,

                 }