##
##

##
# This is a base file that is not intended to be overridden.
##

# ---------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without
# technical  support, and with no warranty, express or implied, as to
# its usefulness for any purpose.
#
# PublicZones_EditAreas_<site>_<MultiPil>_Definition.TextUtility
#
#  This file is used to generate a combinations file that contains
#  all PublicZones codes for the given site.  This file is similar to a product
#  definition file, but only contains what is needed for the installation
#  to generate a combinations file.
#
# ---------------------------------------------------------------------

Definition = {}

#----- WFO <site> PublicZones_EditAreas Definition -----

#Name of the generated combinations file
Definition["defaultEditAreas"] = "EditAreas_PublicZones_<site>_<MultiPil>"

#MapBackground for creating the combinations file
Definition["mapNameForCombinations"] = "Zones_<site>" 

#Special multiple product domains for certain sites:
if "<site>" == "AFG":
    if "_<MultiPil>" == "_AFG":
        Definition["subDomainUGCs"] = ["AKZ218","AKZ219","AKZ220","AKZ221",
                                       "AKZ222","AKZ223","AKZ224","AKZ225",
                                       "AKZ226"]
    elif "_<MultiPil>" == "_NSB":
        Definition["subDomainUGCs"] = ["AKZ201","AKZ202","AKZ203","AKZ204",
                                       "AKZ205","AKZ206"]
    elif "_<MultiPil>" == "_WCZ":
        Definition["subDomainUGCs"] = ["AKZ207","AKZ208","AKZ209","AKZ210",
                                       "AKZ211","AKZ212","AKZ213","AKZ214",
                                       "AKZ215","AKZ216","AKZ217","AKZ227"]

