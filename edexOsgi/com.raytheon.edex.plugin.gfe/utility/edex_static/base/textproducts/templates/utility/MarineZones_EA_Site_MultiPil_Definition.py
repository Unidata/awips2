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
# ---------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without
# technical  support, and with no warranty, express or implied, as to
# its usefulness for any purpose.
#
# MarineZones_EditAreas_<site>_<MultiPil>_Definition.TextUtility
#
#  This file is used to generate a combinations file that contains
#  all MarineZones codes for the given site.  This file is similar to a product
#  definition file, but only contains what is needed for the installation
#  to generate a combinations file.
#
# ---------------------------------------------------------------------

Definition = {}

#----- WFO <site> MarineZones_EditAreas Definition -----

#Name of the generated combinations file
Definition["defaultEditAreas"] = "EditAreas_MarineZones_<site>_<MultiPil>"

#MapBackground for creating the combinations file
Definition["mapNameForCombinations"] = "Marine_Zones_<site>" 

#Special multiple product domains for certain sites:
if "<site>" == "AJK":
    if "_<MultiPil>" == "_AJK":
        Definition["subDomainUGCs"] = ["PKZ011","PKZ012","PKZ013","PKZ021",
                                       "PKZ022","PKZ031","PKZ032","PKZ033",
                                       "PKZ034","PKZ035","PKZ036"]
    elif "_<MultiPil>" == "_AEG":
        Definition["subDomainUGCs"] = ["PKZ041","PKZ042","PKZ043","PKZ051",
                                       "PKZ052"]

elif "<site>" == "GUM":
    if "_<MultiPil>" == "_MY":
        Definition["subDomainUGCs"] = ["PMZ151","PMZ152","PMZ153","PMZ154"]
    elif "_<MultiPil>" == "_PQ":
        Definition["subDomainUGCs"] = ["PMZ161","PMZ171","PMZ172","PMZ173",
                                       "PMZ174","PMZ181","PMZ191"]

