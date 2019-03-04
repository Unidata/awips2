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
# PublicMarine_EA_<site>_<MultiPil>_Definition.TextUtility
#
#  This file is used to generate a combinations file that contains
#  all public and marine codes for the given site.  This file is similar to 
#  a product definition file, but only contains what is needed for 
#  the installation to generate a combinations file.
#
# ---------------------------------------------------------------------

Definition = {}

#----- WFO <site> Public and Marine _EditAreas Definition -----

#Name of the generated combinations file
Definition["defaultEditAreas"] = "EditAreas_PublicMarine_<site>_<MultiPil>"

#MapBackground for creating the combinations file
Definition["mapNameForCombinations"] = ["Zones_<site>","Marine_Zones_<site>"]

