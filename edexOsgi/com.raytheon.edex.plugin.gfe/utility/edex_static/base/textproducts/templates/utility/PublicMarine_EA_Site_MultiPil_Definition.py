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

