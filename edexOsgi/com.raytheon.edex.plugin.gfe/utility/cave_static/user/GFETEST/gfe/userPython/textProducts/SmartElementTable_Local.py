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
########################################################################
# SmartElementTable_Local
#
#   Type: smart
#   Local product:
#     SmartElementTable_Local(type: smart)
#   To customize this product for your site:
#      Set up SmartElementTable_Local (see template below)
#      to override variables, definitions, thresholds, and methods
##
##########################################################################
import SmartElementTable
import string, time, re, os, types, copy

class TextProduct(SmartElementTable.TextProduct):
    Definition = copy.deepcopy(SmartElementTable.TextProduct.Definition)

    Definition["displayName"] = "TEST_SmartElementTable"
    #Definition["outputFile"] = "/awips/GFESuite/products/TEXT/SmartElementTable.txt"
    #Definition["defaultEditAreas"] = [
    #        ("area1","AREA 1"),
    #        ("area2","AREA 2"),
    #        ("area3","AREA 3"),
    #        ]
    #Definition["regionList"] = [
    #        ("/33",["AREA 1","AREA 2"]),
    #        ("/19",["AREA 3"])
    #        ]

    #Definition["elementList"] = ["Temp", "PoP"] # Default
    #Definition["elementList"] = ["Temp", "Humidity"]
    #Definition["elementList"] = ["Temp", "Humidity", "PoP"]
    #Definition["elementList"] = ["Temp", "PoP", "Humidity"]
    #Definition["elementList"] = ["PoP", "Humidity", "Temp"]
    #Definition["introLetters"] = ".<"

    def __init__(self):
        SmartElementTable.TextProduct.__init__(self)

 
