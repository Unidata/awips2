##
##
########################################################################
# MultipleElementTable_Local (tp008_Local)
#
#   Type: smart
#   Local product:
#     MultipleElementTable_Local(type: smart)
#   To customize this product for your site:
#      Set up MultipleElementTable_Local (see template below)
#      to override variables, definitions, thresholds, and methods
##
##########################################################################
import MultipleElementTable
import string, time, re, os, types, copy

class TextProduct(MultipleElementTable.TextProduct):
    Definition = copy.deepcopy(MultipleElementTable.TextProduct.Definition)

    Definition["displayName"] = "tp008"
    Definition["outputFile"] = "/aaaa"
    Definition["database"] = "Official"
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
    #Definition["singleValueFormat"] = 1 # Default is 0
    #Definition["introLetters"] = ".<"

    def __init__(self):
        MultipleElementTable.TextProduct.__init__(self)

