# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# CoastalZoneDefinition
#
# Author: lefebvre
# ----------------------------------------------------------------------------

# This is a simple module to store the list of public zones that make up the
# "Coastal" edit area. To update the "WindWWEditAreaCoastalZones" edit area:
#
#    1. Add or remove zones from the list below and save the file. 
#    2. From the Localization perpective, delete the edit area by selecting:
#       GFE->Edit Areas->WindWWEditAreaCoastalZones. Use the right button to
#       delete this edit area file.
#    3. Run RecommendWindWW and the edit area will be re-generated from the updated
#       list of zones (below).
#    4. When finished, promote the edit area to "Site" using the same Localization
#       perspective path and select "Move To->Site. This will ensure that all
#       forecasters can use this updated edit area.

CoastalZoneList = [
                   "ALZ261", "ALZ262", "ALZ263", "ALZ264", "ALZ265", "ALZ266", "CTZ004", "CTZ005", "CTZ006", 
                   "CTZ007", "CTZ008", "CTZ009", "CTZ010", "CTZ011", "CTZ012", "DCZ001", "DEZ001", "DEZ002", "DEZ003", 
                   "DEZ004", "FLZ007", "FLZ008", "FLZ009", "FLZ010", "FLZ011", "FLZ012", "FLZ013", "FLZ014", 
                   "FLZ015", "FLZ016", "FLZ017", "FLZ018", "FLZ019", "FLZ021", "FLZ023", "FLZ024", 
                   "FLZ025", "FLZ026", "FLZ027", "FLZ028", "FLZ029", "FLZ030", "FLZ031", "FLZ032", "FLZ033", 
                   "FLZ034", "FLZ035", "FLZ037", "FLZ038", "FLZ041", "FLZ043", "FLZ044", 
                   "FLZ045", "FLZ046", "FLZ047", "FLZ050", "FLZ052", "FLZ053", "FLZ054", "FLZ056", "FLZ057", 
                   "FLZ058", "FLZ059", "FLZ061", "FLZ063", "FLZ064", "FLZ066", "FLZ067", "FLZ068", "FLZ069", 
                   "FLZ070", "FLZ071", "FLZ072", "FLZ073", "FLZ074", "FLZ075", "FLZ108", "FLZ112", "FLZ114", 
                   "FLZ115", "FLZ118", "FLZ122", "FLZ124", "FLZ125", "FLZ127", "FLZ128", "FLZ133", "FLZ134", "FLZ136", "FLZ138", 
                   "FLZ139", "FLZ140", "FLZ141", "FLZ142", "FLZ144", "FLZ147", "FLZ148", "FLZ149", "FLZ151", "FLZ155", 
                   "FLZ160", "FLZ162", "FLZ165", "FLZ168", "FLZ172", "FLZ173", "FLZ174", "FLZ201", "FLZ202", 
                   "FLZ203", "FLZ204", "FLZ205", "FLZ206", "FLZ222", "FLZ236", "FLZ239", "FLZ240", "FLZ242", "FLZ248", "FLZ249", "FLZ251", 
                   "FLZ255", "FLZ260", "FLZ262", "FLZ265", "FLZ340", "GAZ100", "GAZ101", "GAZ114", "GAZ115", "GAZ116", 
                   "GAZ117", "GAZ118", "GAZ119", "GAZ136", "GAZ137", "GAZ138", "GAZ139", "GAZ140", "GAZ141", 
                   "GAZ152", "GAZ153", "GAZ154", "GAZ165", "GAZ166", "GAZ264", "GAZ364",
                   "LAZ038", "LAZ040", "LAZ041", "LAZ042", "LAZ043", "LAZ044", "LAZ045", 
                   "LAZ046", "LAZ049", "LAZ050", "LAZ052", "LAZ053", "LAZ054", "LAZ055", 
                   "LAZ056", "LAZ057", "LAZ058", "LAZ059", "LAZ060", "LAZ061", "LAZ062", "LAZ063", "LAZ064", 
                   "LAZ065", "LAZ066", "LAZ067", "LAZ068", "LAZ069", "LAZ070", "LAZ072", "LAZ073", 
                   "LAZ074", "MAZ005", "MAZ006", "MAZ007", "MAZ013", "MAZ014", "MAZ015", "MAZ016", 
                   "MAZ017", "MAZ018", "MAZ019", "MAZ020", "MAZ021", "MAZ022", "MAZ023", "MAZ024", "MDZ006", 
                   "MDZ008", "MDZ011", "MDZ012", "MDZ013", "MDZ014", "MDZ015", "MDZ016", "MDZ017", "MDZ018", 
                   "MDZ019", "MDZ020", "MDZ021", "MDZ022", "MDZ023", "MDZ024", "MDZ025", "MDZ504", "MDZ506", 
                   "MDZ507", "MDZ508", "MEZ016", "MEZ017", "MEZ018", "MEZ019", "MEZ020", 
                   "MEZ021", "MEZ022", "MEZ023", "MEZ024", "MEZ025", "MEZ026", "MEZ027", "MEZ028", "MEZ029", 
                   "MEZ030", "MSZ077", "MSZ078", "MSZ079", "MSZ080", "MSZ081", "MSZ082", "NCZ013", "NCZ014", 
                   "NCZ015", "NCZ016", "NCZ017", "NCZ029", "NCZ030", "NCZ031", "NCZ032", "NCZ044", "NCZ045", 
                   "NCZ046", "NCZ047", "NCZ080", "NCZ081", "NCZ090", "NCZ091", "NCZ092", "NCZ093", "NCZ094", 
                   "NCZ095", "NCZ096", "NCZ098", "NCZ099", "NCZ102", "NCZ103", "NCZ104", "NCZ105", "NCZ106", 
                   "NCZ107", "NCZ108", "NCZ109", "NCZ110", "NHZ010", "NHZ013", "NHZ014", 
                   "NJZ004", "NJZ006", "NJZ010", "NJZ012", "NJZ013", "NJZ014", "NJZ015", "NJZ016", 
                   "NJZ017", "NJZ018", "NJZ019", "NJZ020", "NJZ021", "NJZ022", "NJZ023", "NJZ024", "NJZ025", 
                   "NJZ026", "NJZ027", "NJZ103", "NJZ104", "NJZ105", "NJZ106", "NJZ107", "NJZ108", 
                   "NYZ069", "NYZ070", "NYZ071", "NYZ072", "NYZ073", "NYZ074", "NYZ075", "NYZ078", "NYZ079", 
                   "NYZ080", "NYZ081", "NYZ176", "NYZ177", "NYZ178", "NYZ179", "PAZ070", "PAZ071", "PAZ102", 
                   "PAZ106", "PRZ001", "PRZ002", "PRZ003", "PRZ004", "PRZ005", "PRZ006", "PRZ007", "PRZ008", 
                   "PRZ009", "PRZ010", "PRZ011", "PRZ012", "PRZ013", "RIZ001", "RIZ002", "RIZ003", "RIZ004", 
                   "RIZ005", "RIZ006", "RIZ007", "RIZ008", "SCZ032", "SCZ033", "SCZ039", "SCZ042", "SCZ043", 
                   "SCZ044", "SCZ045", "SCZ047", "SCZ048", "SCZ049", "SCZ050", "SCZ051", "SCZ052", "SCZ053", 
                   "SCZ054", "SCZ055", "SCZ056", "TXZ200", "TXZ201", "TXZ213", "TXZ214", "TXZ215", 
                   "TXZ216", "TXZ226", "TXZ227", "TXZ232", "TXZ233", "TXZ234", "TXZ235", "TXZ236", "TXZ237", "TXZ238", 
                   "TXZ242", "TXZ243", "TXZ244", "TXZ245", "TXZ246", "TXZ247", "TXZ251", "TXZ254", 
                   "TXZ255", "TXZ256", "TXZ257", "TXZ262", "TXZ300", "TXZ313", "TXZ335", "TXZ336", "TXZ337", 
                   "TXZ338", "TXZ342", "TXZ343", "TXZ344", "TXZ345", "TXZ346", "TXZ347", "TXZ351", "TXZ436", 
                   "TXZ437", "TXZ438", "TXZ442", "TXZ443", "TXZ447", "VAZ052", "VAZ053", "VAZ054", 
                   "VAZ055", "VAZ057", "VAZ064", "VAZ072", "VAZ073", "VAZ075", "VAZ076", "VAZ077", "VAZ078", 
                   "VAZ082", "VAZ083", "VAZ084", "VAZ085", "VAZ086", "VAZ089", "VAZ090", "VAZ091", "VAZ093", 
                   "VAZ094", "VAZ095", "VAZ096", "VAZ097", "VAZ098", "VAZ099", "VAZ100", "VAZ517", "VAZ518", 
                   "VAZ519", "VAZ520", "VAZ521", "VAZ522", "VAZ523", "VAZ524", "VAZ525", "VIZ001", "VIZ002",
                   ]


