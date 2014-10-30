# Get inventories

import CatalogQuery
query = CatalogQuery.CatalogQuery("grid")
mode = "MMMMM"

if mode == "field" :
   query.setDistinctField("info.parameter.abbreviation")
elif mode == "plane" :
   query.setDistinctField("info.level.masterLevel.name")
   query.setDistinctField("info.level.levelonevalue")
   query.setDistinctField("info.level.leveltwovalue")
else :
    query.setDistinctField("dataTime")

query.addConstraint("info.datasetId", "SSSSS")
query.addConstraint("info.level.masterLevel.name", "TTTTT")
query.addConstraint("info.level.levelonevalue", "LLLLL")
query.addConstraint("info.level.leveltwovalue", "22222")
query.addConstraint("info.parameter.abbreviation", "VVVVV")

return query.executeWrapped()
