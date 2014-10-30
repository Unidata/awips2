import CatalogQuery

query = CatalogQuery.CatalogQuery("satellite")
query.addConstraint("creatingEntity","EEEEE","=")
query.addConstraint("sectorID","SSSSS","=")
query.addConstraint("physicalElement","CCCCC","=")
query.setDistinctField("dataTime")

return query.executeWrapped()
