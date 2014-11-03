import CatalogQuery

query = CatalogQuery.CatalogQuery("radar")
query.addConstraint("icao","KKKK","=")
query.addConstraint("productCode","MMMM","in") 
query.addConstraint("primaryElevationAngle","EEEE","=")
query.addConstraint("dataTime","AAAAA",">=")
query.addConstraint("dataTime","BBBBB","<=")

query.setDistinctField("icao")
query.setDistinctField("dataTime")
query.setDistinctField("trueElevationAngle")
query.setDistinctField("productCode")

return query.executeWrapped()
