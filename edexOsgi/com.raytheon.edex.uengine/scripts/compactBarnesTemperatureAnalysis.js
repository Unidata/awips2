/* the compact action script */
include("BarnesAnalysis.js");
barnes = new BarnesAnalysis("temp");
/* set the grib search parameters */
barnes.addParameter(true,"paramid","Temperature");
barnes.addParameter(true,"levelinfo","2.0_m");
barnes.addParameter(true,"forecasttime","0");
barnes.addParameter(true,"gridid",212);
barnes.setCount(true,1);
barnes.setSortValue(true,"basetime");
/* set the lat/lon bounds*/
barnes.setSpatialBounds(43.00, -98.00, 37.00, -92.00);
/* set the metar search parameters */
barnes.addParameter(false,"refhour","20070601190000");
barnes.setCount(false,0);
barnes.setSortValue(false,"timeobs");
/* set analysis parameters */
barnes.setObParameter("temperature");
barnes.setBarnesParameters("50000.0","0.50","1","1");
/* set image properties */
barnes.setColorMap("GribTempRGB");
barnes.setFormat("png");
barnes.setScaleFactor(3.0);
/* execute the query */
barnes.execute();

