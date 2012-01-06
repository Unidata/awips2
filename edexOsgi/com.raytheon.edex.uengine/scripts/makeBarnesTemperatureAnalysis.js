/* constructor */
function BarnesTemperatureAnalysis() { 
  /* names of constants */
  this.grib = "grib";
  this.icao = "icao";
  this.geometry = "geometry";
  /* settings for Barnes Analysis */
  this.radius = 50000.0;
  this.weight = 0.50;
  this.stations = 1;
  this.passes = 1;
  /* settings for image creation */
  this.reproject = false;
  this.colormap = "StopLight";
  this.format = "png";
  this.scaleFactor = 3.0;
  /* the queries */
  this.gridquery = new TermQuery(this.grib);
  this.obsquery = new TermQuery("obs");
  this.spatial = new SpatialQuery();
}

/* the execute function */
function _execute() {
  var response;
  /* get the grib record */
  var gridResults = this.gridquery.execute();
  if(gridResults == null || gridResults.size() == 0)
  {
    response = new MakeResponseNull("Query for GRIB returned no results.",this.gridquery);
    return response.execute();
  }
  var grid = gridResults.get(0);
  /* get the spatial information */
  this.spatial.addField(this.icao);
  this.spatial.addField(this.geometry);
  var spatialResults = this.spatial.execute();
  if (spatialResults == null || spatialResults.size() == 0) {
    response = new MakeResponseNull("Spatial query returned no results.",this.spatial);
    return response.execute();
  }
  /* get the and geolocate the temperature observations */
  this.addList(false,"stationid",MEUtils.changeArrayListToString(spatialResults.get(this.icao)));
  var obsResults = this.obsquery.execute();
  if (obsResults == null || obsResults.size() == 0) {
    response = new MakeResponseNull("Ob query returned no results.",this.obsquery);
    return response.execute();
  }
  var mapObs = new MapAsciiData("temperature",
                                obsResults,
                                spatialResults.get(this.icao),
                                spatialResults.get(this.geometry));
  /* get the GRIB record and perform the analysis */
  var fileIn = new FileIn(this.grib, grid);
  var geom = grid.getGrid().getGridGeom();
  var crs = grid.getGrid().getCrs();
  var gribRecord = fileIn.execute();
  var analyzer = new ObjectiveAnalysis(gribRecord,
                                       geom,
                                       crs,
                                       mapObs.execute());
  analyzer.addParameter("searchRadius","50000.0");
  analyzer.addParameter("weight","0.50");
  analyzer.addParameter("minNoStns","1");
  analyzer.addParameter("extrapolate","true");
  analyzer.addParameter("numPasses","1");
  var analyzed = analyzer.execute();
  
  /* create the derived image */
  var gribMap = new GribMap(this.grib, this.colormap, analyzed, geom);
  gribMap.setScaleFactor(this.scaleFactor);
  var imageData = gribMap.execute();
  geom = gribMap.getGridGeometry();
  var colorMap = new ColorMapImage(this.colormap, imageData, geom);
  var imageOut = null;
  if(this.reproject){
    var reproject = new ReprojectImage(colorMap.execute(), geom, crs);
    var reprojectedImage = reproject.execute();
    imageOut = new ImageOut(reprojectedImage, this.format, reproject.getGridGeometry());
  }
  else
  {
    imageOut = new ImageOut(colorMap.execute(), this.format,geom);
  }
  var fileOut = new FileOut(imageOut.execute(), this.format);
  var writeFile = fileOut.execute();
  var makeResponse = new MakeResponseUri(writeFile, 
                                         null, 
                                         grid.getDataURI(), 
                                         this.format);
  return makeResponse.execute();
  
}
/* query related setter functions */
function _addParameter(grib, name, value) {
  var query = (grib)?this.gridquery:this.obsquery;
  query.addParameter(name, value);
}

function _addList(grib, name, value) {
  var query = (grib)?this.gridquery:this.obsquery;
  query.addParameter(name, value, "in");
}

function _setCount(grib, count){
  var query = (grib)?this.gridquery:this.obsquery;
  query.setCount(count);
}

function _setSortValue(grib, sortValue){
  var query = (grib)?this.gridquery:this.obsquery;
  query.setSortBy(sortValue);
}

/* image related setter functions */
function _setScaleFactor(scale){
  this.scaleFactor = scale;
}

function _reprojectImage(reproject){
  this.reproject = reproject;
}

function _setColormap(colormap){
  this.colormap = colormap;
}

function _setFormat(format){
  this.format = format;
}

/* setters for the spatial query */
function _setSpatialBounds(ulLat, ulLon, lrLat, lrLon) {
  this.spatial.setUpperLeftLat(ulLat);
  this.spatial.setUpperLeftLon(ulLon);
  this.spatial.setLowerRightLat(lrLat);
  this.spatial.setLowerRightLon(lrLon);
}

/* setters for Barnes Analysis */
function _setBarnesParameters(radius,weight,stations,passes) {
  this.radius = radius;
  this.weight = weight;
  this.stations = stations;
  this.passes = passes;
}

/* mapping functions to the object */
BarnesTemperatureAnalysis.prototype.execute = _execute;
BarnesTemperatureAnalysis.prototype.addParameter = _addParameter;
BarnesTemperatureAnalysis.prototype.addList = _addList;
BarnesTemperatureAnalysis.prototype.setSpatialBounds = _setSpatialBounds;
BarnesTemperatureAnalysis.prototype.setScaleFactor = _setScaleFactor;
BarnesTemperatureAnalysis.prototype.setCount = _setCount;
BarnesTemperatureAnalysis.prototype.reprojectImage = _reprojectImage;
BarnesTemperatureAnalysis.prototype.setColorMap = _setColormap;
BarnesTemperatureAnalysis.prototype.setFormat = _setFormat;
BarnesTemperatureAnalysis.prototype.setSortValue = _setSortValue;
BarnesTemperatureAnalysis.prototype.setBarnesParameters = _setBarnesParameters;

/* the compact action script */
var barnes = new BarnesTemperatureAnalysis();
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
barnes.setBarnesParameters(50000.0,0.50,1,1);
/* set image properties */
barnes.setColorMap("GribTempRGB");
barnes.setFormat("png");
barnes.setScaleFactor(3.0);
/* execute the query */
barnes.execute();

