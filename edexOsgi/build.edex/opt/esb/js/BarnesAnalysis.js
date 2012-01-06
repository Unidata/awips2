/**
 * Performs Barnes Scheme Analysis on a single grid. Both the grid parameter
 * to retrieve and the observation parameter used to modify it are set using
 * property setters. Following the analysis, the result is returned as a
 * displayable image.
 *
 * This script represents a complete product generation. As such, it is not
 * intended to be called from other scripts.
 *
 * Implementation note:
 *  Barnes Analysis requires two queries, one for the grid and one for the
 *  observations used to modify the grid. For this reason, the query property
 *  setters, addParameter, addList, setCount and setSortValue, have a an
 *  initial, boolean parameter. Setting this parameter 'true' performs the
 *  action on the grid query; setting it 'false' performs the query on the
 *  observation query.
 *
 * Usage:
 *  This is an example of a script that retrieves and corrects a temperature
 *  grid:
 *    include("BarnesAnalysis.js");
 *    var barnes = new BarnesAnalysis();
 *    barnes.addParameter(true,"paramid","Temperature");
 *    barnes.addParameter(true,"levelinfo","2.0_m");
 *    barnes.addParameter(true,"forecasttime","0");
 *    barnes.addParameter(true,"gridid",212);
 *    barnes.setCount(true,1);
 *    barnes.setSortValue(true,"basetime");
 *    barnes.setSpatialBounds(43.00, -98.00, 37.00, -92.00);
 *    barnes.addParameter(false,"refhour","20070601190000");
 *    barnes.setCount(false,0);
 *    barnes.setSortValue(false,"timeobs");
 *    barnes.setObParameter("temperature");
 *    barnes.setBarnesParameters("50000.0","0.50","1","1");
 *    barnes.setColorMap("GribTempRGB");
 *    barnes.setFormat("png");
 *    barnes.setScaleFactor(3.0);
 *    barnes.execute();
 */

/**
 * Class constructor.
 */
function BarnesAnalysis() { 
  /* names of constants */
  this.grib = "grib";
  this.obs = "obs";
  this.icao = "icao";
  this.geometry = "geometry";
  /* settings for Barnes Analysis */
  this.obField = "temperature";
  this.radius = "50000.0";
  this.weight = "0.50";
  this.stations = "1";
  this.passes = "1";
  /* settings for image creation */
  this.reproject = false;
  this.colormap = "StopLight";
  this.format = "png";
  this.scaleFactor = 3.0;
  /* the queries */
  this.gridquery = new TermQuery(this.grib);
  this.obsquery = new TermQuery(this.obs);
  this.spatial = new SpatialQuery();
}

/**
 * Main action method. Performs the the Barnes Analysis and creates the image.
 *
 * @return (String) XML string containing the results.
 */
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
  var mapObs = new MapAsciiData(this.obField,
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
  analyzer.addParameter("searchRadius",this.radius);
  analyzer.addParameter("weight",this.weight);
  analyzer.addParameter("minNoStns",this.stations);
  analyzer.addParameter("numPasses",this.passes);
  analyzer.addParameter("extrapolate","true");
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
function _setObParameter(param) {
  this.obField = param;
}
function _setBarnesParameters(radius,weight,stations,passes) {
  this.radius = radius;
  this.weight = weight;
  this.stations = stations;
  this.passes = passes;
}

/* map the functions to the class prototype */
BarnesAnalysis.prototype.execute = _execute;
BarnesAnalysis.prototype.addParameter = _addParameter;
BarnesAnalysis.prototype.addList = _addList;
BarnesAnalysis.prototype.setSpatialBounds = _setSpatialBounds;
BarnesAnalysis.prototype.setScaleFactor = _setScaleFactor;
BarnesAnalysis.prototype.setCount = _setCount;
BarnesAnalysis.prototype.reprojectImage = _reprojectImage;
BarnesAnalysis.prototype.setColorMap = _setColormap;
BarnesAnalysis.prototype.setFormat = _setFormat;
BarnesAnalysis.prototype.setSortValue = _setSortValue;
BarnesAnalysis.prototype.setBarnesParameters = _setBarnesParameters;
BarnesAnalysis.prototype.setObParameter = _setObParameter;

