include("GribWinds.js");
include("GribImage.js");
include("ObsSpatial.js");
/* Define the class to perform the analysis */
function BarnesAnalysisWinds() {
  /* names of things */
  this.grib = "grib";
  this.obs = "obs";
  this.icao = "icao";
  this.geometry = "geometry";
  this.obParam = "windSpeed";
  /* settings for Barnes Analysis */
  this.radius = "50000.0";
  this.weight = "0.50";
  this.stations = "1";
  this.passes = "1";
  /* query objects */
  this.gQuery = new GribWinds(this.grib,true);
  this.oQuery = new ObsSpatialQuery(this.obs,this.obParam);
  /* the image creator */
  this.iMaker = new GribImage(this.grib);
  /* the logger */
  this.logger = new SystemLog();
}

function _execute() {
  var response;
  /* get the wind speed as a grib */
  var wResult = this.gQuery.execute();
  if (wResult.getClass().getSimpleName() != "FloatDataRecord") {
    this.logger.log("warn","GRIB Winds creation failed.");
    return wResult;
  }
  this.logger.log("info","GRIB Winds creation successful.");
  var geom = this.gQuery.getGeom();
  var crs = this.gQuery.getCRS();

  /* get and geolocate the wind speed observations */
  var oResult = this.oQuery.execute();
  
  if(oResult.getClass().getSimpleName() != "ArrayList") {
    this.logger.log("warn","Spatial ob query for " + this.obParam + " failed.");
    return oResult;
  }
  this.logger.log("info","Spatial ob query for " + this.obParam + " successful.");

  /* setup the analyzer and perform the analysis */
  var analyzer = new ObjectiveAnalysis(wResult,
                                       geom,
                                       crs,
                                       oResult);
  analyzer.addParameter("searchRadius",this.radius);
  analyzer.addParameter("weight",this.weight);
  analyzer.addParameter("minNoStns",this.stations);
  analyzer.addParameter("numPasses",this.passes);
  analyzer.addParameter("extrapolate","true");
  var analyzed = analyzer.execute();
  
  /* create the derived image */
  this.iMaker.setGrid(analyzed);
  this.iMaker.setCrs(crs);
  this.iMaker.setGeom(geom);
  var imageOut = this.iMaker.execute();
  geom = this.iMaker.getGeom();
  var format = this.iMaker.getFormat();
  
  /* write the image to the file and return the response */
  var fileOut = new FileOut(imageOut, format);
  var writeFile = fileOut.execute();
  var makeResponse = new MakeResponseUri(writeFile, 
                                         null, 
                                         this.gQuery.getDataURI(), 
                                         this.format);
  return makeResponse.execute();  
}

/* helper methods */
function _makeError(message,query) {
   var response = new MakeResponseNull(message,query);
   return response.execute();
}

/* setters for query objects */
function _addParameter(grid,name,value) {
  var query = (grid)?this.gQuery:this.oQuery;
  query.addParameter(name,value);
}
function _addList(grid,name,value) {
  var query = (grid)?this.gQuery:this.oQuery;
  query.addParameter(name,value,"in");
}
function _setSortValue(grid,sortValue) {
  var query = (grid)?this.gQuery:this.oQuery;
  query.setSortby(sortValue);
}
function _setCount(grid,count) {
  var query = (grid)?this.gQuery:this.oQuery;
  query.setCount(count);
}
/* setters for image creation parameters */
function _setScaleFactor(scale) {
  this.iMaker.setScaleFactor(scale);
}

function _reprojectImage(reproject) {
  this.iMaker.setReproject(reproject);
}

function _setColorMap(colormap){
  this.iMaker.setColormap(colormap);
}

function _setFormat(format){
  this.iMaker.setFormat(format);
}

/* setters for the spatial query */
function _setSpatialBounds(ulLat, ulLon, lrLat, lrLon) {
  this.oQuery.setSpatialBounds(ulLat, 
                               ulLon,
                               lrLat,
                               lrLon,
                               this.icao,
                               this.geometry);
}

/* setters for Barnes Analysis */
function _setBarnesParameters(radius,weight,stations,passes) {
  this.radius = radius;
  this.weight = weight;
  this.stations = stations;
  this.passes = passes;
}

/* mapping functions to the object */
BarnesAnalysisWinds.prototype.execute = _execute;
BarnesAnalysisWinds.prototype.addParameter = _addParameter;
BarnesAnalysisWinds.prototype.addList = _addList;
BarnesAnalysisWinds.prototype.setSortValue = _setSortValue;
BarnesAnalysisWinds.prototype.setCount = _setCount;
BarnesAnalysisWinds.prototype.setScaleFactor = _setScaleFactor;
BarnesAnalysisWinds.prototype.reprojectImage = _reprojectImage;
BarnesAnalysisWinds.prototype.setColorMap = _setColorMap;
BarnesAnalysisWinds.prototype.setFormat = _setFormat;
BarnesAnalysisWinds.prototype.setSpatialBounds = _setSpatialBounds;
BarnesAnalysisWinds.prototype.setBarnesParameters = _setBarnesParameters;
BarnesAnalysisWinds.prototype.makeError = _makeError;

var runner = new BarnesAnalysisWinds();
/* setup the basic grib queries */
runner.addParameter(true,"levelinfo","10.0_m");
runner.addParameter(true,"forecasttime","0");
runner.addParameter(true,"gridid",212);
runner.setSortValue(true,"basetime");
runner.setCount(true,1);
/* setup the basic obs query */
runner.addParameter(false,"refhour","20070601190000");
runner.setSortValue(false,"timeobs");
runner.setCount(false,0);
/* setup the spatial search parameters */
runner.setSpatialBounds(43.00, -98.00, 37.00, -92.00);
/* setup the Barnes analysis paramters */
runner.setBarnesParameters("50000.0","0.50","1","1");
/* set image properties */
runner.setColorMap("GribRGB");
runner.setFormat("png");
runner.setScaleFactor(3.0);
/* execute the script */
runner.execute();
