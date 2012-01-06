include("GribWinds.js");
include("GribImage.js");

/* Define the class to perform the analysis */
function WindSpeedImage() {
  /* names of things */
  this.grib = "grib";
  this.obs = "obs";
  this.icao = "icao";
  this.geometry = "geometry";
  this.obParam = "windSpeed";
  /* query objects */
  this.gQuery = new GribWinds(this.grib,true);
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

  /* create the derived image */
  this.iMaker.setGrid(wResult);
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
function _addParameter(name,value) {
  this.gQuery.addParameter(name,value);
}
function _addList(name,value) {
  this.gQuery.addParameter(name,value,"in");
}
function _setSortValue(sortValue) {
  this.gQuery.setSortby(sortValue);
}
function _setCount(count) {
  this.gQuery.setCount(count);
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

/* mapping functions to the object */
WindSpeedImage.prototype.execute = _execute;
WindSpeedImage.prototype.addParameter = _addParameter;
WindSpeedImage.prototype.addList = _addList;
WindSpeedImage.prototype.setSortValue = _setSortValue;
WindSpeedImage.prototype.setCount = _setCount;
WindSpeedImage.prototype.setScaleFactor = _setScaleFactor;
WindSpeedImage.prototype.reprojectImage = _reprojectImage;
WindSpeedImage.prototype.setColorMap = _setColorMap;
WindSpeedImage.prototype.setFormat = _setFormat;
WindSpeedImage.prototype.makeError = _makeError;

var runner = new WindSpeedImage();
/* setup the basic grib queries */
runner.addParameter("levelinfo","10.0_m");
runner.addParameter("forecasttime","0");
runner.addParameter("gridid",212);
runner.setSortValue("basetime");
runner.setCount(1);
/* set image properties */
runner.setColorMap("GribRGB");
runner.setFormat("png");
runner.setScaleFactor(3.0);
/* execute the script */
runner.execute();
