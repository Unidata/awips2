/**
 * This script creates a stop light (green/yellow/red) decision chart from a
 * grid product. The chart created is derived from the Wind-U and Wind-V grib
 * parameters and represent either wind speed or direction. The othe parameters
 * for the grid retrieval are set via the property setter. The color map is user
 * selectable, but should be set to "StopLight".
 *
 * This script represents a complete product generation. As such, it is not
 * intended to be called from other scripts.
 *
 * Implementation Note:
 *  The wind mode property (this.windSpeed), which is set using the setWindMode()
 *  method, determines is wind speed or wind direction is computer. It is boolean;
 *  'true' indicates wind speed.
 *
 * Usage:
 *  This script creates a wind speed based stop light chart.
 *    include("WindStopLight.js");
 *    var dataRequest = new WindStopLight();
 *    dataRequest.setLevelInfo("10.0_m");
 *    dataRequest.setScaleFactor(3.0);
 *    dataRequest.setFcstHour("0");
 *    dataRequest.setGridId(212);
 *    dataRequest.setCount(1);
 *    dataRequest.setColormap("StopLight");
 *    dataRequest.setColors(0.0,1.0,2.0);
 *    dataRequest.setSortValue("basetime");
 *    dataRequest.setFormat("png");
 *    dataRequest.reprojectImage(false);
 *    dataRequest.setWindMode(true);
 *    dataRequest.execute();
 */

/**
 * Class constructor.
 */
function WindStopLight() {
  this.plugin = "grib";
  this.format = "png";
  this.colormap = "StopLight";
  this.scaleFactor = 3.0;
  this.count = 1;
  this.levelInfo = "10.0_m";
  this.fcstHour = "0";
  this.gridId = "212";
  this.reproject = false;
  this.sortValue = "basetime";
  this.green = 0.0;
  this.yellow = 1.0;
  this.red = 2.0;
  this.windSpeed = true;
}

/**
 * Main action method. Creates the stop light chart.
 *
 * @return (String) XML representing the final product
 */
function _execute() {
  var uQuery = new TermQuery(this.plugin);
  uQuery.addParameter("paramid","U%wind","like");
  uQuery.addParameter("levelinfo",this.levelInfo);
  uQuery.addParameter("forecasttime",this.fcstHour);
  uQuery.addParameter("gridid",this.gridId);
  uQuery.setSortBy(this.sortValue);
  uQuery.setCount(this.count);
  var uQueryResults = uQuery.execute();
  if(uQueryResults == null || uQueryResults.size() == 0)
    {
        response = new MakeResponseNull("Query for Wind-U returned 0 results.",uQuery);
        return response.execute();
    }
  
  var vQuery = new TermQuery(this.plugin);
  vQuery.addParameter("paramid","V%wind","like");
  vQuery.addParameter("levelinfo",this.levelInfo);
  vQuery.addParameter("forecasttime",this.fcstHour);
  vQuery.addParameter("gridid",this.gridId);
  vQuery.setSortBy(this.sortValue);
  vQuery.setCount(this.count);
  var vQueryResults = vQuery.execute();
  if(vQueryResults == null || vQueryResults.size() == 0)
  {
    response = new MakeResponseNull("Query for Wind-V returned 0 results.",uQuery);
    return response.execute();
  }

  /* read the data from the data store */
  var geom = uQueryResults.get(0).getGrid().getGridGeom();
  var crs = uQueryResults.get(0).getGrid().getCrs();
  var uFile = new FileIn(this.plugin,uQueryResults.get(0));
  var vFile = new FileIn(this.plugin,vQueryResults.get(0));
  var uData = uFile.execute();
  var vData = vFile.execute();

  /* combine the data into wind speed data */
  var windSpeed = new ConvertWindsData(uData,vData,this.windSpeed);
  /* create the stoplight chart */
  var stopLight = new StopLightImage(windSpeed.execute());
  var gribMap = new GribMap(this.plugin, this.colormap, stopLight.execute(), geom);
  gribMap.setScaleFactor(this.scaleFactor);
  var imageData = gribMap.execute();
  geom = gribMap.getGridGeometry();
  /* create the output image */
  var colorMap = new ColorMapImage(this.colormap, imageData, geom);
  var imageOut = null;
  if(this.reproject){
    var reproject = new ReprojectImage(colorMap.execute(), geom, crs);
    var reprojectedImage = reproject.execute();
    imageOut = new ImageOut(reprojectedImage, this.format, reproject.getGridGeometry());
  }
  else
  {
    imageOut = new ImageOut(colorMap.execute(), this.format);
  }
  /* write the image to disk and return the response info to the client */
  var fileOut = new FileOut(imageOut.execute(), this.format);
  var writeFile = fileOut.execute();
  var makeResponse = new MakeResponseUri(writeFile, null, null, this.format);
  return makeResponse.execute();
}

/* setter methods */
function _setScaleFactor(factor) {
  this.scaleFactor = factor;
}
function _setCount(count){
  this.count = count;
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

function _setLevelInfo(levelInfo) {
  this.levelInfo = levelInfo;
}

function _setFcstHour(fcstHour) {
  this.fcstHour = fcstHour;
}
function _setGridId(gridId) {
  this.gridId = gridId;
}

function _setSortValue(sortValue) {
  this.sortValue = sortValue;
}

function _setWindMode(speed) {
  this.windSpeed = speed;
}

function _setColors(green,yellow,red) {
  this.green = green;
  this.yellow = yellow;
  this.red = red;
}
 
/* add the methods to the SatelliteRequest Object. */
WindStopLight.prototype.execute = _execute;
WindStopLight.prototype.setScaleFactor = _setScaleFactor;
WindStopLight.prototype.setCount = _setCount;
WindStopLight.prototype.reprojectImage = _reprojectImage;
WindStopLight.prototype.setColormap = _setColormap;
WindStopLight.prototype.setFormat = _setFormat;
WindStopLight.prototype.setLevelInfo = _setLevelInfo;
WindStopLight.prototype.setFcstHour = _setFcstHour;
WindStopLight.prototype.setGridId = _setGridId;
WindStopLight.prototype.setSortValue = _setSortValue;
WindStopLight.prototype.setColors = _setColors;
WindStopLight.prototype.setWindMode = _setWindMode;

