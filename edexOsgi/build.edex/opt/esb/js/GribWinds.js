/**
 * Computes a derived winds grid based on combining the wind-U
 * and wind-V parameters for a specific GRIB parameter.
 *
 * Performs a pair of queries, one for wind-U and one for wind-V,
 * based on a single set of criteria. A single instance will create
 * either a wind speed or wind direction derived grid product.
 *
 * This class may be imported into a JS uEngine script needing to
 * compute a derived winds grid.
 * 
 * Usage:
 *   include("GribWinds.js");
 *   var gQuery = new GribWinds("grib",true);
 *   gQuery.addParameter("levelinfo","10.0_m");
 *   gQuery.addParameter("forecasttime","0");
 *   gQuery.addParameter("gridid",212);
 *   gQuery.setSortby("basetime");
 *   gQuery.setCount(1);
 *   var gResult = gQuery.execute();
 */
 
/**
 * Class constructor.
 *
 * @param plugin (String) identifies the data-type plugin - normally "grib"
 * @param speed  (boolean) true to compute wind speed, false to compute wind direction
 */ 
function GribWinds(plugin,speed) {
  this.plugin = plugin;
  this.speed = speed;
  this.params = new Object();
  this.count = 1;
  this.sortby = "basetime";
  this.query = new TermQuery(this.plugin);
  this.geom = null;
  this.crs = null;
  this.dataURI = null;
}
/**
 * Main action method. Performs the the queries and creates the derived grid.
 *
 * @return (FloatDataRecord) the derived grid on success, or
 *         (String)          XML null response string on failure 
 */
function _execute() {
  var response;
  /* get the U wind grib */
  var query = new TermQuery(this.plugin);
  for (name in this.params) {
    query.addParameter(name,this.params[name]);
  }
  query.addParameter("paramid","U%wind","like");
  query.setCount(this.count);
  query.setSortBy(this.sortby);
  var uResult = query.execute();
  if (uResult.size() == 0) {
    return this.makeError("Query for Wind-U returned 0 results.",this.query);
  }
  /* get the V wind grib */
  var query = new TermQuery(this.plugin);
  for (name in this.params) {
    query.addParameter(name,this.params[name]);
  }
  query.addParameter("paramid","V%wind","like");
  query.setCount(this.count);
  query.setSortBy(this.sortby);
  var vResult = query.execute();
  if (vResult.size() == 0) {
    return this.makeError("Query for Wind-V returned 0 results.",this.query);
  }
    /* read the data from the data store */
  this.geom = uResult.get(0).getGrid().getGridGeom();
  this.crs = uResult.get(0).getGrid().getCrs();
  this.dataURI = uResult.get(0).getDataURI();
  var uFile = new FileIn(this.plugin,uResult.get(0));
  var vFile = new FileIn(this.plugin,vResult.get(0));
  var uData = uFile.execute();
  var vData = vFile.execute();
    /* combine the data into wind speed data */
  var windSpeed = new ConvertWindsData(uData,vData,this.speed);
  return windSpeed.execute();
}

/* GWS accessors */
function _setSortby(sortValue) {
  this.sortby = sortValue;
}
function _setCount(count) {
  this.count = count;
}
function _addParameter(name,value) {
  this.params[name] = value;
}
function _getGeom() {
  return this.geom;
}
function _getCRS () {
  return this.crs;
}
function _getDataURI() {
  return this.dataURI;
}

/* helper methods */
function _makeError(message,query) {
   var response = new MakeResponseNull(message,query);
   return response.execute();
}

/* map the functions to the class prototype */
GribWinds.prototype.execute = _execute;
GribWinds.prototype.addParameter = _addParameter;
GribWinds.prototype.getGeom = _getGeom;
GribWinds.prototype.getCRS = _getCRS;
GribWinds.prototype.getDataURI = _getDataURI;
GribWinds.prototype.setCount = _setCount;
GribWinds.prototype.setSortby = _setSortby;
GribWinds.prototype.makeError = _makeError;