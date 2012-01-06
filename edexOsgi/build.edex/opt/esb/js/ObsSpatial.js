/**
 * Performs a spatial query and creates a list of Observation objects for the
 * specified observation parameter for stations in the specified spatial region.
 * 
 * First performs the spatial query for the region to create a list of stations,
 * then queries for observations for the stations. Finally it creates a list of
 * Observation objects for the specified parameter.
 *
 * This class may be imported into a JS uEngine script needing to perform a
 * spatial query for a specific observation parameter.
 * 
 * Usage:
 *   include("ObsSpatial.js");
 *   var oQuery = new ObsSpatialQuery("obs","temperature");
 *   oQuery.addParameter("refhour","20070601190000");
 *   oQuery.setSortby("timeobs");
 *   oQuery.setCount(0);
 *   oQuery.setSpatialBounds(43.00, -98.00, 37.00, -92.00,"icao","geometry");
 *   var oResult = oQuery.execute();
 */

/**
 * Class constructor.
 *
 * @param plugin  (String) identifies the data-type plugin - normally "obs"
 * @param obParam (String) the observation parameter to query.
 */ 
function ObsSpatialQuery(plugin,obParam) {
  this.icao = "icao";
  this.geometry = "geometry";
  this.plugin = plugin;
  this.obParam = obParam;
  this.oQuery = new TermQuery(this.plugin);
  this.sQuery = new SpatialQuery();
}
/**
 * Main action method. Performs the the queries and creates the Observation list.
 *
 * @return (ArrayList) the Observation list on success, or
 *         (String)    XML null response string on failure
 */
function _execute() {
  /* get the spatial results */
  var sResult = this.sQuery.execute();
  if(sResult == null || sResult.size() == 0) {
    return this.makeError("Spatial query returned no results.",this.spatial);
  }
  /* get and geolocate the wind speed observations */
  this.addList("stationid",MEUtils.changeArrayListToString(sResult.get(this.icao)));
  var oResult = this.oQuery.execute();
  if(oResult == null || oResult.size() == 0) {
    return this.makeError("Ob query returned no results.",oQuery);
  }
  var oMap = new MapAsciiData(this.obParam,
                              oResult,
                              sResult.get(this.icao),
                              sResult.get(this.geometry));
  
  return oMap.execute();
}
/* OSQ accessors */
function _setSortby(sortValue) {
  this.oQuery.setSortBy(sortValue);
}
function _setCount(count) {
  this.oQuery.setCount(count);
}
function _addParameter(name,value) {
  this.oQuery.addParameter(name,value);
}
function _addList(name,value) {
  this.oQuery.addParameter(name,value,"in");
}

/* setter for the spatial bounds */
function _setSpatialBounds(ulLat, ulLon, lrLat, lrLon, icao, geom) {
  this.icao = icao;
  this.geometry = geom;
  this.sQuery.setUpperLeftLat(ulLat);
  this.sQuery.setUpperLeftLon(ulLon);
  this.sQuery.setLowerRightLat(lrLat);
  this.sQuery.setLowerRightLon(lrLon);
  /* preload fields of the query */
  this.sQuery.addField(this.icao);
  this.sQuery.addField(this.geometry);
}

/* helper methods */
function _makeError(message,query) {
   var response = new MakeResponseNull(message,query);
   return response.execute();
}

/* map the functions to the class prototype */
ObsSpatialQuery.prototype.execute = _execute;
ObsSpatialQuery.prototype.addParameter = _addParameter;
ObsSpatialQuery.prototype.setCount = _setCount;
ObsSpatialQuery.prototype.setSortby = _setSortby;
ObsSpatialQuery.prototype.makeError = _makeError;
ObsSpatialQuery.prototype.setSpatialBounds = _setSpatialBounds;
ObsSpatialQuery.prototype.addList = _addList;