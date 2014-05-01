/* the class constructor */
function SpatialDataQuery(plugin) {
  /* named constants */
  this.icao = "icao";
  this.stationid = "stationid";
  /* the query objects */
  this.asciiResponse = false;
  this.plugin = (plugin!=null)?plugin:"obs";
  this.spatial = new SpatialQuery();
  this.query = new TermQuery(this.plugin);
}

/* the execute function */
function _execute() {
  var response = this.spatial.execute();
  if (response.size() == 0 || response.get(this.icao).size() == 0) {
    return this.makeNullResponse(null);
  }
  var icaos = this.listToString(response.get(this.icao),",");
  this.query.addParameter(this.stationid,icaos,"in");
  this.query.setCount(0);
  var result = this.query.execute();
  if (result.size() == 0) {
    return this.makeNullResponse(this.query);
  } else if (this.asciiResponse) {
    return this.makeAsciiResponse(result);
  } else {
      return this.makeXmlResponse(result);
  }
}

/* converts a List to a string */
function _listToString(list,sep){
   var string = new Array();
   for (i=0;i<list.size();i++) {
      string[i] = list.get(i);
   }
   return string.join(sep);
}

/* creates the null response when a query returns no results */
function _makeNullResponse(query) {
  var plugin;
  if (query == null) {
  	query = new TermQuery(this.plugin);
    query.addParameter(this.stationid,".+?");
    plugin = "spatial";
  } else {
    plugin = query.getPlugin();
  }
  var msg = plugin + " query returned no results.";
  var response = new MakeResponseNull(msg,query);
  return response.execute();
}

/* generates the ascii response */
function _makeAsciiResponse(result) {
  var xmlResults = new Array();
  var response = new Array();
  for(i=0; i < result.size(); i++)
  {
    var toXml = new DataToXml(result.get(i));
    xmlResults[i] = toXml.execute();	
    var makeResponse = new MakeResponseAscii(result.get(i), xmlResults[i]);
    response[i] = makeResponse.execute();
  }
  return response;
}

/* generates XML response */
function _makeXmlResponse(result) {
  var response = new Array();
  for (i=0; i < result.size(); i++) {
    var makeResponse = new MakeResponseXML(result.get(i));
    response[i] = makeResponse.execute();
  }
  return response;
}

/* setters for the spatial query */
function _setSpatialBounds(ulLat, ulLon, lrLat, lrLon) {
  this.spatial.setUpperLeftLat(ulLat);
  this.spatial.setUpperLeftLon(ulLon);
  this.spatial.setLowerRightLat(lrLat);
  this.spatial.setLowerRightLon(lrLon);
}

function _addSpatialField(name) {
  this.spatial.addField(name);
}

/* mapping functions to the object */
SpatialDataQuery.prototype.execute = _execute;
SpatialDataQuery.prototype.setSpatialBounds = _setSpatialBounds;
SpatialDataQuery.prototype.addSpatialField = _addSpatialField;
SpatialDataQuery.prototype.makeAsciiResponse = _makeAsciiResponse;
SpatialDataQuery.prototype.makeXmlResponse = _makeXmlResponse;
SpatialDataQuery.prototype.makeNullResponse = _makeNullResponse;
SpatialDataQuery.prototype.enableAsciiResponse = _enableAsciiResponse;
SpatialDataQuery.prototype.listToString = _listToString;

/* compact version */
var demo = new SpatialDataQuery();
demo.setSpatialBounds(42.00,-96.50,41.00,-95.50);
demo.addSpatialField("icao");
demo.execute();