<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
    pageEncoding="ISO-8859-1"%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<%
response.setHeader("Pragma", "no-cache");
response.setHeader("Cache-Control", "no-cache");
response.setDateHeader("Expires", 0);
%>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
<title>Spatial Query Demo</title>
<STYLE TYPE="text/css">
h1    {text-align:center}
table {background-color:silver;border-style:solid;border-color:black;border-width:1}
td    {background-color:white;border-style:solid;border-color:black;border-width:1}
th    {background-color:white;border-style:solid;border-color:black;border-width:1}
</STYLE>
</head>
<body>
<table align=center>
<TR><TD><img src="rayAWIPS.jpg" align=middle></TD>
<TD><H1>
&mu;Engine Demonstration<BR>
JScript Script Runner<BR>
Sample Spatial Query
</H1>
</TD>
<TD><img src="rayAWIPS.jpg" align=middle></TD></TR>
<TR><TD COLSPAN=3><CENTER>
This script demonstrates using a Spatial Query to obtain observation records.<BR>
This page uses the <em>SpatialQuery</em> task to obtain the station list.
</CENTER></TD></TR>
</table>

<form method=post action="runAction.jas">
<input type=hidden name=requesttype value=javascript>
<input type=hidden name=sortby value=timeobs>
<table align=center>
<tr><td colspan=2>
<textarea name="actionXML" cols="80" rows="22" style="background-color:aqua">
/* the class constructor */
function SpatialDataQuery(plugin) {
  /* named constants */
  this.icao = "icao";
  this.stationid = "stationid";
  /* the query objects */
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
  }
  return this.makeAsciiResponse(result);
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
SpatialDataQuery.prototype.makeNullResponse = _makeNullResponse;
SpatialDataQuery.prototype.listToString = _listToString;

/* compact version */
var demo = new SpatialDataQuery();
demo.setSpatialBounds(42.00,-96.50,41.00,-95.50);
demo.addSpatialField("icao");
demo.execute();

</textarea>
</td></tr>
<tr><th colspan=2><em>Enter Values for Message</em></th></tr> 

<tr><td><B>Name:</B></td>
<td><input type=text name=name value="JS Request"></td></tr>

</td></td>
<tr><td><B>Action:</B></td><td>
<input type=radio name=function value=validate disabled>Validate
<input type=radio name=function value=subscribe disabled>Subscribe
<input type=radio name=function value=execute checked>Execute</tr>
</td></tr></table>
<br>
<div align=center>
<input type="submit" value="Get Report">
<input type=reset>
Timeout:
<select name=receiveTime>
   <option value=60000 selected>1 minute
   <option value=120000>2 minutes
   <option value=180000>3 minutes
   <option value=240000>4 minutes
   <option value=300000>5 minutes
   <option value=360000>6 minutes
   <option value=420000>7 minutes
   <option value=480000>8 minutes
   <option value=540000>9 minutes
   <option value=600000>10 minutes
</select>
</div>
</form>
</body>
</html>