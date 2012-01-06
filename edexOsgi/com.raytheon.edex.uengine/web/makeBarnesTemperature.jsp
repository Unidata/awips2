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
<title>Barnes Analysis</title>
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
<TD><H1>&mu;Engine Demonstration<BR>JScript Script Runner<BR>Barnes Analysis</H1></TD>
<TD><img src="rayAWIPS.jpg" align=middle></TD></TR>
<TR><TD COLSPAN=3>
<CENTER>
This script demonstrates using Barnes Scheme analysis to correct a temperature grid.<BR>
This page shows the entire script.
</CENTER>
</TD></TR>
</table>

<form method=post action="runAction.jas">
<input type=hidden name=requesttype value=javascript>
<input type=hidden name=sortby value=timeobs>
<table align=center>
<tr><td colspan=2>
<textarea name="actionXML" cols="80" rows="22" style="background-color:aqua">
/* constructor */
function BarnesAnalysis() { 
  /* names of constants */
  this.grib = "grib";
  this.icao = "icao";
  this.geometry = "geometry";
  /* settings for Barnes Analysis */
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

/* mapping functions to the object */
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

/* the compact action script */
var barnes = new BarnesAnalysis();
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
   <option value=60000>1 minute
   <option value=120000>2 minutes
   <option value=180000>3 minutes
   <option value=240000 selected>4 minutes
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