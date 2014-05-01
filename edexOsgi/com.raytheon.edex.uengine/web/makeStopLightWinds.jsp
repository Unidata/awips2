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
<title>Stoplight Demo</title>
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
<TD>
<H1>&mu;Engine Demonstration<BR>
JScript Script Runner<BR>
Wind Stop Light Chart
</H1>
</TD>
<TD><img src="rayAWIPS.jpg" align=middle></TD></TR>
<TR><TD COLSPAN=3>
<CENTER>
Combines two GRIB records to create a Wind Speed stoplight chart.<BR>
Uses the <em>StopLightImage</em> and <em>ConvertWindsData</em> tasks to convert image.<BR>
This page contains the complete script.
</CENTER>
</TD></TR>
</table>

<form method=post action="runAction.jas">
<input type=hidden name=requesttype value=javascript>
<input type=hidden name=sortby value=timeobs>
<table align=center>
<tr><td colspan=2>
<textarea name="actionXML" cols="80" rows="22" style="background-color:aqua">
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
  var geom = uQueryResults.get(0).getGrid().getGridGeom();
  var uFile = new FileIn(this.plugin,uQueryResults.get(0));
  var vFile = new FileIn(this.plugin,vQueryResults.get(0));
  var uData = uFile.execute();
  var vData = vFile.execute();
  var windSpeed = new ConvertWindsData(uData,vData,this.windSpeed);
  var stopLight = new StopLightImage(windSpeed.execute());
  var gribMap = new GribMap(this.plugin, this.colormap, stopLight.execute(), geom);
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
    imageOut = new ImageOut(colorMap.execute(), this.format);
  }
  var fileOut = new FileOut(imageOut.execute(), this.format);
  var writeFile = fileOut.execute();
  var makeResponse = new MakeResponseUri(writeFile, null, null, this.format);
  return makeResponse.execute();
}

// setter methods
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
 
// add the methods to the SatelliteRequest Object.
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

// Code the user writes:
var dataRequest = new WindStopLight();
dataRequest.setLevelInfo("10.0_m");
dataRequest.setScaleFactor(3.0);
dataRequest.setFcstHour("0");
dataRequest.setGridId(212);
dataRequest.setCount(1);
dataRequest.setColormap("StopLight");
dataRequest.setColors(0.0,1.0,2.0);
dataRequest.setSortValue("basetime");
dataRequest.setFormat("png");
dataRequest.reprojectImage(false);
dataRequest.setWindMode(true);
dataRequest.execute();

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