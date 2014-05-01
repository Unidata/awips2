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
<title>Stop Light Demo</title>
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
Stop Light Chart</H1>
<CENTER>Uses <em>StopLightImage task</em> to convert image.</CENTER></TD>
<TD><img src="rayAWIPS.jpg" align=middle></TD></TR>
<TR><TD COLSPAN=3><CENTER>
Simulates creation of a "Stop Light" decision chart based on temperature.<BR>
Uses the <em>StopLightImage</em> task to convert image.<BR>
This page contains the entire script.
</CENTER></TD></TR>
</table>

<form method=post action="runAction.jas">
<input type=hidden name=requesttype value=javascript>
<input type=hidden name=sortby value=timeobs>
<table align=center>
<tr><td colspan=2>
<textarea name="actionXML" cols="80" rows="22" style="background-color:aqua">
function StopLightMaker(){
  this.plugin = "grib";
  this.subscribe = false;
  this.subscription = null;
  this.queryResults = null;
  this.reproject = false;
  this.colormap = "StopLight";
  this.format = "png";
  this.scaleFactor = 3.0;
  this.query = new TermQuery(this.plugin, subscriptionDataFieldId, subscriptionDataQueryId);
}

function _addParameter(name, value){
  this.query.addParameter(name, value);
}

function _addList(name, value){
  this.query.addParameter(name, value, "in");
}

function _setCount(count){
  this.query.setCount(count);
}

function _setScaleFactor(scale){
  this.scaleFactor = scale;
}

function _enableSubscription(){
  this.subscribe = true;
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

function _setSortValue(sortValue){
  this.query.setSortBy(sortValue);
}

function _requestImage(image){
  this.createImage = image;
}

function _execute()
{
  if(this.subscribe){
    this.subscription = new Subscription();
    this.subscription.setup(this.query);
  }
  this.queryResults = this.query.execute();
  if(this.queryResults == null || this.queryResults.size() == 0)
  {
    var response = new MakeResponseNull("Query returned 0 results.",this.query);
    return response.execute();
  }
  else
  {
    return this.makeImageResponse();
  }
}

function _makeImageResponse(){
  var response = new Array();
  for(i=0; i < this.queryResults.size(); i++)
  {  
    var currentQuery = this.queryResults.get(i);
    var geom = currentQuery.getGrid().getGridGeom();
    var crs = currentQuery.getGrid().getCrs();
    var fileIn = new FileIn(this.plugin, currentQuery);
    var stopLight = new StopLightImage(fileIn.execute());
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
    var makeResponse = new MakeResponseUri(writeFile, null, currentQuery.getDataURI(), this.format);
    response[i] = makeResponse.execute();
  }
  return response;
}

StopLightMaker.prototype.execute = _execute;
StopLightMaker.prototype.makeImageResponse = _makeImageResponse;
StopLightMaker.prototype.addParameter = _addParameter;
StopLightMaker.prototype.addList = _addList;
StopLightMaker.prototype.setCount = _setCount;
StopLightMaker.prototype.setScaleFactor = _setScaleFactor;
StopLightMaker.prototype.enableSubscription = _enableSubscription;
StopLightMaker.prototype.reprojectImage = _reprojectImage;
StopLightMaker.prototype.setColormap = _setColormap;
StopLightMaker.prototype.setFormat = _setFormat;
StopLightMaker.prototype.setSortValue = _setSortValue;
StopLightMaker.prototype.requestImage = _requestImage;

var dataRequest = new StopLightMaker();
dataRequest.addParameter("paramid","Temperature");
dataRequest.addParameter("levelinfo","2.0_m");
dataRequest.addParameter("forecasttime","0");
dataRequest.addParameter("gridid",212);
dataRequest.setCount(1);
dataRequest.setColormap("StopLight");
dataRequest.setSortValue("basetime");
dataRequest.execute();

</textarea>
</td></tr>
<tr><th colspan=2><em>Enter Values for Message</em></th></tr> 

<tr><td><B>Name:</B></td>
<td><input type=text name=name value="JS Request"></td></tr>

</td></td>
<tr><td><B>Action:</B></td><td>
<input type=radio name=function value=validate disabled>Validate
<input type=radio name=function value=subscribe>Subscribe
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