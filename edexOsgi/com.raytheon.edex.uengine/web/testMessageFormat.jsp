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
<title>JS Script Test</title>
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
<TD><H1>&mu;Engine Demonstration<BR>GOES Image Retrieval</H1></TD>
<TD><img src="rayAWIPS.jpg" align=middle></TD></TR>
<TR><TD COLSPAN=3><CENTER>
This page contains the entire demo script.
</CENTER></TD></TR>
</table>

<form method=post action="runAction.jas">
<table align=center>
<TR><TH>Enter/modify the Action JS Script into the textbox and press Submit.</TR></TD>
<TR><TD>
    <textarea name="actionXML" cols="80" rows="22" style="background-color:aqua">
&lt;message&gt;
    &lt;header&gt;
        &lt;property name="id" value="QueryImageDecode" /&gt;
        &lt;property name="time" value="20060809152600" /&gt;
        &lt;property name="function" value="execute" /&gt;
    &lt;/header&gt;
&lt;![CDATA[
function SatelliteRequest(count, sat, image) {
	this.satellite = sat;
	this.area = "East CONUS";
	this.count = count;
	this.makeImage = image;
    this.format = "png";
    this.colormap = "IREnhanced";
    this.reproject = false;
    this.plugin = "satellite";
}

function _execute()
{
	var query = new TermQuery("satellite");
	query.addParameter("satellite", this.satellite);
	query.addParameter("area_subtype", this.area);
	query.setCount(this.count);
	var queryResults = query.execute();
    if(queryResults == null || queryResults.size() == 0)
    {
        response = new MakeResponseNull("Query returned 0 results.",query);
        return response.execute();
    }
	else
	{
		if (!this.makeImage)
		{
			return this.makeXmlResponse(queryResults);
		} else {
			return this.makeImageResponse(queryResults);
		}
	}
}

function _makeXmlResponse(queryResults)
{
	var count = queryResults.size();
	var response = new Array(count);
	for(i=0; i < count; i++)
	{
		var dataToXml = new DataToXml(queryResults.get(i));		
		var makeResponse = new MakeResponseXml(dataToXml.execute());
		response[i] = makeResponse.execute();
	}

	return response;
}

function _makeImageResponse(queryResults){
  var response = new Array();
  for(i=0; i < queryResults.size(); i++)
  {
    var currentQuery = queryResults.get(i);
    var geom = currentQuery.getCoverage().getMapGeom();
    var crs = currentQuery.getCoverage().getCrs();
    var fileIn = new FileIn(this.plugin, currentQuery);
    var record = fileIn.execute();
    var colorMap = new ColorMapImage(this.colormap, record.getDataObject(), geom);
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

SatelliteRequest.prototype.execute = _execute;
SatelliteRequest.prototype.makeXmlResponse = _makeXmlResponse;
SatelliteRequest.prototype.makeImageResponse = _makeImageResponse;

// Code the user writes:
var dataRequest = new SatelliteRequest(1, "GOES",true);
dataRequest.execute();
]]&gt;
&lt;/message&gt;
</textarea>
</TD></TR>
</TABLE>
<br>
<div align=center>
<input type="submit" value="Get Image">
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
<br>
</form>
</body>
</html>
