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
<title>Multi Product Request</title>
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
Multi-Product Retrieval</H1>
<TD><img src="rayAWIPS.jpg" align=middle></TD></TR>
<TR><TD COLSPAN=3><CENTER>
This script retrieves and displays observations and generates an image.<BR>
This page uses <em>SatelliteRequest.js</em> and <em>ObsRequest.js</em> from the script library.<BR>
This page contains the entire demo script.
</CENTER></TD></TR>
</table>

<form method=post action="runAction.jas">
<input type=hidden name=requesttype value=javascript>
<input type=hidden name=sortby value=timeobs>
<table align=center>
<tr><td colspan=2>
<textarea name="actionXML" cols="80" rows="22" style="background-color:aqua">
include("SatelliteRequest.js");
include("ObsRequest.js");
function ASCIIandImage(count,icaos) {
  var sat = new SatelliteRequest();
  sat.setCount(1);
  sat.addParameter("product_type","Satellite");
  sat.addParameter("datatype","Images");
  sat.addParameter("area_subtype","East CONUS");
  sat.addParameter("goes","Imager 11 micron (IR)");
  sat.setSortValue("valid_time");
  sat.setFormat("png");
  sat.setColormap("IREnhanced");
  sat.requestImage(true);
  var obs = new ObsRequest();
  obs.setCount(count);
  if (icaos != null) {
    obs.addList("stationid",icaos);
  }
  obs.addParameter("reporttype","METAR");
  obs.setSortValue("timeobs");
  return combineResponses(obs.execute(),sat.execute());
}
function combineResponses(respA,respB) {
  var retVal = new Array();
  var ptr = 0;
  if (respA != null && respA.length > 0) {
    for (i = 0; i < respA.length; i++) {
      retVal[ptr] = respA[i];
      ptr++;
    }
  }
  if (respB != null && respB.length > 0) {
    for (i = 0; i < respB.length; i++) {
      retVal[ptr] = respB[i];
      ptr++;
    }
  }
  return retVal;
}
/* change count to retrieve multiple obs */
ASCIIandImage(1);

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
   <option value=120000 selected>2 minutes
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