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
<TD><H1>&mu;Engine Demonstration<BR>METAR and TAF Retrieval</H1></TD>
<TD><img src="rayAWIPS.jpg" align=middle></TD></TR>
<TR><TD COLSPAN=3><CENTER>
This script displays observations and tafs.<BR>
It  uses <em>ObsRequest.js</em> and <em>TafRequest.js</em> from the script library.<BR>
This page contains the entire demo script.
</CENTER></TD></TR>
</table>

<form method=post action="runAction.jas">
<input type=hidden name=requesttype value=javascript>
<table align=center>
<tr><td colspan=2>
<textarea name="actionXML" cols="80" rows="22" style="background-color:aqua">
include("ObsRequest.js");
include("TafRequest.js");

var oQuery = new ObsRequest();
var tQuery = new TafRequest();

function ObAndTaf(count,icaos) {
  var icao = "KOFF";
  oQuery.setCount(count);
  oQuery.setSortValue("timeobs");
  oQuery.addParameter("reporttype","METAR");
  if (icaos != null) {
    oQuery.addList("stationid",icaos);
  } else {
    oQuery.addParameter("stationid",icao);
  }
  tQuery.setCount(count);
  if (icaos != null) {
    tQuery.addList("stationid",icaos);
  } else {
    tQuery.addParameter("stationid",icao);
  }
  tQuery.setSortValue("issue_time");
  return combineResponses(oQuery.execute(),tQuery.execute());
}
function combineResponses(respA,respB) {
  var retVal = new Array();
  var ptr = 0;
  if (respA != null && respA.length > 0) {
    for (i = 0; i < respA.length; i++) {
      retVal[ptr] = respA[i];
      ptr++;
    }
  } else {
    retVal[ptr] = respA;
    ptr++;
  }
  if (respB != null && respB.length > 0) {
    for (i = 0; i < respB.length; i++) {
      retVal[ptr] = respB[i];
      ptr++;
    }
  } else {
    retVal[ptr] = respB;
    ptr++
  }
  return retVal;
}
function makeError(message,query) {
   var response = new MakeResponseNull(message,query);
   return response.execute();
}
ObAndTaf(3,"KSDF,KGAG,KIAH,KLBE,KLUK");
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
