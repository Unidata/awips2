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
<TD><H1>
&mu;Engine Demonstration<BR>
GOES Image Retrieval
</H1></TD>
<TD><img src="rayAWIPS.jpg" align=middle></TD></TR>
<TR><TD COLSPAN=3><CENTER>
Uses <em>SatelliteRequest.js</em> from the script library.
</CENTER></TD></TR>
</table>
<P>
<form method=post action="runAction.jas">
<table align=center>
<TR><TH>Enter/Edit the Action JS Script into the textbox and press Submit.</TR></TD>
<TR><TD>
    <textarea name="actionXML" cols="80" rows="22" style="background-color:aqua">
&lt;message&gt;
    &lt;header&gt;
        &lt;property name="id" value="QueryImageDecode" /&gt;
        &lt;property name="time" value="20060809152600" /&gt;
        &lt;property name="function" value="execute" /&gt;
    &lt;/header&gt;
&lt;![CDATA[
include("SatelliteRequest.js");
var dataRequest = new SatelliteRequest();
dataRequest.requestImage(true);
dataRequest.setCount(1);
dataRequest.addParameter("area_subtype","East CONUS");
dataRequest.setColormap("IREnhanced");
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