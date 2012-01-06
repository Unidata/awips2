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
<STYLE TYPE="text/css">
font.l    {font-size:large}
font.xl   {font-size:x-large}
font.2xl  {font-size:xx-large}
a         {text-decoration:none; color: #707070;}
table     {background-color:silver;border-style:solid;border-color:black;border-width:1}
td        {background-color:white;border-style:solid;border-color:black;border-width:1}
th        {background-color:white;border-style:solid;border-color:black;border-width:1}
</STYLE>
<title>Action</title>
</head>
<body>

<center>
<TABLE>
<TR><TD COLSPAN=5 style="text-align: right; font-size: 8pt;">
<a href="index.html">Click here for the Test Driver Interface</a>
</TD></TR>
<TR><TD><img src="rayAWIPS.jpg" align=middle></TD>
<TD colspan=3 style="background-color:silver;">
<FONT CLASS="2xl">&mu;Engine Demonstration Page</FONT></TD>
<TD><img src="rayAWIPS.jpg" align=middle></TD></TR>
</table>
</center>
<P>
<center>
<table>
<TR><TH ColSpan=4>
<FONT CLASS="xl">Select the demonstration script to execute.</FONT>
</TH></TR>


<TR><TD colspan=4><FONT CLASS="l">Subscription/Notificaton Demos</TD></TR>

<TR><TD>
<A HREF="getASCII.jsp">ASCII Retrieval</A> 
</TD><TD>
<A HREF="simpleGrib.jsp">GRIB Retrieval</A>
</TD><TD>
<A HREF="goesDemoSubscribe.jsp">IMAGE Retrieval</A>
</TD><TD>
<A HREF="radarSubscribe.jsp">Radar Retrieval</A>
</TD></TR>

<TR><TD>
<A HREF="asciiDemoUnsubscribe.jsp">ASCII Unsubscribe</A>
</TD><TD>
<A HREF="gribDemoUnsubscribe.jsp">GRIB Unsubscribe<A>
</TD><TD>
<A HREF="goesDemoUnsubscribe.jsp">IMAGE Unsubscribe</A>
</TD><TD>
<A HREF="radarUnsubscribe.jsp">Radar Unsubscribe</A>
</TD></TR>

<TR><TD colspan=4>
<FONT CLASS="l">Test Editor Pages:</FONT>
</TD></TR>
<TR><TD colspan=2>JS Script editors</TD><TD>
<A HREF="jsRunScript.jsp">JScript Editor</A>
</TD><TD>
<A HREF="jsUnsubscribe.jsp">JScript Unsubscribe</A>
</TD></TR>
<TR><TD colspan=2>Hello World Demo</TD><TD>
<A HREF="HelloWorld.jsp">Form Version</A>
</TD><TD>
<A HREF="makeHelloWorld.jsp">Demo Script</A>
</TD></TR>

<TR><TD colspan=4>
<font class="l">VTEC Retrieval Demos</font>
</TD></TR>
<TR><TD>
<A HREF="jsGetVTECAction.jsp">Simple Retrieval</A>
</TD><TD>
<A HREF="jsGetVTECActionNew.jsp">Object Retrieval</A>
</TD><TD>
<A HREF="jsVTECEventQuery.jsp">Query VTN</A>
</TD><TD>
<A HREF="jsVTECEventUpdate.jsp">Update VTN</A>
</TD></TR>

<TR><TD colspan=4>
<FONT CLASS="l">Legacy Adaptor Test Scripts:</FONT>
</TD></TR>
<TR><TD colspan=2>
Execute External Process
</TD><TD>
<A HREF="scriptExecute.jsp">AdapterSrv Test (bat)</A>
</TD><TD>
<A HREF="applicationExecute.jsp">AdapterSrv Test (exe)</A>
</TD></TR>

<TR><TD colspan=4>
<FONT CLASS="l">Test scripts demonstrating JS Scripting &mdash; Full Message Format:</FONT><BR>
</TD></TR>
<TR><TD COLSPAN=2>
GOES Image Retrieval
</TD><TD>
<A HREF="testMessageFormat.jsp">Full Script</A>
</TD><TD>
<A HREF="compactMessageFormat.jsp">Compact Version</A>
</TD></TR>
<TR><TD COLSPAN=2>
Observation Retrival via Spatial Query
</TD><TD>
<A HREF="makeSpatialQuery.jsp">Full Script</A>
</TD><TD>
<A HREF="compactSpatialQuery.jsp">Compact Version</A>
</TD></TR>

<TR><TD colspan=4>
<FONT CLASS="l">Retrieval scripts with some Java Processing:</FONT>
</TD></TR>
<TR><TD COLSPAN=2>
GOES Image Filtering
</TD><TD>
<A HREF="makeFilteredImage.jsp">Full Script</A>
</TD><TD>
<A HREF="compactFilteredImage.jsp">Compact Version</A>
</TD></TR>

<TR><TD COLSPAN=2>
Stoplight Chart from GRIB Retrieval
</TD><TD>
<A HREF="makeStopLightImage.jsp">Full Script</A>
</TD><TD>
<A HREF="compactStopLightImage.jsp">Compact Version</A>
</TD></TR>

<TR><TD colspan=4>
<FONT CLASS="l">Retrieval scripts demonstrating more complex products:</FONT>
</TD></TR>

<TR><TD COLSPAN=2>
Two GRID retrieval &ndash; Computes Wind Speed
</TD><TD>
<A HREF="makeWindSpeedImage.jsp">Full Script</A>
</TD><TD>
<A HREF="compactWindSpeedImage.jsp">Compact Version</A>
</TD></TR>

<TR><TD COLSPAN=2>
Sample Math Scripting &ndash; Emulates creation of wind warning chart
</TD><TD>
<A HREF="makeStopLightWinds.jsp">Full Script</A>
</TD><TD>
<A HREF="compactStopLightWinds.jsp">Compact Version</A>
</TD></TR>

<TR><TD COLSPAN=2>
Barnes Scheme Analysis &ndash; Corrects Temperature
</TD><TD>
<A HREF="makeBarnesTemperature.jsp">Full Script</A>
</TD><TD>
<A HREF="compactBarnesTemperature.jsp">Compact Version</A>
</TD></TR>


<TR><TD colspan=4>
<FONT CLASS="l">Multi-product Retrievals:</FONT>

</TD></TR>

<TR><TD COLSPAN=2>
Two Product Retrieval
</TD><TD>
<A HREF="makeASCIIandImage.jsp">OBs and Image</A>
</TD><TD>
<A HREF="METARandTAF.jsp">OBs and TAFs</A>
</TD></TR>

<TR><TD COLSPAN=2>
Barnes Scheme Analysis &ndash; Corrects Wind Speed
</TD><TD>
<A HREF="makeBarnesAnalysisWinds.jsp">Full Script</A>
</TD><TD>
<A HREF="compactBarnesAnalysisWinds.jsp">Compact Version</A>
</TD></TR>

</TABLE>
</center>
</body>
</html>
