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
<title>ASCII Request</title>
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
<TD><H1>&mu;Engine Demonstration<BR>ASCII Product Unsubscribe</H1>
<CENTER>Form Driven Test Page</CENTER></TD>
<TD><img src="rayAWIPS.jpg" align=middle></TD></TR>
</table>
<BR>

<BR>
<form method=post action="runAction.jas">
<input type=hidden name=requesttype value=unsubscribe>
<input type=hidden name=datatype value=ascii>
<input type=hidden name=function value=unsubscribe>

<table width=350 align=center>
<tr><th colspan=2>
Enter Action Script Parameters and press Submit.
</th></tr>
<TR><TD><B>Name:</B></TD>
<TD><input type=text name=scriptname value="ASCII Request"></TD></TR>

<tr><td><B>Type:</B></td><td>
<input type=radio name=type value=METAR checked>METAR
<input type=radio name=type value=SPECI>SPECI
<input type=radio name=type value=MESOWEST>Mesowest
<!--
<input type=radio name=type value=TAF>TAF
-->
</td></tr>

<tr><td><B>Station:</B></td><td><input type=text name=station value=""></td></tr>

<tr><td><B>Time:</B></td>
<td><input type=text name=time value="">(YYYYMMDDhhmmss)<BR>
<input type=radio name=refTime value=base checked>Base
<input type=radio name=refTime value=bin>Reference
<input type=radio name=refTime value=hour>Cardinal
</td></tr>
</table>
<BR>
<div align=center>
<input type="submit"><input type=reset>
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
