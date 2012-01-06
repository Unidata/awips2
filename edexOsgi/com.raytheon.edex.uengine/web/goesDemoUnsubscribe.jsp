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
<title>Image Retrieval</title>
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
<TD><H1>&mu;Engine Demonstration<BR>GOES Image Unsubscribe</H1>
<CENTER>Form Driven Test Page</CENTER></TD>
<TD><img src="rayAWIPS.jpg" align=middle></TD></TR>
</table>
<BR>
<form method=post action="runAction.jas">
<input type=hidden name=requesttype value=unsubscribe>
<input type=hidden name=plugin value=Satellite>
<input type=hidden name=datatype value=Images>
<input type=hidden name=function value=unsubscribe>
<table width="50%" align=center>
<TR><TH colspan=3>Enter Action Script Parameters to unsubscribe and press Submit.</TH></TR>
<TR><TD><B>Name:</B></TD><TD colspan=2><input type=text name=scriptname value="Sat Request"></TD></TR>
<TR><TD><B>DataType:</B></TD><TD colspan=2><em>Satellite</em></TD></TR>
<TR><TD><B>Location:</B></TD>
<TD colspan=2>
   <select name=location>
      <option value="East CONUS" SELECTED>East CONUS
      <option value="West CONUS">West CONUS
   </select>
</TD></TR>
<TR><TD><B>Parameter:</B></TD>
<TD colspan=2>
   <select name=parameter>
      <option value="Imager 11 micron (IR)" SELECTED>Imager 11 micron (IR)
      <option value="Imager Visible">Imager Visible (VIS)
   </select>
</TD></TR>
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
