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
<title>Radar Retrieval</title>
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
<TD><H1>&mu;Engine Demonstration<BR>Radar Image Unsubscribe</H1>
<CENTER>Form Driven Test Page</CENTER></TD>
<TD><img src="rayAWIPS.jpg" align=middle></TD></TR>
</table>
<form method=post action="runAction.jas">
<input type=hidden name=requesttype value=unsubscribe>
<input type=hidden name=datatype value=radar>
<input type=hidden name=plugin value=radar>
<input type=hidden name=function value=unsubscribe>
<table width="50%" align=center>
<TR>
<TH colspan=3>Enter Action Script Parameters to unsubscribe and press Submit.</TH>
</TR>

<TR>
<TD><B>Name:</B></TD>
<TD colspan=2><input type=text name=scriptname value="Radar Image Demo" size=25></TD>
</TR>

<TR>
<TD><B>DataType:</B></TD>
<TD colspan=2><em>Radar</em></TD>
</TR>

<TR>
<TD><B>Station:</B></TD>
<TD><input type=text name=station value="KABR"></TD>
</TR>

<TR>
<TD><B>Product Code:</B></TD>
<TD><input type=text name=productcode value="94"></TD>
</TR>

<TR>
<TD><B>Elevation Angle:</B></TD>
<TD><input type=text name=elevation value="0.5"></TD>
</TR>

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