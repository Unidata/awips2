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
<title>JS Script Runner</title>
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
JScript Product Unsubscribe
</H1></TD>
<TD><img src="rayAWIPS.jpg" align=middle></TD></TR>
</table>
<BR>
<form method=post action="runAction.jas">
<input type=hidden name=requesttype value=jsunsubscribe>
<input type=hidden name=datatype value=ascii>
<input type=hidden name=function value=unsubscribe>

<table width=350 align=center>
<tr><th colspan=2><em>Enter Subscription Values</em></th></tr>
<TR><TD><B>Name:</B></TD>
<TD><input type=text name=scriptname value="JS Request"></TD></TR>

<tr><td><B>Subscription URI:</B></td><td><input type=text name=scripturi value=""></td></tr>
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
