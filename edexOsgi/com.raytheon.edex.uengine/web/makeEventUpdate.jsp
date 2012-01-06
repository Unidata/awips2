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
<title>Event Tracking Update</title>
<STYLE TYPE="text/css">
h1    {text-align:center}
table {background-color:silver;border-style:solid;border-color:black;border-width:1}
td    {background-color:white;border-style:solid;border-color:black;border-width:1}
th    {background-color:white;border-style:solid;border-color:black;border-width:1}
</STYLE>
<SCRIPT LANGUAGE="JavaScript">
   function makeSubmit() {
      var form = document.updater;
      var script = "include(\"VtecEventUpdate.js\");\n" +
                   "var runner = new VtecUpdater();\n" +
                   "runner.setTimeOut(10);\n" +
                   "runner.setClientID(\"VTEC Update\");\n" +
                   "runner.setFields(\"" + form.office.value + 
                   "\",\"" + form.phenom.value + "\",\"" + form.signif.value + 
                   "\"," + form.number.value + ");\n" +
                   "runner.execute();\n";
      form.actionXML.value = script;

      form.action = "runAction.jas";
      form.submit();
   }
</SCRIPT>
</head>

<body>
<table align=center>
<TR><TD><img src="rayAWIPS.jpg" align=middle></TD>
<TD><H1>
&mu;Engine Demonstration<BR>
Event Tracking Update
</H1></TD>
<TD><img src="rayAWIPS.jpg" align=middle></TD></TR>
<TR><TD COLSPAN=3><CENTER>
This page creates a &mu;Engine Script "on the fly".
</CENTER></TD></TR>
</table>

<form name=updater method=post action="JavaScript:makeSubmit();">
<input type=hidden name=requesttype value=javascript>
<input type=hidden name=sortby value=timeobs>
<input type=hidden name="VTEC Request">
<input type=hidden name=actionXML>
<table align=center>
<tr><th colspan=2><em>Enter Values for Message</em></th></tr>

<tr><td><b>Office</b>:</td><td><input name=office size=10 maxLength=10 value=KOAX></td></tr>
<tr><td><b>Phenomena</b>:</td><td><input name=phenom size=10 maxLength=10 value=BZ></td></tr>
<tr><td><b>Significance</b>:</td><td><input name=signif size=10 maxLength=10 value=W></td></tr>
<tr><td><b>New VTN</b>:</td><td><input name=number size=10 maxLength=10 value=1></td></tr>

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

