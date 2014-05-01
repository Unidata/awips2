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
<title>Hello World</title>
<STYLE TYPE="text/css">
h1    {text-align:center}
table {background-color:silver;border-style:solid;border-color:black;border-width:1}
td    {background-color:white;border-style:solid;border-color:black;border-width:1}
th    {background-color:white;border-style:solid;border-color:black;border-width:1}
</STYLE>
<SCRIPT LANGUAGE="JavaScript">
   function makeSubmit() {
      var form = document.hello;
      var script = "include(\"HelloWorld.js\");\n"
                 + "var runner = new HelloWorld();\n"
                 + "runner.setMessage(\"" + form.message.value + "\");\n"
                 + "runner.execute();"
      form.actionXML.value = script;
      form.action = "runAction.jas";
      form.submit();
   }
</SCRIPT>
</head>
<body>
<table align=center>
<TR><TD><img src="rayAWIPS.jpg" align=middle></TD>
<TD><CENTER><H1>&mu;Engine Demonstration<BR>Say Hello!</TD></CENTER>
<TD><img src="rayAWIPS.jpg" align=middle></TD></TR>
<TR><TD COLSPAN=3><CENTER>
This page creates the JS uEngine script prior to submitting the form request.
</CENTER></TD></TR>
</table>

<form name=hello method=post action="JavaScript:makeSubmit();">
<input type=hidden name=requesttype value=javascript>
<input type=hidden name=datatype value=ascii>
<input type=hidden name=function value=execute>
<input type=hidden name=receiveTime value=60000>
<input type=hidden name="actionXML">
<input type=hidden name="Hello World">
<table align=center>
<TR><TH>Enter your message:</TH></TR>
<TR><TD>
<input type=text name=message size=75 maxlength=255>
</TD></TR>
<TR><TD>
<input type="submit" value="Execute">
<input type=reset>
</TD></TR>
</table>
</form>
</body>
</html>