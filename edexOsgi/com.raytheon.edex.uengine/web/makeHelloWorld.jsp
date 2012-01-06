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
      if (form.message.value.length != 0) {
	      var script = "include(\"HelloWorld.js\");\n"
	                 + "var runner = new HelloWorld();\n"
	                 + "runner.setMessage(\"" + form.message.value + "\");\n"
	                 + "runner.execute();"
	      form.actionXML.value = script;
      }
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
Say Hello!
</H1></TD>
<TD><img src="rayAWIPS.jpg" align=middle></TD></TR>
<TR><TD COLSPAN=3><CENTER>
This page modifies the JS uEngine script prior to submitting the form request.
</CENTER></TD></TR>
</table>

<form name=hello method=post action="JavaScript:makeSubmit();"">
<input type=hidden name=requesttype value=javascript>
<input type=hidden name=sortby value=timeobs>
<input type=hidden name="Hello World">
<table align=center>
<tr><td colspan=2>
<textarea name="actionXML" cols="80" rows="22" style="background-color:aqua">
/* the class constructor */
function HelloWorld() {
  this.message = "";
}

/* class methods */
function _execute() {
  var logger = new SystemLog();
  logger.log("info",this.message);
  /* empty response */
  var response = new MakeResponseNull(this.message,new TermQuery("obs"));
  return response.execute();
}

function _setMessage(text) {
  this.message = text;
}

/* attach methods to class - w/aliases */
HelloWorld.prototype.execute = _execute;
HelloWorld.prototype.setMessage = _setMessage;

/* script to use the class */
var runner = new HelloWorld();
runner.setMessage("Put your message here!");
runner.execute();
</textarea>
</td></tr>
<tr><th colspan=2><em>Enter Values for Message</em></th></tr> 
<TR><TD>Enter message:</TD><TD>
<input type=text name=message size=75 maxlength=255>
</TD></TR>

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
<select name=receiveTime disabled>
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