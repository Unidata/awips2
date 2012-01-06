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
<title>Execute External Process</title>
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
<TD><H1>&mu;Engine Demonstration<BR>AdapterSrv Demo Script</H1></TD>
<TD><img src="rayAWIPS.jpg" align=middle></TD></TR>
</table>

<P>
<form method=post action="runAction.jas">
<table align=center>
<tr><th>Enter/Edit the Action JS Script into the textbox and press Execute.</th></tr>
<tr><td>
<textarea name="actionXML" cols="80" rows="22" style="background-color:aqua">
/* class constructor */
function ExecuteApplication() {
  this.timeout = 10;
  this.command = "";
}

/* main action method */
function _execute() {
  var executer = new ExecuteCommand(this.command,this.timeout);
  var result = executer.execute();
  return result;
}

/* class setters */
function _setTimeOut(time) {
  this.timeout = time * 1000;
}
function _setCommandLine(commandLine) {
  this.command = commandLine;
}


/* add methods to class */
ExecuteApplication.prototype.execute = _execute;
ExecuteApplication.prototype.setTimeOut = _setTimeOut;
ExecuteApplication.prototype.setCommandLine = _setCommandLine;

/* short form of the script */
var runner = new ExecuteApplication();
runner.setTimeOut(10000);
runner.setCommandLine("D:/bin/hello.exe 0");
runner.execute();

</textarea>
</TD></TR>
</TABLE>
<br>
<div  align=center>
    <input type="submit" value="Execute">
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
