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
runner.setCommandLine("D:/bin/TestScript.bat This is a test");
runner.execute();