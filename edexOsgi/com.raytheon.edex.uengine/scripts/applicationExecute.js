/* class constructor */
function ExecuteApplication() {
  this.timeout = 10;
  this.command = "";
}

/* main action method */
function _execute() {
  var executer = new ExecuteCommand(this.command,this.timeout);
  var result = executer.execute();
  return this.makeAsciiResponse(executer.execute());;
}

/* generates the ascii response */
function _makeAsciiResponse(result) {
  var response = new Array();
  var makeResponse = new MakeResponseXml(result);
  response[0] = makeResponse.execute();
  return response;
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
ExecuteApplication.prototype.makeAsciiResponse = _makeAsciiResponse;

/* short form of the script */
var runner = new ExecuteApplication();
runner.setTimeOut(10000);
/* for EDEX on Windows, change 'ps' to 'ipconfig' */
runner.setCommandLine("ps");
runner.execute();