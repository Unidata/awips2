/**
 * uEngine implementation of the clasic "Hello World" program.
 * When executed by the uEngine, the message set but the client
 * is logged to the Mule system log as an "info" level log entry.
 * The message is also eche'd back to the client as part of the
 * response message.
 *
 * This class represents the mid tier of the 3-tier scripting
 * model used by uEngine. 
 *
 * Usage:
 *   include("HelloWorld.js");
 *   var runner = new HelloWorld();
 *   runner.setMessage("Hello World from Omaha.");
 *   runner.execute();
 *
 */
/* the class constructor */
function HelloWorld() {
  this.message = "";
}

/* class methods */
function _execute() {
  var logger = new SystemLog();
  logger.log("info",this.message);
  /* empty response */
  var response = new MakeResponseGeneric(this.message);
  return response.execute();
}

function _setMessage(text) {
  this.message = text;
}

/* attach methods to class - w/aliases */
HelloWorld.prototype.execute = _execute;
HelloWorld.prototype.setMessage = _setMessage;