/* the class constructor */
function HelloWorld() {
  this.message = "";
}

/* class methods */
function _execute() {
  var logger = new SystemLog();
  logger.log("info",this.message);
  /* empty response */
  var response = new MakeResponseNull("Hello World.",new TermQuery("obs"));
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
runner.setMessage("Hello World");
runner.execute();
