/**
 * Performs a simple VTEC Request and returns the result to the requestor.
 *
 * This script performs a complete product generation. As such, it is not
 *  intended to be called from other scripts.
 *
 * Implementation Note:
 *  This script uses two uEngine tasks to perform its work:
 *    1) VtecQuery:       used to send the VTEC Request to the VTEC Server
 *                        and receive the results. The results are returned
 *                        to the script as an ArrayList, which is accessible
 *                        JavaScript.
 *    2) MakeResponseXml: used to convert each of the VTEC Request results
 *                        into a ResponseMessageXML object for return to the
 *                        script runner.
 *  Note the loop used to prepare the VTEC Request results into an array for
 *  return to the script runner. The ArrayList (from Java) is accessed using
 *  the get() method. The JavaScript Array object is handled as a normal
    array.
 *
 * Usage:
 *  This is a sample script that requests P-VTEC Event Group "Action" tokens
 *  from the VTEC Server and returns the list of available tokens to the client.
 *    include("VtecRetrieval.js");
 *    var runner = new VtecQuery();
 *    runner.setTimeOut(10);
 *    runner.initialize("named","action");
 *    runner.setClientID("VTEC Action");
 *    runner.addProperty("count","10");
 *    runner.execute();
 */ 

/* 
 * Class constructor.
 */
function VtecRetrieval() {
  this.query = new VtecQuery();
  this.setTimeOut(10);
  this.addProperty("count","0");
}

/* 
 * Main action method. Performs the VTEC request and
 * packages the results.
 *
 *@return (Object[]) Contains the results of the VTEC request.
 */
function _execute() {
  var results = this.query.execute();
  var response = new Array();
  for (i=0; i<results.size(); i++) {
   var makeResponse = new MakeResponseXml(results.get(i));
   response[i] = makeResponse.execute();
  }
  return response;
}

/*
 * Adds property to query object.
 *
 * @param key property name.
 * @param value property value. 
 */
function _addProperty(key,value) {
   this.query.addProperty(key,value);
}
/* 
 * Allows setting of several properties. The properties set
 * are the "request token", "class token", and "type token"
 * properties.
 *
 * @param aRequest value for the request token
 * @param aClass value for the class token
 */
function _initialize(aRequest,aClass) {
  this.addProperty("request",aRequest);
  this.addProperty("class",aClass);
}

/* class setters */
/*
 * Sets the time-out interval for the request.
 *
 * @param time timeout period in seconds.
 */
function _setTimeOut(time) {
  this.query.setTimeOut(time * 1000);
}
/*
 * Sets the Client ID for the request.
 *
 * @param clientID the Client ID
 */
function _setClientID(clientID) {
  this.query.setClientID(clientID);
}
/* add methods to class prototype */
VtecRetrieval.prototype.execute = _execute;
VtecRetrieval.prototype.setTimeOut = _setTimeOut;
VtecRetrieval.prototype.addProperty = _addProperty;
VtecRetrieval.prototype.initialize = _initialize;
VtecRetrieval.prototype.setClientID = _setClientID;
