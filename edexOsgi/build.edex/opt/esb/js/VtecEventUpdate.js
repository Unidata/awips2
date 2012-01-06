/**
 * Performs a VTEC request to update the VTN for a specific VTEC Event
 * at an office.
 *
 * This script performs a complete product generation. As such, it is not
 * intended to be called from other scripts.
 *
 * Implementation Note:
 *  This script uses a single uEngine task to perform its work.
 *    1) VtecUpdateEvent: used to send the VTEC request to the VTEC server
 *       and receive the results. The results are returned as a single
 *       Response message. As such, no additional processing is required.  
 *
 * Usage:
 *  This is a sample script that updates the VTN for a Blizard ("BZ")
 *  warning ("W") at "KOAX". The new VTN will be 1.
 *    include("VtecEventUpdate.js");
 *    var runner = new VtecUpdater();
 *    runner.setTimeOut(10);
 *    runner.setClientID("VTEC Update");
 *    runner.setFields("KOAX","BZ","W",1);
 *    runner.execute();
 */
/* constructor */
function VtecUpdater() {
  this.query = new VtecUpdateEvent();
  this.setTimeOut(10);
}

/* main action method */
function _execute() {
  return this.query.execute();
}

function _setFields(site,phenom,level,vtn) {
  this.addField(this.query.SITE,site);
  this.addField(this.query.PHENOM,phenom);
  this.addField(this.query.LEVEL,level);
  this.addField(this.query.VTN,vtn);
}
// initializers
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
 * Adds a field key/value pair to the example object.
 */
function _addField(name,value) {
  this.query.addField(name,value);
}

// setters
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

/* map objects to object prototype */
VtecUpdater.prototype.setTimeOut = _setTimeOut;
VtecUpdater.prototype.setClientID = _setClientID;
VtecUpdater.prototype.setFields = _setFields;
VtecUpdater.prototype.addField = _addField;
VtecUpdater.prototype.addProperty = _addProperty;
VtecUpdater.prototype.execute = _execute;
