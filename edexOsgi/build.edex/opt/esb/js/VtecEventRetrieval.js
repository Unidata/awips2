/**
 * Performs a VTEC query to request an Event Tracking object for a specific VTEC
 * event at a specified office.
 *
 * This script performs complete product generation. As such, it is not
 * intended to be called from other scripts.
 *
 * Implementation Note:
 *  This script uses a single uEngine task to perform its work.
 *    1) VtecUpdateEvent: used to send the VTEC request to the VTEC server
 *       and receive the results. The results are returned as a single
 *       Response message. As such, no additional processing is required.  
 *
 * Usage:
 *  This is a sample script that retrieves the VTN for a Blizard ("BZ")
 *  warning ("W") at "KOAX".
 *    include("VtecEventRetrieval.js");
 *    var runner = new VtecGetEvent();
 *    runner.setTimeOut(10);
 *    runner.setClientID("VTEC Update");
 *    runner.setFields("KOAX","BZ","W");
 *    runner.execute();
 */
/* constructor */
function VtecGetEvent() {
  this.query = new VtecEventQuery();
  this.setTimeOut(10);
}

/* main action function */
function _execute() {
  return this.query.execute();
}

/* 
 * main setter - handles required fields
 *
 * @param site the ICAO of the WFO
 * @param phenom the VTEC phenomenon
 * @param level the VTEC significance
 */
function _setFields(site,phenom,level) {
  this.addField(this.query.SITE,site);
  this.addField(this.query.PHENOM,phenom);
  this.addField(this.query.LEVEL,level);
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
VtecGetEvent.prototype.setTimeOut = _setTimeOut;
VtecGetEvent.prototype.setClientID = _setClientID;
VtecGetEvent.prototype.setFields = _setFields;
VtecGetEvent.prototype.addField = _addField;
VtecGetEvent.prototype.addProperty = _addProperty;
VtecGetEvent.prototype.execute = _execute;
