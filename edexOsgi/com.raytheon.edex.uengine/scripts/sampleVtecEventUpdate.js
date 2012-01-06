/**
 * Performs an update of a single VTN.
 *
 * Change the values in the "setFields" method 
 * to control the actual VTN being updated.  
 */
include("VtecEventUpdate.js");
var runner = new VtecUpdater();
runner.setTimeOut(10);
runner.setClientID("VTEC Update");
runner.setFields("KOAX","BZ","W",1);
runner.execute();