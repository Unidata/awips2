/**
 * Performs a simple VTEC Request and returns the result to the requestor.
 * 
 * Request the P-VTEC Event Group "Action" tokens.
 */
include("VtecRetrieval.js");
var runner = new VtecRetrieval();
runner.setTimeOut(10);
runner.initialize("named","action");
runner.addProperty("count","0");
runner.setClientID("VTEC REQUEST");
runner.execute();
