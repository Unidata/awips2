/**
 * Performs a simple VTEC Request and returns the result to the requestor.
 * 
 * Request the P-VTEC Event Group "Action" tokens.
 */
include("VtecObjectRetrieval.js");
var runner = new VtecRetrieval("action");
runner.setTimeOut(10);
runner.initialize("named","VTEC Action");
runner.addProperty("count","0");
runner.addField("name","NEW");
runner.setClientID("VTEC REQUEST");
runner.execute();
