include("ObsRequest.js");
include("TafRequest.js");

var oQuery = new ObsRequest();
var tQuery = new TafRequest();

function ObAndTaf(count,icaos) {
  var icao = "KOFF";
  oQuery.setCount(count);
  oQuery.setSortValue("timeobs");
  oQuery.addParameter("reporttype","METAR");
  if (icaos != null) {
    oQuery.addList("stationid",icaos);
  } else {
    oQuery.addParameter("stationid",icao);
  }
  tQuery.setCount(count);
  if (icaos != null) {
    tQuery.addList("stationid",icaos);
  } else {
    tQuery.addParameter("stationid",icao);
  }
  tQuery.setSortValue("issue_time");
  return combineResponses(oQuery.execute(),tQuery.execute());
}
function combineResponses(respA,respB) {
  var retVal = new Array();
  var ptr = 0;
  if (respA != null && respA.length > 0) {
    for (i = 0; i < respA.length; i++) {
      retVal[ptr] = respA[i];
      ptr++;
    }
  } else {
    retVal[ptr] = respA;
    ptr++;
  }
  if (respB != null && respB.length > 0) {
    for (i = 0; i < respB.length; i++) {
      retVal[ptr] = respB[i];
      ptr++;
    }
  } else {
    retVal[ptr] = respB;
    ptr++
  }
  return retVal;
}
function makeError(message,query) {
   var response = new MakeResponseNull(message,query);
   return response.execute();
}
ObAndTaf(3,"KSDF,KGAG,KIAH,KLBE,KLUK");