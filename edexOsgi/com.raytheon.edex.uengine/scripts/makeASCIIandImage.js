include("SatelliteRequest.js");
include("ObsRequest.js");
function ASCIIandImage() {
  var sat = new SatelliteRequest();
  sat.setCount(1);
  sat.addParameter("product_type","Satellite");
  sat.addParameter("datatype","Images");
  sat.addParameter("area_subtype","East CONUS");
  sat.addParameter("goes","Imager 11 micron (IR)");
  sat.setSortValue("valid_time");
  sat.setFormat("png");
  sat.setColormap("IREnhanced");
  sat.requestImage(true);
  var obs = new ObsRequest();
  obs.setCount(1);
  obs.addParameter("reporttype","METAR");
  obs.setSortValue("timeobs");
  return combineResponses(obs.execute(),sat.execute());
}
function combineResponses(respA,respB) {
  var retVal = new Array();
  var ptr = 0;
  if (respA != null && respA.length > 0) {
    for (i = 0; i < respA.length; i++) {
      retVal[ptr] = respA[i];
      ptr++;
    }
  }
  if (respB != null && respB.length > 0) {
    for (i = 0; i < respB.length; i++) {
      retVal[ptr] = respB[i];
      ptr++;
    }
  }
  return retVal;
}

ASCIIandImage();