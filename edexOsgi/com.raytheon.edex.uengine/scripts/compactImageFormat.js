include("SatelliteRequest.js");
var dataRequest = new SatelliteRequest();
dataRequest.requestImage(true);
dataRequest.setCount(4);
dataRequest.addParameter("area_subtype","East CONUS");
dataRequest.setColormap("IREnhanced");
dataRequest.execute();
