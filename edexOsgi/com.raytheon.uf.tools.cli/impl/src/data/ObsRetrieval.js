include("BaseRequest.js");
var dataRequest = new BaseRequest("obs");
dataRequest.setCount(%COUNT%);
dataRequest.addParameter("reportType", "METAR");
dataRequest.addParameter("location.stationId", "%STATION%");
dataRequest.execute();