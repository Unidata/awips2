function SatelliteRequest(count, sat, image) {
	this.satellite = sat;
	this.area = "East CONUS";
	this.count = count;
	this.makeImage = image;
    this.format = "png";
    this.colormap = "IREnhanced";
    this.reproject = false;
    this.plugin = "satellite";
}

function _execute()
{
	var query = new TermQuery("satellite");
	query.addParameter("satellite", this.satellite);
	query.addParameter("area_subtype", this.area);
	query.setCount(this.count);
	var queryResults = query.execute();
    if(queryResults == null || queryResults.size() == 0)
    {
        response = new MakeResponseNull("Query returned 0 results.",query);
        return response.execute();
    }
	else
	{
		if (!this.makeImage)
		{
			return this.makeXmlResponse(queryResults);
		} else {
			return this.makeImageResponse(queryResults);
		}
	}
}

function _makeXmlResponse(queryResults)
{
	var count = queryResults.size();
	var response = new Array(count);
	for(i=0; i < count; i++)
	{
		var dataToXml = new DataToXml(queryResults.get(i));		
		var makeResponse = new MakeResponseXml(dataToXml.execute());
		response[i] = makeResponse.execute();
	}

	return response;
}

function _makeImageResponse(queryResults){
  var response = new Array();
  for(i=0; i < queryResults.size(); i++)
  {
    var currentQuery = queryResults.get(i);
    var geom = currentQuery.getCoverage().getMapGeom();
    var crs = currentQuery.getCoverage().getCrs();
    var fileIn = new FileIn(this.plugin, currentQuery);
    var record = fileIn.execute();
    var colorMap = new ColorMapImage(this.colormap, record.getDataObject(), geom);
    var imageOut = null;
    if(this.reproject){
      var reproject = new ReprojectImage(colorMap.execute(), geom, crs);
      var reprojectedImage = reproject.execute();
      imageOut = new ImageOut(reprojectedImage, this.format, reproject.getGridGeometry());
    }
    else
    {
      imageOut = new ImageOut(colorMap.execute(), this.format);
    }
    var fileOut = new FileOut(imageOut.execute(), this.format);
    var writeFile = fileOut.execute();
    var makeResponse = new MakeResponseUri(writeFile, null, currentQuery.getDataURI(), this.format);
    response[i] = makeResponse.execute();
  }
  return response;
}

SatelliteRequest.prototype.execute = _execute;
SatelliteRequest.prototype.makeXmlResponse = _makeXmlResponse;
SatelliteRequest.prototype.makeImageResponse = _makeImageResponse;

// Code the user writes:
var dataRequest = new SatelliteRequest(4, "GOES",true);
dataRequest.execute();