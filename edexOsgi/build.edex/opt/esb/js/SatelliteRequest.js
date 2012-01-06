function SatelliteRequest(){
  this.plugin = "satellite";
  this.subscribe = false;
  this.subscription = null;
  this.queryResults = null;
  this.createImage = false;
  this.reproject = false;
  this.colormap = "BW";
  this.format = "png";
  this.sortValue = "";
  this.query = new TermQuery(this.plugin, subscriptionDataFieldId, subscriptionDataQueryId);
}

function _addParameter(name,value,operand){
  	if(arguments.length==2){
		this.query.addParameter(name,value);
	} else{
		this.query.addParameter(name,value,operand);
	}
}

function _addList(name, value){
  this.query.addList(name, value);
}

function _setCount(count){
  this.query.setCount(count);
}

function _enableSubscription(){
  this.subscribe = true;
}

function _reprojectImage(reproject){
  this.reproject = reproject;
}

function _setColormap(colormap){
  this.colormap = colormap;
}

function _setFormat(format){
  this.format = format;
}

function _setSortValue(sortValue){
  this.query.setSortBy(sortValue);
}

function _requestImage(image){
  this.createImage = image;
}

function _execute()
{
  if(this.subscribe){
    this.subscription = new Subscription();
    this.subscription.setup(this.query);
  }
  this.queryResults = this.query.execute();
  if(this.queryResults == null || this.queryResults.size() == 0)
  {
    response = new MakeResponseNull("Query returned 0 results.",this.query);
    return response.execute();
  }
  else
  {
    if(this.createImage){
      return this.makeImageResponse();
    }
    else{
      return this.makeXmlResponse();
    }  }
}

function _makeXmlResponse()
{
  var count = this.queryResults.size();
  var response = new Array(count);
  for(i=0; i < count; i++)
  {
    var makeResponse = new MakeResponseXml(this.queryResults.get(i));
    response[i] = makeResponse.execute();
  }
  return response;
}

function _makeImageResponse(){
  var response = new Array();
  for(i=0; i < this.queryResults.size(); i++)
  {
    var currentQuery = this.queryResults.get(i);
    var geom = currentQuery.getSpatialInfo().getMapGeom();
    var crs = currentQuery.getSpatialInfo().getCrsObject();
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
      imageOut = new ImageOut(colorMap.execute(), this.format,geom);
    }
    var fileOut = new FileOut(imageOut.execute(), this.format);
    var writeFile = fileOut.execute();
    var makeResponse = new MakeResponseUri(writeFile, null, currentQuery.getIdentifier(), this.format);
    response[i] = makeResponse.execute();
  }
  return response;
}

SatelliteRequest.prototype.execute = _execute;
SatelliteRequest.prototype.makeXmlResponse = _makeXmlResponse;
SatelliteRequest.prototype.makeImageResponse = _makeImageResponse;
SatelliteRequest.prototype.addParameter = _addParameter;
SatelliteRequest.prototype.addList = _addList;
SatelliteRequest.prototype.setCount = _setCount;
SatelliteRequest.prototype.enableSubscription = _enableSubscription;
SatelliteRequest.prototype.reprojectImage = _reprojectImage;
SatelliteRequest.prototype.setColormap = _setColormap;
SatelliteRequest.prototype.setFormat = _setFormat;
SatelliteRequest.prototype.setSortValue = _setSortValue;
SatelliteRequest.prototype.requestImage = _requestImage;
