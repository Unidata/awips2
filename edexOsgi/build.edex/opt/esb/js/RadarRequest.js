function RadarRequest(){
  this.plugin = "radar";
  this.subscribe = false;
  this.subscription = null;
  this.queryResults = null;
  this.createImage = false;
  this.reproject = false;
  this.colormap = "BW";
  this.format = "png";
  this.query = new TermQuery(this.plugin, subscriptionDataFieldId, subscriptionDataQueryId);
}

function _addParameter(name, value, operand){
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
    var fileIn = new FileIn(this.plugin, currentQuery);
    var records = fileIn.retrieveGroup();
    var radarImage = new DecodeRadarImage(currentQuery, records);

    var geom = radarImage.getGridGeometry();
    var colorMap = new ColorMapImage(this.colormap, radarImage.execute(), geom);
    
    var imageOut = null;
    if(this.reproject){
      var crs = radarImage.getCrs();
      var reproject = new ReprojectImage(colorMap.execute(), geom, crs);
      var reprojectedImage = reproject.execute();
      imageOut = new ImageOut(reprojectedImage, this.format, reproject.getGridGeometry());
    }
    else
    {
      imageOut = new ImageOut(colorMap.execute(), this.format, geom);
    }

    var fileOut = new FileOut(imageOut.execute(), this.format);
    var writeFile = fileOut.execute();
    var makeResponse = new MakeResponseUri(writeFile, null, currentQuery.getDataURI(), this.format);
    response[i] = makeResponse.execute();
  }
  return response;
}

RadarRequest.prototype.execute = _execute;
RadarRequest.prototype.makeXmlResponse = _makeXmlResponse;
RadarRequest.prototype.makeImageResponse = _makeImageResponse;
RadarRequest.prototype.addParameter = _addParameter;
RadarRequest.prototype.addList = _addList;
RadarRequest.prototype.setCount = _setCount;
RadarRequest.prototype.enableSubscription = _enableSubscription;
RadarRequest.prototype.reprojectImage = _reprojectImage;
RadarRequest.prototype.setColormap = _setColormap;
RadarRequest.prototype.setFormat = _setFormat;
RadarRequest.prototype.setSortValue = _setSortValue;
RadarRequest.prototype.requestImage = _requestImage;
