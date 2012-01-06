function GridRequest(){
  this.plugin = "grib";
  this.subscribe = false;
  this.subscription = null;
  this.queryResults = null;
  this.createImage = false;
  this.reproject = false;
  this.colormap = "BW";
  this.format = "png";
  this.scaleFactor = 1.0;
  this.longitude = -91.0;
  this.latitude = 42.0;
  this.slice = false;
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

function _setScaleFactor(scale){
  this.scaleFactor = scale;
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
function _requestSlice(slice){
  this.createSlice = slice;
}

function _setLatitude(latitude){
  	this.latitude = latitude;
}
function _setLongitude(longitude){
	this.longitude = longitude;
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
    var response = new MakeResponseNull("Query returned 0 results.",this.query);
    return response.execute();
  }
  else
  {
  
  	if(this.createSlice) {
      return this.makegridslicerResponse();
    }

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
    var currentQuery = this.queryResults.get(i);
    var fileIn = new FileIn(this.plugin, currentQuery);
    var extractor = new GribExtractData(fileIn.execute(),currentQuery);
    extractor.execute();
    
    var makeResponse = new MakeResponseXml(currentQuery);
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
    var extractor = new GribExtractData(fileIn.execute(),currentQuery);    
    var gribMap = new GribMap(this.plugin, this.colormap, extractor.execute(), geom);
    gribMap.setScaleFactor(this.scaleFactor);
    var imageData = gribMap.execute();
    geom = gribMap.getGridGeometry();
    var colorMap = new ColorMapImage(this.colormap, imageData, geom);
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

GridRequest.prototype.execute = _execute;
GridRequest.prototype.makeXmlResponse = _makeXmlResponse;
GridRequest.prototype.makeImageResponse = _makeImageResponse;
GridRequest.prototype.addParameter = _addParameter;
GridRequest.prototype.addList = _addList;
GridRequest.prototype.setCount = _setCount;
GridRequest.prototype.setScaleFactor = _setScaleFactor;
GridRequest.prototype.enableSubscription = _enableSubscription;
GridRequest.prototype.reprojectImage = _reprojectImage;
GridRequest.prototype.setColormap = _setColormap;
GridRequest.prototype.setFormat = _setFormat;
GridRequest.prototype.setSortValue = _setSortValue;
GridRequest.prototype.requestImage = _requestImage;
GridRequest.prototype.setLatitude = _setLatitude;
GridRequest.prototype.setLongitude = _setLongitude;
