function StopLightMaker(){
  this.plugin = "grib";
  this.subscribe = false;
  this.subscription = null;
  this.queryResults = null;
  this.reproject = false;
  this.colormap = "StopLight";
  this.format = "png";
  this.scaleFactor = 3.0;
  this.query = new TermQuery(this.plugin, subscriptionDataFieldId, subscriptionDataQueryId);
}

function _addParameter(name, value){
  this.query.addParameter(name, value);
}

function _addList(name, value){
  this.query.addParameter(name, value, "in");
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
    return this.makeImageResponse();
  }
}

function _makeImageResponse(){
  var response = new Array();
  for(i=0; i < this.queryResults.size(); i++)
  {  
    var currentQuery = this.queryResults.get(i);
    var geom = currentQuery.getGrid().getGridGeom();
    var crs = currentQuery.getGrid().getCrs();
    var fileIn = new FileIn(this.plugin, currentQuery);
    var stopLight = new StopLightImage(fileIn.execute());
    var gribMap = new GribMap(this.plugin, this.colormap, stopLight.execute(), geom);
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
      imageOut = new ImageOut(colorMap.execute(), this.format);
    }
    var fileOut = new FileOut(imageOut.execute(), this.format);
    var writeFile = fileOut.execute();
    var makeResponse = new MakeResponseUri(writeFile, null, currentQuery.getDataURI(), this.format);
    response[i] = makeResponse.execute();
  }
  return response;
}

StopLightMaker.prototype.execute = _execute;
StopLightMaker.prototype.makeImageResponse = _makeImageResponse;
StopLightMaker.prototype.addParameter = _addParameter;
StopLightMaker.prototype.addList = _addList;
StopLightMaker.prototype.setCount = _setCount;
StopLightMaker.prototype.setScaleFactor = _setScaleFactor;
StopLightMaker.prototype.enableSubscription = _enableSubscription;
StopLightMaker.prototype.reprojectImage = _reprojectImage;
StopLightMaker.prototype.setColormap = _setColormap;
StopLightMaker.prototype.setFormat = _setFormat;
StopLightMaker.prototype.setSortValue = _setSortValue;
StopLightMaker.prototype.requestImage = _requestImage;

var dataRequest = new StopLightMaker();
dataRequest.addParameter("paramid","Temperature");
dataRequest.addParameter("levelinfo","2.0_m");
dataRequest.addParameter("forecasttime","0");
dataRequest.addParameter("gridid",212);
dataRequest.setCount(1);
dataRequest.setColormap("StopLight");
dataRequest.setSortValue("basetime");
dataRequest.execute();