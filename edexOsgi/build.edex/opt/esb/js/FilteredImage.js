/**
 * Performs an enhancement of a satellite image. The image is enhanced in two
 * ways. 
 *   1) the high and low pixel values are set to the background value, and
 *   2) a custom color table is applied to the image.
 *
 * This script may be subscribed using the EDEX subscription service.
 *
 * This script is designed to create a visualization product. It is not intended
 * to be embedded within another uEngine script.
 *
 * Usage:
 *  This script creates a filtered image product from an IR image over East CONUS. 
 *    include("FilteredImage.js");
 *    var dataRequest = new FilteredImage();
 *    dataRequest.addParameter("satellite","GOES");
 *    dataRequest.addParameter("area_subtype","East CONUS");
 *    dataRequest.addParameter("parameter","Imager 11 micron IR");
 *    dataRequest.setCount(1);
 *    dataRequest.setColormap("IREnhanced");
 *    dataRequest.remapImage(64,192);
 *    dataRequest.setSortValue("valid_time");
 *    dataRequest.execute();
 */

/**
 * Class Constructor.
 */
function FilteredImage() {
    this.format = "png";
    this.colormap = "BW";
    this.reproject = false;
    this.plugin = "satellite";
    this.low = 0;
    this.high = 255;
    this.remap = false;
    this.subscribe = false;
    this.subscription = null;
    this.queryResults = null;
    this.createImage = false;
    this.sortValue = "";
    this.query = new TermQuery(this.plugin, subscriptionDataFieldId, subscriptionDataQueryId);
}
/**
 * Main action method. Performs the image creation.
 *
 * @return (String) XML string containing the result of the execution.
 */
function _execute()
{
    if(this.subscribe){
      this.subscription = new Subscription();
      this.subscription.setup(this.query);
    }
	
	var queryResults = this.query.execute();
    if(queryResults == null || queryResults.size() == 0)
    {
        response = new MakeResponseNull("Query returned 0 results.",this.query);
        return response.execute();
    }
	else
	{
		return this.makeImageResponse(queryResults);
	}
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

/* helper methods */
function _makeImageResponse(queryResults){
  var response = new Array();
  for(i=0; i < queryResults.size(); i++)
  {
    var currentQuery = queryResults.get(i);
    var geom = currentQuery.getCoverage().getMapGeom();
    var crs = currentQuery.getCoverage().getCrs();
    var fileIn = new FileIn(this.plugin, currentQuery);
    var record = fileIn.execute();
    var colorMap = null;
    if (this.remap) {
	    colorMap = new ColorMapImage(this.colormap, MEUtils.byteHighLowFilterImage(record.getDataObject(),this.low,this.high), geom);
    } else {
	    colorMap = new ColorMapImage(this.colormap, record.getDataObject(), geom);
    }
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
function _remapImage(low,high) {
	this.low = low;
	this.high = high;
	this.remap = true;
}

/* map the functions to the class prototype */
FilteredImage.prototype.remapImage = _remapImage;
FilteredImage.prototype.execute = _execute;
FilteredImage.prototype.makeImageResponse = _makeImageResponse;
FilteredImage.prototype.addParameter = _addParameter;
FilteredImage.prototype.addList = _addList;
FilteredImage.prototype.setCount = _setCount;
FilteredImage.prototype.enableSubscription = _enableSubscription;
FilteredImage.prototype.reprojectImage = _reprojectImage;
FilteredImage.prototype.setColormap = _setColormap;
FilteredImage.prototype.setFormat = _setFormat;
FilteredImage.prototype.setSortValue = _setSortValue;
FilteredImage.prototype.requestImage = _requestImage;
