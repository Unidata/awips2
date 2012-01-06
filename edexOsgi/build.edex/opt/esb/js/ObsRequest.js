function ObsRequest(){
  this.plugin = "obs";
  this.subscribe = false;
  this.subscription = null;
  this.queryResults = null;
  this.spatial = new SpatialQuery;
  this.asciiResponse = false;
  
  this.upperLeftLat = null;
  this.upperLeftLon = null;
  this.lowerRightLat = null;
  this.lowerRightLon = null;
  
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

function _setSortValue(sortValue){
  this.query.setSortBy(sortValue);
}

function _enableSubscription(){
  this.subscribe = true;
}

function _enableAsciiResponse(){
  this.asciiResponse = true;
}

function _execute()
{
  if(this.subscribe){
    this.subscription = new Subscription();
    this.subscription.setup(this.query);
  }
  
  var stations = null;
  var icaos = new Array();
  if(this.spatial!=null){
	stations=this.spatial.execute();

	if(stations!=null){
		
		for(i=0;i<stations.size();i++){
			
			icaos[i]=stations.get(i).getIcao();
		}
	
  		if(icaos.length>0){
  			this.addParameter("stationID",icaos,"in");
  		}
  	}
  	
  }
  
  this.queryResults = this.query.execute();
  if(this.queryResults == null || this.queryResults.size() == 0)
  {
  	var response = new MakeResponseNull("Query returned 0 results.",
  	                                        this.query);
    return response.execute();
  }
  else
  {
    if(this.asciiResponse)
    {
       return this.makeAsciiResponse();
    }
    else
    {
       return this.makeXmlResponse();
    }  }
}

function _setSpatialBox(ulLat,ulLon,lrLat,lrLon){
  this.spatial.setUpperLeftLat(ulLat);
  this.spatial.setUpperLeftLon(ulLon);
  this.spatial.setLowerRightLat(lrLat);
  this.spatial.setLowerRightLon(lrLon);
}

function _makeXmlResponse()
{
  var xmlResults = new Array();
  var response = new Array();
  for(i=0; i < this.queryResults.size(); i++)
  {    
    var makeResponse = new MakeResponseXml(this.queryResults.get(i));
    response[i] = makeResponse.execute();
  }
  return response;
}

function _makeAsciiResponse()
{
  var xmlResults = new Array();
  var response = new Array();
  for(i=0; i < this.queryResults.size(); i++)
  {    
    var toXml = new DataToXml(this.queryResults.get(i)); 
    xmlResults[i] = toXml.execute();     
	var makeResponse = new MakeResponseAscii(this.queryResults.get(i), xmlResults[i]);
	response[i] = makeResponse.execute(); 
  }
  return response;
}

ObsRequest.prototype.execute = _execute;
ObsRequest.prototype.makeXmlResponse = _makeXmlResponse;
ObsRequest.prototype.makeAsciiResponse = _makeAsciiResponse;
ObsRequest.prototype.addParameter = _addParameter;
ObsRequest.prototype.addList = _addList;
ObsRequest.prototype.setCount = _setCount;
ObsRequest.prototype.setSortValue = _setSortValue;
ObsRequest.prototype.enableSubscription = _enableSubscription;
ObsRequest.prototype.enableAsciiResponse = _enableAsciiResponse;
ObsRequest.prototype.setSpatialBox = _setSpatialBox;