function WarningRequest(){
  this.plugin = "warning";
  this.subscribe = false;
  this.subscription = null;
  this.queryResults = null;
  this.createImage = false;
  this.reproject = false;
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

function _setSortValue(sortValue){
  this.query.setSortBy(sortValue);
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
    return this.makeXmlResponse();
  }
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

WarningRequest.prototype.execute = _execute;
WarningRequest.prototype.makeXmlResponse = _makeXmlResponse;
WarningRequest.prototype.addParameter = _addParameter;
WarningRequest.prototype.addList = _addList;
WarningRequest.prototype.setCount = _setCount;
WarningRequest.prototype.enableSubscription = _enableSubscription;
WarningRequest.prototype.reprojectImage = _reprojectImage;
WarningRequest.prototype.setSortValue = _setSortValue;
