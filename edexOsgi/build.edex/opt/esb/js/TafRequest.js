function TafRequest() {
  this.plugin = "taf";
  this.subscribe = false;
  this.subscription = null;
  this.queryResults = null;
  this.asciiResponse = false;
  this.query = new TermQuery(this.plugin, subscriptionDataFieldId, subscriptionDataQueryId);
}

function _addParameter(name,value,operand) {
  	if(arguments.length==2) {
		this.query.addParameter(name,value);
	} else {
		this.query.addParameter(name,value,operand);
	}
}

function _addList(name, value) {
  this.query.addList(name, value);
}

function _setCount(count) {
  this.query.setCount(count);
}

function _setSortValue(sortValue) {
  this.query.setSortBy(sortValue);
}

function _enableSubscription() {
  this.subscribe = true;
}

function _enableAsciiResponse() {
    this.asciiResponse = true;
}

function _execute() {
  if(this.subscribe){
    this.subscription = new Subscription();
    this.subscription.setup(this.query);
  }
  this.queryResults = this.query.execute();
  if(this.queryResults == null || this.queryResults.size() == 0) {
  	var response = new MakeResponseNull("Query returned 0 results.",
  	                                        this.query);
    return response.execute();
  } else if (this.asciiResponse) {
    return this.makeAsciiResponse();
  } else {
    return this.makeXmlResponse();
  }
}

function _makeAsciiResponse() {
  var response = new Array();
  for (i = 0; i < this.queryResults.size(); i++) {
    var result = this.queryResults.get(i);
    var toXml = new DataToXml(result);
    var xmlString = toXml.execute();
    var makeResponse = new MakeResponseAscii(result,xmlString);
    response[i] = makeResponse.execute();
  }
  return response;
}
function _makeXmlResponse() {
  var response = new Array();
  for(i=0; i < this.queryResults.size(); i++) {
    var makeResponse = new MakeResponseXml(this.queryResults.get(i));
    response[i] = makeResponse.execute();
  }
  return response;
}

TafRequest.prototype.execute = _execute;
TafRequest.prototype.makeXmlResponse = _makeXmlResponse;
TafRequest.prototype.makeAsciiResponse = _makeAsciiResponse;
TafRequest.prototype.addParameter = _addParameter;
TafRequest.prototype.addList = _addList;
TafRequest.prototype.setCount = _setCount;
TafRequest.prototype.setSortValue = _setSortValue;
TafRequest.prototype.enableSubscription = _enableSubscription;
TafRequest.prototype.enableAsciiResponse = _enableAsciiResponse;