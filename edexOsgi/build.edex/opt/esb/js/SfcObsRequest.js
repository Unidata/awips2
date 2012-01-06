/**
 * Performs a TermQuery for SfcObs (Surface Synoptic) data.
 * 
 * Usage:
 *   TBD
 *   
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080105            713 jkorman     Initial Coding.
 */
function SfcObsRequest(){
  this.plugin = "sfcobs";
  this.subscribe = false;
  this.subscription = null;
  this.queryResults = null;
  this.asciiResponse = false;
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
    }
  }
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

SfcObsRequest.prototype.execute = _execute;
SfcObsRequest.prototype.makeXmlResponse = _makeXmlResponse;
SfcObsRequest.prototype.makeAsciiResponse = _makeAsciiResponse;
SfcObsRequest.prototype.addParameter = _addParameter;
SfcObsRequest.prototype.addList = _addList;
SfcObsRequest.prototype.setCount = _setCount;
SfcObsRequest.prototype.setSortValue = _setSortValue;
SfcObsRequest.prototype.enableSubscription = _enableSubscription;
SfcObsRequest.prototype.enableAsciiResponse = _enableAsciiResponse;
