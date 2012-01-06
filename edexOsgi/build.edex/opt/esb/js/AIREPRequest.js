/**
 * Performs a TermQuery for AIREP data.
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
function AIREPRequest(){
  this.plugin = "airep";
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
    }  }
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

AIREPRequest.prototype.execute = _execute;
AIREPRequest.prototype.makeXmlResponse = _makeXmlResponse;
AIREPRequest.prototype.makeAsciiResponse = _makeAsciiResponse;
AIREPRequest.prototype.addParameter = _addParameter;
AIREPRequest.prototype.addList = _addList;
AIREPRequest.prototype.setCount = _setCount;
AIREPRequest.prototype.setSortValue = _setSortValue;
AIREPRequest.prototype.enableSubscription = _enableSubscription;
AIREPRequest.prototype.enableAsciiResponse = _enableAsciiResponse;
