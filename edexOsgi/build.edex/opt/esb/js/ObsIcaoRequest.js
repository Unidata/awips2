function ObsIcaoRequest(){
  this.plugin = "obsIcao";

  this.obsicao = new ObsIcaoQuery();
  this.icao = null;
}

function _addParameter(name,value,operand){
	this.icao = value;
}

function _execute()
{

  var station = null;
  var icaos = new Array();

  this.obsicao.setIcao(this.icao);
  station = this.obsicao.execute();

  if(station!=null){
  	var response = new Array();
  	response[0] = station;
  	var makeResponse = new MakeResponseXml(station);
  	response[0] = makeResponse.execute();
  		return response;
  } else {
  	var response = new MakeResponseNull("Query returned 0 results.",
  	                                        this.obsicao);
    return response.execute();
  	}
  	
}


function _makeXmlResponse()
{
  var xmlResults = new Array();
  var response = new Array();

  return response;
}

ObsIcaoRequest.prototype.execute = _execute;
ObsIcaoRequest.prototype.makeXmlResponse = _makeXmlResponse;
ObsIcaoRequest.prototype.addParameter = _addParameter;
