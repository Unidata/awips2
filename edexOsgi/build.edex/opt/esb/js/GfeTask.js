function GfeTask(wsId,task){
	this.task = task;
	this.wsId = wsId;
}

function _addArgument(xml){
	this.task.addXml(xml);
}

function _execute(){
	this.task.setWorkstationID(this.wsId);
	var result = this.task.execute();

	if(result==null){
		return new MakeResponseNull("GFE task generated NULL response",null).execute();
	}else{
		var count = result.size();
		var response = new Array(count);
		for(i = 0; i < count; i++){
			var makeResponse = new MakeResponseXml(result.get(i));
			response[i] = makeResponse.execute();
		}
		return response;
	}

}

GfeTask.prototype.addArgument = _addArgument;
GfeTask.prototype.execute = _execute;
