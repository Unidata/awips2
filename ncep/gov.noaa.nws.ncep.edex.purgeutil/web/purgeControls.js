function PurgeControls(){
	alert("Purgecontrol created");
    this.tbName = document.getElementById("tbName");		
	this.purgeResult = document.getElementById("result");
	this.purgeResult.value = "";
	this.tbName.value = "";
}
PurgeControls.prototype.purge = function(){
    		var inputtbName = this.tbName.value;
    		var self = this;
		self.processResult("Purging! May take a while!");
    		DbPurge.purgeDbTable(inputtbName, function(data) { self.processResult(data); });
}

PurgeControls.prototype.processResult = function(data){
	this.purgeResult.value = data;
}

PurgeControls.prototype.reset = function(data){
	this.purgeResult.value = "";
	this.tbName.value = "";
}
