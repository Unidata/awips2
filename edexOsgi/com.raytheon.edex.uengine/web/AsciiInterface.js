function AsciiInterface(defaultMode){
  this.asciiForm = document.getElementById("asciiForm");
  this.location = document.getElementById("ascLocation");
  this.type = document.getElementById("ascType");
  this.time = document.getElementById("ascTime");
  this.count = document.getElementById("ascCount");
  this.timeout = document.getElementById("ascTimeout");
  
  this.request = document.getElementById("requestViewBox");
  
  this.hasInit = false;
}

AsciiInterface.prototype.init = function(){
  if(!this.hasInit){
    this.getData("location");
    this.hasInit = true;
  }
}

AsciiInterface.prototype.getData = function(type){
  this.disableControls();
  var self = this;
  var ascType = this.type.options[this.type.selectedIndex].value;
  if(ascType == "TAF"){
    switch(type){
      case "location":     
        var queryArray = {"plugin" : "taf"};
        RequestTestDriver.requestCatalog("stationId",queryArray,60000,function(output){self.processData(type,output);});
        break;
      case "time":
        var location = this.location.options[this.location.selectedIndex].value;
        var queryArray = {"plugin" : "taf" , "stationId" : location};
        RequestTestDriver.requestCatalog("dataTime",queryArray,60000,function(output){self.processData(type,output);});
        break;
    }
  }
  else{
    switch(type){
      case "location":     
        var queryArray = {"plugin" : "obs" , "reportType" : ascType};
        RequestTestDriver.requestCatalog("location.stationId",queryArray,60000,function(output){self.processData(type,output);});
        break;
      case "time":
        var location = this.location.options[this.location.selectedIndex].value;
        var queryArray = {"plugin" : "obs" , "location.stationId" : location , "reportType" : ascType};
        RequestTestDriver.requestCatalog("dataTime",queryArray,60000,function(output){self.processData(type,output);});
        break;
    }
  }
}

AsciiInterface.prototype.processData = function(type,output){
  if(output == null || output.length == 0){
    alert("The ascii index appears to be empty.  Please ingest data and try again.");
    this.location.options.length = 0;
    this.time.options.length = 0;
    this.hasInit = false;
    this.enableControls();
    return false;
  }
  output.sort();
  switch(type){
  case "location":
    this.location.options.length = 0;
    for(var i = 0; i < output.length; i++){
      this.location.options[i] = new Option(output[i],output[i]);
    }
    if(this.location.options.length > 0){
      this.getData("time");
    }
    else{
      this.enableControls();
    }
    break;
  case "time":
    this.time.options.length = 0;
    for(var i = 0; i < output.length; i++){
      this.time.options[i] = new Option(output[i],output[i]);
    }
    output = null;
    if(this.time.options.length > 0){
      this.createScript();
    }
    else{
      this.enableControls();
    }
    break;
  }
  output = null;
}

AsciiInterface.prototype.createScript = function(){
  if(this.location.options.length == 0){
    return false;
  }
  this.disableControls();
  var location = this.location.options[this.location.selectedIndex].value;
  var type = this.type.options[this.type.selectedIndex].value;
  var time = this.time.options[this.time.selectedIndex].value;
  var count = this.count.options[this.count.selectedIndex].value;
  var engine = document.getElementById("scriptInterface").checked ? "xmljs" : "xml";
  var self = this;
  if(type == "TAF"){
    var queryArray = {"pluginName" : "taf", "stationId" : location , "dataTime" : time};
  } else {
    var queryArray = {"pluginName" : "obs" , "reportType" : type , "location.stationId" : location , "dataTime" : time};
  }
    ScriptTestDriver.createScript(queryArray,count,engine,function(output){self.processScript(output);})
}

AsciiInterface.prototype.getTimeout = function(){
  var timeout = this.timeout.options[this.timeout.selectedIndex].value;
  return timeout;
}

AsciiInterface.prototype.processScript = function(output){
  this.request.value = output;
  output = null;
  this.enableControls();
}

AsciiInterface.prototype.disableControls = function(){
  for(var i = 0; i < this.asciiForm.length; i++){
    this.asciiForm[i].disabled = true;
  }
}

AsciiInterface.prototype.enableControls = function(){
  for(var i = 0; i < this.asciiForm.length; i++){
    this.asciiForm[i].disabled = false;
  }
}
