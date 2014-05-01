function SatelliteInterface(defaultMode){
  this.satelliteForm = document.getElementById("satelliteForm");
  this.location = document.getElementById("satLocation");
  this.parameter = document.getElementById("satParameter");
  this.time = document.getElementById("satTime");
  this.colorMap = document.getElementById("satColormap");
  this.reproject = document.getElementById("satReproject");
  this.format = document.getElementById("satFormat");
  this.count = document.getElementById("satCount");
  this.timeout = document.getElementById("satTimeout");
  this.jython = document.getElementById("satJython");
  
  this.request = document.getElementById("requestViewBox");
  
  this.hasInit = false;
}

SatelliteInterface.prototype.init = function(){
  if(!this.hasInit){
    this.getData("location");
    this.hasInit = true;
  }
}

SatelliteInterface.prototype.getData = function(type){
  this.disableControls();
  var self = this;
  switch(type){
    case "location":
      var queryArray = {"plugin" : "satellite"};
      RequestTestDriver.requestCatalog("sectorID",queryArray,60000,function(output){self.processData(type,output);});
      break;
    case "parameter":
      var location = this.location.options[this.location.selectedIndex].value;
      var queryArray = {"plugin" : "satellite", "sectorID" : location};
      RequestTestDriver.requestCatalog("physicalElement",queryArray,60000,function(output){self.processData(type,output);});
      break;
    case "time":
      var location = this.location.options[this.location.selectedIndex].value;
      var parameter = this.parameter.options[this.parameter.selectedIndex].value;
      var queryArray = {"plugin" : "satellite", "sectorID" : location , "physicalElement" : parameter};
      RequestTestDriver.requestCatalog("dataTime",queryArray,60000,function(output){self.processData(type,output);});
      break;
    case "colormap":
      RequestTestDriver.requestColormaps(60000,function(output){self.processData(type,output);});
      break;
  }
}

SatelliteInterface.prototype.processData = function(type,output){
  if(output == null || output.length == 0){
    alert("The satellite index appears to be empty.  Please ingest data and try again.");
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
      this.getData("parameter");
    }
    else{
      this.enableControls();
    }
    break;
  case "parameter":
    this.parameter.options.length = 0;
    for(var i = 0; i < output.length; i++){
      this.parameter.options[i] = new Option(output[i],output[i]);
    }
    if(this.parameter.options.length > 0){
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
      this.getData("colormap");
    }
    else{
      this.enableControls();
    }
    break;
  case "colormap":
    this.colorMap.options.length = 0;
    for(var i = 0; i < output.length; i++){
      this.colorMap.options[i] = new Option(output[i],output[i]);
    }
    output = null;
    if(this.colorMap.options.length > 0){
      this.createScript();
    }
    else{
      this.enableControls();
    }
    break;
  }
  output = null;
}

SatelliteInterface.prototype.createScript = function(){
  if(this.location.options.length == 0){
    return false;
  }
  this.disableControls();
  var location = this.location.options[this.location.selectedIndex].value;
  var parameter = this.parameter.options[this.parameter.selectedIndex].value;
  var time = this.time.options[this.time.selectedIndex].value;
  var jython = this.jython.value;
  var format = this.format.options[this.format.selectedIndex].value;
  var colormap = this.colorMap.options[this.colorMap.selectedIndex].value;
  var reproject = this.reproject.checked ? 1 : 0;
  var count = this.count.options[this.count.selectedIndex].value;
  var engine = document.getElementById("scriptInterface").checked ? "imagejs" : "image";
  var self = this;
  var queryArray = {"pluginName":"satellite","sectorID":location,"physicalElement":parameter,"dataTime":time,"colormap":colormap,"reproject":reproject,"format":format};
  ScriptTestDriver.createScript(queryArray,count,engine,function(output){self.processScript(output);});
}

SatelliteInterface.prototype.getTimeout = function(){
  var timeout = this.timeout.options[this.timeout.selectedIndex].value;
  return timeout;
}

SatelliteInterface.prototype.processScript = function(output){
  this.request.value = output;
  output = null;
  this.enableControls();
}

SatelliteInterface.prototype.disableControls = function(){
  for(var i = 0; i < this.satelliteForm.length; i++){
    this.satelliteForm[i].disabled = true;
  }
}

SatelliteInterface.prototype.enableControls = function(){
  for(var i = 0; i < this.satelliteForm.length; i++){
    this.satelliteForm[i].disabled = false;
  }
}
