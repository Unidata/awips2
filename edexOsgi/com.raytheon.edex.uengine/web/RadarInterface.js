function RadarInterface(defaultMode){
  this.radarForm = document.getElementById("radarForm");
  this.location = document.getElementById("radLocation");
  this.type = document.getElementById("radType");
  this.elevation = document.getElementById("radElevation");
  this.time = document.getElementById("radTime");
  this.colorMap = document.getElementById("radColormap");
  this.reproject = document.getElementById("radReproject");
  this.format = document.getElementById("radFormat");
  this.count = document.getElementById("radCount");
  this.timeout = document.getElementById("radTimeout");
  
  this.request = document.getElementById("requestViewBox");
  
  this.radarType = {19:"Level 3 16 Level Base Reflectivity",27:"Level 3 16 Level Base Velocity",56:"Level 3 16 Level Storm Relative Mean Radial Velocity",94:"8-bit Reflectivity",99:"8-bit Velocity",300:"Level 2 Base Reflectivity",301:"Level 2 Base Velocity"};
  
  this.hasInit = false;
}

RadarInterface.prototype.init = function(){
  if(!this.hasInit){
    this.getData("type");
    this.hasInit = true;
  }
}

RadarInterface.prototype.getData = function(type){
  this.disableControls();
  var self = this;
  switch(type){
    case "type":
      var queryArray = {"plugin" : "radar"};
      RequestTestDriver.requestCatalog("productCode",queryArray,60000,function(output){self.processData(type,output);});
      break;
    case "elevation":
      var radType = this.type.options[this.type.selectedIndex].value;
      var queryArray = {"plugin" : "radar", "productCode" : radType};
      RequestTestDriver.requestCatalog("primaryElevationAngle",queryArray,60000,function(output){self.processData(type,output);});
      break;
    case "location":
      var radType = this.type.options[this.type.selectedIndex].value;
      var elevation = this.elevation.options[this.elevation.selectedIndex].value;
      var queryArray = {"plugin" : "radar", "productCode" : radType, "primaryElevationAngle" : elevation};
      RequestTestDriver.requestCatalog("icao",queryArray,60000,function(output){self.processData(type,output);});
      break;
    case "time":
      var radType = this.type.options[this.type.selectedIndex].value;
      var elevation = this.elevation.options[this.elevation.selectedIndex].value;
      var location = this.location.options[this.location.selectedIndex].value;
      var queryArray = {"plugin" : "radar", "icao" : location , "primaryElevationAngle" : elevation, "productCode" : radType};
      RequestTestDriver.requestCatalog("dataTime",queryArray,60000,function(output){self.processData(type,output);});
      break;
    case "colormap":
      RequestTestDriver.requestColormaps(60000,function(output){self.processData(type,output);});
      break;
  }
}

RadarInterface.prototype.processData = function(type,output){
  if(output == null || output.length == 0){
    alert("The radar index appears to be empty.  Please ingest data and try again.");
    this.hasInit = false;
    this.enableControls();
    return false;
  }
  output.sort();
  switch(type){
  case "type":
    this.type.options.length = 0;
    for(var i = 0; i < output.length; i++){
      var s = this.radarType[output[i]];
      if (s == null) {
         s = "Product code " + output[i];
      }
      this.type.options[i] = new Option(s,output[i]);
    }
    if(this.type.options.length > 0){
      this.getData("elevation");
    }
    else{
      this.enableControls();
    }
    break;
  case "elevation":
    this.elevation.options.length = 0;
    for(var i = 0; i < output.length; i++){
      this.elevation.options[i] = new Option(output[i],output[i]);
    }
    if(this.elevation.options.length > 0){
      this.getData("location");
    }
    else{
      this.enableControls();
    }
    break;
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

RadarInterface.prototype.createScript = function(){
  if(this.type.options.length == 0){
    return false;
  }
  this.disableControls();
  var location = this.location.options[this.location.selectedIndex].value;
  var elevation = this.elevation.options[this.elevation.selectedIndex].value;
  var type = this.type.options[this.type.selectedIndex].value;
  var time = this.time.options[this.time.selectedIndex].value;
  var colormap = this.colorMap.options[this.colorMap.selectedIndex].value;
  var reproject = this.reproject.checked ? 1 : 0;
  var format = this.format.options[this.format.selectedIndex].value;
  var count = this.count.options[this.count.selectedIndex].value;
  var engine = document.getElementById("scriptInterface").checked ? "imagejs" : "image";
  var self = this;
  var queryArray = {"pluginName":"radar","icao":location,"productCode":type,"dataTime":time,"primaryElevationAngle":elevation,"colormap":colormap,"reproject":reproject,"format":format};
  ScriptTestDriver.createScript(queryArray,count,engine,function(output){self.processScript(output);});
}

RadarInterface.prototype.getTimeout = function(){
  var timeout = this.timeout.options[this.timeout.selectedIndex].value;
  return timeout;
}

RadarInterface.prototype.processScript = function(output){
  this.request.value = output;
  output = null;
  this.enableControls();
}

RadarInterface.prototype.disableControls = function(){
  for(var i = 0; i < this.radarForm.length; i++){
    this.radarForm[i].disabled = true;
  }
}

RadarInterface.prototype.enableControls = function(){
  for(var i = 0; i < this.radarForm.length; i++){
    this.radarForm[i].disabled = false;
  }
}
