function GridInterface(defaultMode){
  this.gridForm = document.getElementById("gridForm");
  
  this.model = document.getElementById("grdModel");
  this.basetime = document.getElementById("grdBasetime");
  this.parameter = document.getElementById("grdParameter");
  this.level = document.getElementById("grdLevel");
  this.levelone = document.getElementById("grdlevelone");
  this.leveltwo = document.getElementById("grdleveltwo");
  //this.forecasthour = document.getElementById("grdForecasthour");
  
  this.colormap = document.getElementById("grdColormap");
  this.reproject = document.getElementById("grdReproject");
  this.format = document.getElementById("grdFormat");
  this.jython = document.getElementById("grdJython");
  this.count = document.getElementById("grdCount");
  this.timeout = document.getElementById("grdTimeout");
  
  this.request = document.getElementById("requestViewBox");
  
  this.hasInit = false;
}

GridInterface.prototype.init = function(){
  if(!this.hasInit){
    this.getData("model");
    this.hasInit = true;
  }
}

GridInterface.prototype.getData = function(type){
  this.disableControls();
  var self = this;
  switch(type){
    case "model":
      var queryArray = {"plugin":"grid"};
      RequestTestDriver.requestCatalog("info.datasetId",queryArray,60000,function(output){self.processData(type,output);});
      break;
    case "basetime":
      var model = this.model.options[this.model.selectedIndex].value;
      var queryArray = {"plugin":"grid","info.datasetId":model};
      RequestTestDriver.requestCatalog("dataTime",queryArray,60000,function(output){self.processData(type,output);});
      break;
    case "parameter":
      var model = this.model.options[this.model.selectedIndex].value;
      var basetime = this.basetime.options[this.basetime.selectedIndex].value;
      var queryArray = {"plugin":"grid","info.datasetId":model,"dataTime":basetime};
      RequestTestDriver.requestCatalog("info.parameter.name",queryArray,60000,function(output){self.processData(type,output);});
      break;
    case "level":
      var model = this.model.options[this.model.selectedIndex].value;
      var basetime = this.basetime.options[this.basetime.selectedIndex].value;
      var parameter = this.parameter.options[this.parameter.selectedIndex].value;
      var queryArray = {"plugin":"grid","info.datasetId":model,"dataTime":basetime,"info.parameter.name":parameter};
      RequestTestDriver.requestCatalog("info.level.masterLevel.name",queryArray,60000,function(output){self.processData(type,output);});
      break;
    case "levelone":
      var model = this.model.options[this.model.selectedIndex].value;
      var basetime = this.basetime.options[this.basetime.selectedIndex].value;
      var parameter = this.parameter.options[this.parameter.selectedIndex].value;
      var level = this.level.options[this.level.selectedIndex].value;
      var queryArray = {"plugin":"grid","info.datasetId":model,"dataTime":basetime,"info.parameter.name":parameter,"info.level.masterLevel.name":level};
      RequestTestDriver.requestCatalog("info.level.levelonevalue",queryArray,60000,function(output){self.processData(type,output);});
      break;
    case "leveltwo":
      var model = this.model.options[this.model.selectedIndex].value;
      var basetime = this.basetime.options[this.basetime.selectedIndex].value;
      var parameter = this.parameter.options[this.parameter.selectedIndex].value;
      var level = this.level.options[this.level.selectedIndex].value;
      var levelone = this.levelone.options[this.levelone.selectedIndex].value;
      var queryArray = {"plugin":"grid","info.datasetId":model,"dataTime":basetime,"info.parameter.name":parameter,"info.level.masterLevel.name":level,
"info.level.levelonevalue":levelone};
      RequestTestDriver.requestCatalog("info.level.leveltwovalue",queryArray,60000,function(output){self.processData(type,output);});
      break;
    case "colormap":
      RequestTestDriver.requestColormaps(60000,function(output){self.processData(type,output);});
      break;
  }
}

GridInterface.prototype.processData = function(type,output){
  if(type != 'leveltwo' && (output == null || output.length == 0)){
    alert("The grid index appears to be empty.  Please ingest data and try again.");
    this.hasInit = false;
    this.enableControls();
    return false;
  }
  if (output != null && output.length > 0) {
     output.sort();
  }
  switch(type){
  case "model":
    this.model.options.length = 0;
    for(var i = 0; i < output.length; i++){
      this.model.options[i] = new Option(output[i],output[i]);
    }
    if(this.model.options.length > 0){
      this.getData("basetime");
    }
    else{
      this.enableControls();
    }
    break;
  case "basetime":
    this.basetime.options.length = 0;
    for(var i = 0; i < output.length; i++){
      this.basetime.options[i] = new Option(output[i],output[i]);
    }
    if(this.basetime.options.length > 0){
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
    output = null;
    if(this.parameter.options.length > 0){
      this.getData("level");
    }
    else{
      this.enableControls();
    }
    break;
  case "level":
    this.level.options.length = 0;
    for(var i = 0; i < output.length; i++){
      this.level.options[i] = new Option(output[i],output[i]);
    }
    output = null;
    if(this.level.options.length > 0){
      this.getData("levelone");
    }
    else{
      this.enableControls();
    }
    break;
  case "levelone":
    this.levelone.options.length = 0;
    for(var i = 0; i < output.length; i++){
      this.levelone.options[i] = new Option(output[i],output[i]);
    }
    output = null;
    if(this.levelone.options.length > 0){
      this.getData("leveltwo");
    }
    else{
      this.enableControls();
    }
    break;
  case "leveltwo":
    this.leveltwo.options.length = 0;
    if (output != null) {
       for(var i = 0; i < output.length; i++){
          this.leveltwo.options[i] = new Option(output[i],output[i]);
       }
    }
    output = null;
    this.getData("colormap");
    break;
  case "colormap":
    this.colormap.options.length = 0;
    for(var i = 0; i < output.length; i++){
      this.colormap.options[i] = new Option(output[i],output[i]);
    }
    output = null;
    if(this.colormap.options.length > 0){
      this.createScript();
    }
    else{
      this.enableControls();
    }
    break;
  }
  output = null;
}

GridInterface.prototype.createScript = function(){
  if(this.model.options.length == 0){
    return 0;
  }
  this.disableControls();
  var model = this.model.options[this.model.selectedIndex].value;
  var basetime = this.basetime.options[this.basetime.selectedIndex].value;
  var parameter = this.parameter.options[this.parameter.selectedIndex].value;
  var level = this.level.options[this.level.selectedIndex].value;
  var levelone = this.levelone.options[this.levelone.selectedIndex].value;
  var jython = this.jython.value;
  var reproject = this.reproject.checked ? 1 : 0;
  var colormap = this.colormap.options[this.colormap.selectedIndex].value;
  var format = this.format.options[this.format.selectedIndex].value;
  var count = this.count.options[this.count.selectedIndex].value;
  var self = this;
  var engine = document.getElementById("scriptInterface").checked ? "imagejs" : "image";
  if (this.leveltwo.selectedIndex >= 0) { 
     var leveltwo = this.leveltwo.options[this.leveltwo.selectedIndex].value;
     var queryArray = {"pluginName":"grid","info.datasetId":model,"dataTime":basetime,
      "info.parameter.name":parameter,"info.level.masterLevel.name":level,
      "info.level.levelonevalue":levelone, "info.level.leveltwovalue":leveltwo,
      "colormap":colormap,"reproject":reproject,"format":format};
  } else {
     var queryArray = {"pluginName":"grid","info.datasetId":model,"dataTime":basetime,
      "info.parameter.name":parameter,"info.level.masterLevel.name":level,
      "info.level.levelonevalue":levelone, "colormap":colormap,"reproject":reproject,"format":format};
  }
  ScriptTestDriver.createScript(queryArray,count,engine,function(output){self.processScript(output);});
}

GridInterface.prototype.getTimeout = function(){
  var timeout = this.timeout.options[this.timeout.selectedIndex].value;
  return timeout;
}

GridInterface.prototype.processScript = function(output){
  this.request.value = output;
  output = null;
  this.enableControls();
}

GridInterface.prototype.disableControls = function(){
  for(var i = 0; i < this.gridForm.length; i++){
    this.gridForm[i].disabled = true;
  }
}

GridInterface.prototype.enableControls = function(){
  for(var i = 0; i < this.gridForm.length; i++){
    this.gridForm[i].disabled = false;
  }
}
