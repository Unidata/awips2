function TestDriverControls(defaultMode){
  this.decodeDir = "";
  this.dataDir = "";
  this.tab = null;
  this.queryForm = null;
  this.displayViewButton = false;
  this.busy = false;
  this.tabHolder = document.getElementById("leftTabs");
  this.fileSelectBox = document.getElementById("files");
  this.fileViewButton = document.getElementById("fileViewButton");
  this.fileViewBox = document.getElementById("fileViewBox");
  this.testDriverName = document.getElementById("testDriverName");
  this.actionScriptBox = document.getElementById("requestViewBox");
  this.actionResponseBox = document.getElementById("responseViewBox");
  this.returnData = document.getElementById("returnedData");
  
  this.satInterface = document.getElementById("satelliteInterface");
  this.radInterface = document.getElementById("radarInterface");
  this.grdInterface = document.getElementById("gridInterface");
  this.ascInterface = document.getElementById("asciiInterface");
  this.satInterfaceInit = false;
  this.radInterfaceInit = false;
  this.grdInterfaceInit = false;
  this.ascInterfaceInit = false;
  
  this.currentInterface = null;
  
  this.setMode(defaultMode);    
}

TestDriverControls.prototype.updateCatalog = function(type){
  this.currentInterface.getData(type);
}

TestDriverControls.prototype.createScript = function(){
  this.currentInterface.createScript();
}

TestDriverControls.prototype.getFileList = function(){
  this.currentInterface.disableControls();
  var self = this;
  TestDataDriver.listTestDataFiles(this.dataDir,function(files){ self.processFileList(files); });
}

TestDriverControls.prototype.processFileList = function(files){
  this.fileSelectBox.options.length = 0;
  for(var i = 0; i < files.length; i++){
    this.fileSelectBox.options[this.fileSelectBox.options.length] = new Option(files[i],files[i]);
  }
  files = null;
  this.currentInterface.enableControls();
}

TestDriverControls.prototype.viewFile = function(){
  var fileName = this.getFileName();
  if(fileName != "NONE"){
    var self = this;
    TestDataDriver.viewTestDataFiles(this.dataDir, fileName, function(response){self.processFileView(response);});
  }
}

TestDriverControls.prototype.processFileView = function(fileData){
  this.fileViewBox.value = fileData;
  fileData = null;
}

TestDriverControls.prototype.copyFile = function(){
  this.currentInterface.disableControls();
  this.busy = true;
  var fileName = this.getFileName();
  if(fileName != "NONE"){
    var self = this;
    TestDataDriver.ingestTestDataFiles(this.dataDir, this.decodeDir, fileName, function(response){self.copyFileStatus(response);});
  }
}

TestDriverControls.prototype.getFileName = function(){
  if(this.fileSelectBox.selectedIndex < 0){
    alert("Please select a file from the box above.");
    return "NONE";
  }
  var fileName = this.fileSelectBox.options[this.fileSelectBox.selectedIndex].value;
  if(fileName == "No Files Available"){
    alert("No files are available.");
    return "NONE";
  }
  return fileName;
}

TestDriverControls.prototype.copyFileStatus = function(copySuccess){
  if(copySuccess){
    alert("The file was copied to the decode directory.  Please wait for a few seconds to allow time to ingest.");
  }
  else{
    alert("The copy failed.  Please check the data and decode directories for any problems.");
  }
  copySuccess = null;
  this.currentInterface.enableControls();
  this.busy = false;
}

TestDriverControls.prototype.setMode = function(mode){
  if(mode == this.mode || this.busy){
    return false;
  }
  this.currentInterface = null;
  var displayViewButton = false;
  var initInterface = false;
  this.mode = mode;
  this.queryForm = document.getElementById(this.mode + "Form");
  for(var i = 0; i < this.tabHolder.childNodes.length; i++){
    this.tabHolder.childNodes[i].className = "leftTabEntry";
  }
  this.satInterface.style.display = "none";
  this.radInterface.style.display = "none";
  this.grdInterface.style.display = "none";
  this.ascInterface.style.display = "none";
  document.getElementById(this.mode + "Tab").className = "leftTabSelected";
  switch(this.mode){
    case "satellite":
      this.decodeDir = "sat/";
      this.dataDir = "tst/satellite/";
      displayViewButton = false;
      this.satInterface.style.display = "block";
      this.currentInterface = new SatelliteInterface();
      if(!this.satInterfaceInit){
        this.satInterfaceInit = true;
        initInterface = true;
      }
    break;
    case "radar":
      this.decodeDir = "radar/";
      this.dataDir = "tst/radar/";
      displayViewButton = false;
      this.radInterface.style.display = "block";
      this.currentInterface = new RadarInterface();
      if(!this.radInterfaceInit){
        this.radInterfaceInit = true;
        initInterface = true;
      }
    break;
    case "grid":
      this.decodeDir = "grib/";
      this.dataDir = "tst/grib/";
      displayViewButton = false;
      this.grdInterface.style.display = "block";
      this.currentInterface = new GridInterface();
      if(!this.grdInterfaceInit){
        this.grdInterfaceInit = true;
        initInterface = true;
      }
    break;
    case "ascii":
      this.decodeDir = "ascii/";
      this.dataDir = "tst/ascii/";
      displayViewButton = true;
      this.ascInterface.style.display = "block"
      this.currentInterface = new AsciiInterface();
      if(!this.ascInterfaceInit){
        this.ascInterfaceInit = true;
        initInterface = true;
      }
    break;
  }
  if(displayViewButton){
    this.fileViewBox.value = "";
    this.fileViewButton.style.visibility = "visible";
    this.fileViewBox.style.visibility = "visible";
  }
  else{
    this.fileViewButton.style.visibility = "hidden";
    this.fileViewBox.style.visibility = "hidden";
  }
  if(initInterface){
    this.currentInterface.init();
  }
  else{
    this.currentInterface.createScript();
  }
  this.testDriverName.childNodes[0].nodeValue = this.mode.toUpperCase() + " Test Driver";
  this.getFileList();
  while(this.returnData.hasChildNodes()){ 
    this.returnData.removeChild(this.returnData.firstChild);
  }
}

TestDriverControls.prototype.requestData = function(){
  this.busy = true;
  var scriptJs = document.getElementById("scriptInterface");
  this.currentInterface.disableControls();
  var timeout = this.currentInterface.getTimeout();
  var script = this.actionScriptBox.value;
  if(script.length == 0){
    alert("No script was available to send.  Please check 'Request' in the 'Request/Response Message' area.");
    return false;
  }
  this.actionResponseBox.value = "";
  var self = this;
  RequestTestDriver.requestProduct(script,timeout,scriptJs.checked,function(response){self.processDataRequest(response);});
}

TestDriverControls.prototype.processDataRequest = function(response){
  while(this.returnData.hasChildNodes()){ 
    this.returnData.removeChild(this.returnData.firstChild);
  }
  this.actionResponseBox.value = response["Response"];
  if(response["TYPE"] == "URI"){
    this.processURIResponse(response);
  }
  else if(response["TYPE"] == "XML"){
    this.processAsciiResponse(response);
  }
  else if(response["TYPE"] == "ERROR"){
    alert("A 'responseError' message was returned.  Please look in 'Request/Response' to see the raw response.");
  }
  else{
    alert("The response type was " + response["TYPE"] + " which is not supported.  Please look in 'Request/Response' to see the raw response.");
  }
  this.busy = false;
  this.currentInterface.enableControls();
}

TestDriverControls.prototype.purgeData = function(){
  this.busy = true;
  this.currentInterface.disableControls();
  var self = this;
  if (!window.confirm("you are about to purge the database on " + window.location.hostname +
                      "\nAre you sure you want to perform this pruge?" +
                      "\n(Cancel to exit pruge, OK to perform purge)\n")) {
      exit();
  }
  RequestTestDriver.purgeDataStore(function(response){self.processPurgeRequest(response);});
}

TestDriverControls.prototype.processPurgeRequest = function(response){
  if(response){
    alert("Request for data purge sent successfully.  Please allow thirty seconds for the purge to complete.");
  }
  else{
    alert("Request for data purge failed.  Please check the Tomcat log for errors.");
  }
  this.busy = false;
  this.currentInterface.enableControls();
}

TestDriverControls.prototype.processURIResponse = function(response){
  var uriCount = response["URICOUNT"];
  for(var i = 0; i < uriCount; i++){
    var spacer = null;
    var productLocation = "/uEngineProducts/" + response["URI" + i];
    var productAnchor = document.createElement("a");
    productAnchor.setAttribute("href",productLocation);
    productAnchor.setAttribute("target","_new");
    if(response["TYPE"+i] == "tiff" || response["TYPE"+i] == "kml"){
      var productName = document.createTextNode(response["URI" + i]);
      productAnchor.appendChild(productName);
      spacer = document.createElement("br");
      
    } else{
      var productImage = document.createElement("img");
      productImage.setAttribute("src",productLocation);
      productImage.setAttribute("width","200px");
      productImage.setAttribute("border","0");
      productImage.setAttribute("title",response["DATE"+i]);
      productAnchor.appendChild(productImage);
      spacer = document.createTextNode(" ");
    }
    this.returnData.appendChild(productAnchor);
    this.returnData.appendChild(spacer);
  }
}

TestDriverControls.prototype.processAsciiResponse = function(response){
  for(var i = 0; i < response["XMLCOUNT"]; i++){
    var doc = null;
    var spacer = document.createElement("br");
    if (window.ActiveXObject){
      doc=new ActiveXObject("Microsoft.XMLDOM");
      doc.async="false";
      doc.loadXML(response["MESSAGE" + i]);
    }
    else{
      var parser=new DOMParser();
      var doc=parser.parseFromString(response["MESSAGE" + i],"text/xml");
    }
    var wxDoc = doc.documentElement;
    var fullObs = this.traverse(wxDoc,"");
    this.returnData.appendChild(fullObs);
    this.returnData.appendChild(spacer);
  }
}

TestDriverControls.prototype.traverse = function(tree,title) {
  if(tree.hasChildNodes()) {
    var title = tree.tagName;
    var baseDoc = document.createElement("ul");
    var docEntry = document.createElement("li");
    var titleText = document.createTextNode(tree.tagName + " : ");
    docEntry.appendChild(titleText);
    var nodes=tree.childNodes.length;
    for(var i=0; i<tree.childNodes.length; i++){
      var nextLevel = this.traverse(tree.childNodes[i],title);
      docEntry.appendChild(nextLevel);
    }
    baseDoc.appendChild(docEntry);
    return baseDoc;
  }
  else if(tree.nodeValue != null){
    if(title == "raw_text"){
     var rawText = document.createElement("pre");
     var rawTextValue = document.createTextNode(tree.nodeValue);
     rawText.appendChild(rawTextValue);
     return rawText;
    }
    else{
      var rawText = document.createTextNode(tree.nodeValue);
      return rawText;
    }
  }
  else{
    var nextBaseDoc = document.createElement("ul");
    var nextDocEntry = document.createElement("li");
    var empty = document.createTextNode(tree.tagName + " : EMPTY");
    nextDocEntry.appendChild(empty);
    nextBaseDoc.appendChild(nextDocEntry);
    return nextBaseDoc;
  }
}

TestDriverControls.prototype.getCurrentTimestamp = function(){
  var myDate = new Date();
  var timestamp = new StringBuffer();
  var month = myDate.getUTCMonth() + 1;
  month = month < 10 ? "0" + month : month;
  var day = myDate.getUTCDate() < 10 ? "0" + myDate.getUTCDate() : myDate.getUTCDate();
  var hour = myDate.getUTCHours() < 10 ? "0" + myDate.getUTCHours() : myDate.getUTCHours();
  var minutes = myDate.getUTCMinutes() < 10 ? "0" + myDate.getUTCMinutes() : myDate.getUTCMinutes();
  var seconds = myDate.getUTCSeconds() < 10 ? "0" + myDate.getUTCSeconds() : myDate.getUTCSeconds();
  timestamp.append(myDate.getUTCFullYear());
  timestamp.append(month);
  timestamp.append(day);
  timestamp.append(hour);
  timestamp.append(minutes);
  timestamp.append(seconds);
  return timestamp.toString();
  
}