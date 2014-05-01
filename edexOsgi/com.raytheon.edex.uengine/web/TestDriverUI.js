function TestDriverUI(){
  this.expandImg = new Image();
  this.expandImg.src = "expand.gif";
  this.hideImg = new Image();
  this.hideImg.src = "hide.gif";
}

TestDriverUI.prototype.toggleSection = function(divName,divControlImg){
  var divToToggle = document.getElementById(divName);
  var divImgToggle = document.getElementById(divControlImg);
  divImgToggle.src = divToToggle.style.display == "none" ? this.hideImg.src : this.expandImg.src;
  divToToggle.style.display = divToToggle.style.display == "none" ? "block" : "none";
}