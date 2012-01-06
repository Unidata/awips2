function CatalogQuery(plugin){
  this.cat = new MetadataCatalogQuery(plugin);
}

function _setDistinctField(name){
	this.cat.setDistinctField(name);
}

function _addReturnedField(name){
  this.cat.addReturnedField(name);
}

function _addConstraint(name, value,operand){
	if(arguments.length==2){
  		this.cat.addParameter(name, value);
  	}else{
  		this.cat.addParameter(name,value,operand);
  	}
}

function _execute(){
  return this.cat.execute();
}

CatalogQuery.prototype.addReturnedField = _addReturnedField;
CatalogQuery.prototype.addConstraint = _addConstraint;
CatalogQuery.prototype.execute = _execute;
CatalogQuery.prototype.setDistinctField = _setDistinctField;
