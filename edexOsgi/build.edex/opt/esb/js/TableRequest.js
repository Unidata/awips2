/**
 * Obtains an entry from a table.
 *
 * Usage:
 *   include("TableRequest.js");
 *   var query = new TableQuery("tableName");
 *   query.addParameter ("site", "Valley"); 
 *   query.addParameter ("type", "flood"); 
 *   var results = query.execute();
 */

/**
 * Class constructor.
 *
 * @param tableName (String) identifies the table 
 */
function TableRequest(database, table){
  this.database = database;
  this.tableName = table;
  this.queryResults = null;
  this.query = new TableQuery(this.database,this.tableName);
}

function _addParameter(name,value,operand)
{
  if(arguments.length==2)
  {
    this.query.addParameter(name,value);
  } 
  else
  {
    this.query.addParameter(name,value,operand);
  }
}

function _addList(name, value){
  this.query.addList(name, value);
}

function _setCount(count){
  this.query.setCount(count);
}

function _setSortValue(sortValue){
  this.query.setSortBy(sortValue);
}

function _setTableName(name){
  this.tableName = name;
}

function _setDatabase(name){
  this.database = name;
}

/**
 * Main action method. Queries the database and returns the XML
 * representation of a persistable data object. 
 *
 * @return (String) XML representation of a table on success
 *         (String) XML null response string on failure
 */
function _execute()
{
  this.queryResults = this.query.execute();
  if(this.queryResults == null || this.queryResults.size() == 0)
  {
    var response = new MakeResponseNull("Query returned 0 results.",
                                           this.query);
    return response.execute();
  }
  else
  {
    return this.makeXmlResponse();  
  }
}

function _makeXmlResponse()
{
  var response = new Array();
  for(i=0; i < this.queryResults.size(); i++)
  {	
    var makeResponse = new MakeResponseXml(this.queryResults.get(i));
    response[i] = makeResponse.execute();
  }
  return response;
}

TableRequest.prototype.execute = _execute;
TableRequest.prototype.makeXmlResponse = _makeXmlResponse;
TableRequest.prototype.addParameter = _addParameter;
TableRequest.prototype.addList = _addList;
TableRequest.prototype.setCount = _setCount;
TableRequest.prototype.setSortValue = _setSortValue;
TableRequest.prototype.setTableName = _setTableName;
TableRequest.prototype.setDatabase = _setDatabase;
