/**
 * Updates an entry in a table.
 *
 * Usage:
 *   include("TableResponse.js");
 *   var query = new TableUpdate("tableName", "rowName");
 *   var results = query.execute();
 */

/**
 * Class constructor.
 *
 * @param tableName (String) identifies the table 
 * @param rowName (String) identifies the row 
 */
function TableResponse(database, table, row){
  this.database = database;
  this.tableName = table;
  this.rowName = row
  this.updateResults = null;
  this.update = new TableUpdate(this.database, this.tableName, this.rowName);
}

/**
 * Main action method. Updates the database and returns the XML
 * representation of an acknowledgement. 
 *
 * @return (String) XML acknowledgement on success
 *         (String) XML null response string on failure
 */
function _execute()
{
  this.updateResults = this.update.execute();
  return this.updateResults;
}

function _makeXmlResponse()
{
  var response = new Array();
  for(i=0; i < this.updateResults.size(); i++)
  {	
    var makeResponse = new MakeResponseXml(this.updateResults.get(i));
    response[i] = makeResponse.execute();
  }
  return response;
}

TableResponse.prototype.execute = _execute;
TableResponse.prototype.makeXmlResponse = _makeXmlResponse;
