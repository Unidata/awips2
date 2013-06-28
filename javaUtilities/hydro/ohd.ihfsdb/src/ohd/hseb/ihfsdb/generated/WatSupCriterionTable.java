// filename: WatSupCriterionTable.java
// author  : DBGEN
// created : Tue May 31 17:52:30 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              watsupcriterion table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class WatSupCriterionTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  WatSupCriterionTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public WatSupCriterionTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("watsupcriterion");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of WatSupCriterionRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        WatSupCriterionRecord record = null;

        // create a List to hold WatSupCriterion Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM watsupcriterion " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a WatSupCriterionRecord
            // and store its address in oneRecord
            record = new WatSupCriterionRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a WatSupCriterionRecord object

            record.setWatsup_criterion(getString(rs, 1));
            
            // add this WatSupCriterionRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the WatSupCriterionRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of WatSupCriterionRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        WatSupCriterionRecord record = null;

        // create a List to hold WatSupCriterion Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM watsupcriterion " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a WatSupCriterionRecord
            // and store its address in oneRecord
            record = new WatSupCriterionRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a WatSupCriterionRecord object

            record.setWatsup_criterion(getString(rs, 1));
            
            // add this WatSupCriterionRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the WatSupCriterionRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a WatSupCriterionRecord object and..
//-----------------------------------------------------------------
    public int insert(WatSupCriterionRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO watsupcriterion VALUES (?        )");

        setString(insertStatement, 1, record.getWatsup_criterion());
        
        // get the number of records processed by the insert
        returnCode = insertStatement.executeUpdate();

        return returnCode;

    } // end of insert method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(String where) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM watsupcriterion " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a WatSupCriterionRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(WatSupCriterionRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE watsupcriterion SET watsup_criterion = ?        " + where );

        setString(updateStatement, 1, record.getWatsup_criterion());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(WatSupCriterionRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM watsupcriterion " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a WatSupCriterionRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(WatSupCriterionRecord oldRecord, WatSupCriterionRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE watsupcriterion SET watsup_criterion = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getWatsup_criterion());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a WatSupCriterionRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(WatSupCriterionRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            WatSupCriterionRecord oldRecord = (WatSupCriterionRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of WatSupCriterionTable class
