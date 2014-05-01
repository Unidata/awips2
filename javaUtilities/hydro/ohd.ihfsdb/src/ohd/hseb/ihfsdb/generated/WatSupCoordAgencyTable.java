// filename: WatSupCoordAgencyTable.java
// author  : DBGEN
// created : Tue May 31 17:52:30 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              watsupcoordagency table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class WatSupCoordAgencyTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  WatSupCoordAgencyTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public WatSupCoordAgencyTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("watsupcoordagency");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of WatSupCoordAgencyRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        WatSupCoordAgencyRecord record = null;

        // create a List to hold WatSupCoordAgency Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM watsupcoordagency " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a WatSupCoordAgencyRecord
            // and store its address in oneRecord
            record = new WatSupCoordAgencyRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a WatSupCoordAgencyRecord object

            record.setWatsup_coord_agency(getString(rs, 1));
            
            // add this WatSupCoordAgencyRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the WatSupCoordAgencyRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of WatSupCoordAgencyRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        WatSupCoordAgencyRecord record = null;

        // create a List to hold WatSupCoordAgency Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM watsupcoordagency " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a WatSupCoordAgencyRecord
            // and store its address in oneRecord
            record = new WatSupCoordAgencyRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a WatSupCoordAgencyRecord object

            record.setWatsup_coord_agency(getString(rs, 1));
            
            // add this WatSupCoordAgencyRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the WatSupCoordAgencyRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a WatSupCoordAgencyRecord object and..
//-----------------------------------------------------------------
    public int insert(WatSupCoordAgencyRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO watsupcoordagency VALUES (?        )");

        setString(insertStatement, 1, record.getWatsup_coord_agency());
        
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
        String deleteStatement = "DELETE FROM watsupcoordagency " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a WatSupCoordAgencyRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(WatSupCoordAgencyRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE watsupcoordagency SET watsup_coord_agency = ?        " + where );

        setString(updateStatement, 1, record.getWatsup_coord_agency());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(WatSupCoordAgencyRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM watsupcoordagency " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a WatSupCoordAgencyRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(WatSupCoordAgencyRecord oldRecord, WatSupCoordAgencyRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE watsupcoordagency SET watsup_coord_agency = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getWatsup_coord_agency());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a WatSupCoordAgencyRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(WatSupCoordAgencyRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            WatSupCoordAgencyRecord oldRecord = (WatSupCoordAgencyRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of WatSupCoordAgencyTable class
