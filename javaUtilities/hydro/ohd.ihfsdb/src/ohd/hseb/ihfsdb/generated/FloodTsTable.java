// filename: FloodTsTable.java
// author  : DBGEN
// created : Tue May 31 17:52:22 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              floodts table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class FloodTsTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  FloodTsTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public FloodTsTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("floodts");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of FloodTsRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        FloodTsRecord record = null;

        // create a List to hold FloodTs Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM floodts " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a FloodTsRecord
            // and store its address in oneRecord
            record = new FloodTsRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a FloodTsRecord object

            record.setLid(getString(rs, 1));
            record.setObstime(getTimeStamp(rs, 2));
            record.setFlood_event_id(getInt(rs, 3));
            record.setValue(getDouble(rs, 4));
            
            // add this FloodTsRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the FloodTsRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of FloodTsRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        FloodTsRecord record = null;

        // create a List to hold FloodTs Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM floodts " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a FloodTsRecord
            // and store its address in oneRecord
            record = new FloodTsRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a FloodTsRecord object

            record.setLid(getString(rs, 1));
            record.setObstime(getTimeStamp(rs, 2));
            record.setFlood_event_id(getInt(rs, 3));
            record.setValue(getDouble(rs, 4));
            
            // add this FloodTsRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the FloodTsRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a FloodTsRecord object and..
//-----------------------------------------------------------------
    public int insert(FloodTsRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO floodts VALUES (?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setTimeStamp(insertStatement, 2, record.getObstime());
        setInt(insertStatement, 3, record.getFlood_event_id());
        setDouble(insertStatement, 4, record.getValue());
        
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
        String deleteStatement = "DELETE FROM floodts " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a FloodTsRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(FloodTsRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE floodts SET lid = ?, obstime = ?, flood_event_id = ?, value = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setTimeStamp(updateStatement, 2, record.getObstime());
        setInt(updateStatement, 3, record.getFlood_event_id());
        setDouble(updateStatement, 4, record.getValue());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(FloodTsRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM floodts " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a FloodTsRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(FloodTsRecord oldRecord, FloodTsRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE floodts SET lid = ?, obstime = ?, flood_event_id = ?, value = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setTimeStamp(updateStatement, 2, newRecord.getObstime());
        setInt(updateStatement, 3, newRecord.getFlood_event_id());
        setDouble(updateStatement, 4, newRecord.getValue());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a FloodTsRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(FloodTsRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            FloodTsRecord oldRecord = (FloodTsRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of FloodTsTable class
