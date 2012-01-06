// filename: RadarRespTable.java
// author  : DBGEN
// created : Tue May 31 17:52:26 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              radarresp table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class RadarRespTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  RadarRespTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public RadarRespTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("radarresp");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of RadarRespRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        RadarRespRecord record = null;

        // create a List to hold RadarResp Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM radarresp " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RadarRespRecord
            // and store its address in oneRecord
            record = new RadarRespRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RadarRespRecord object

            record.setRadid(getString(rs, 1));
            record.setSite_id(getString(rs, 2));
            
            // add this RadarRespRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the RadarRespRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of RadarRespRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        RadarRespRecord record = null;

        // create a List to hold RadarResp Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM radarresp " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RadarRespRecord
            // and store its address in oneRecord
            record = new RadarRespRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RadarRespRecord object

            record.setRadid(getString(rs, 1));
            record.setSite_id(getString(rs, 2));
            
            // add this RadarRespRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the RadarRespRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a RadarRespRecord object and..
//-----------------------------------------------------------------
    public int insert(RadarRespRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO radarresp VALUES (?, ?        )");

        setString(insertStatement, 1, record.getRadid());
        setString(insertStatement, 2, record.getSite_id());
        
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
        String deleteStatement = "DELETE FROM radarresp " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RadarRespRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RadarRespRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE radarresp SET radid = ?, site_id = ?        " + where );

        setString(updateStatement, 1, record.getRadid());
        setString(updateStatement, 2, record.getSite_id());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(RadarRespRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM radarresp " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RadarRespRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RadarRespRecord oldRecord, RadarRespRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE radarresp SET radid = ?, site_id = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getRadid());
        setString(updateStatement, 2, newRecord.getSite_id());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a RadarRespRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(RadarRespRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            RadarRespRecord oldRecord = (RadarRespRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of RadarRespTable class
