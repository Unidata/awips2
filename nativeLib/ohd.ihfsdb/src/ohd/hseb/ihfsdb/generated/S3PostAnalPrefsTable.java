// filename: S3PostAnalPrefsTable.java
// author  : DBGEN
// created : Tue May 31 17:52:28 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              s3postanalprefs table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class S3PostAnalPrefsTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  S3PostAnalPrefsTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public S3PostAnalPrefsTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("s3postanalprefs");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of S3PostAnalPrefsRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        S3PostAnalPrefsRecord record = null;

        // create a List to hold S3PostAnalPrefs Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM s3postanalprefs " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a S3PostAnalPrefsRecord
            // and store its address in oneRecord
            record = new S3PostAnalPrefsRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a S3PostAnalPrefsRecord object

            record.setUserid(getString(rs, 1));
            record.setState_overlay(getString(rs, 2));
            record.setCity_overlay(getString(rs, 3));
            record.setRiver_overlay(getString(rs, 4));
            record.setBasin_overlay(getString(rs, 5));
            record.setRadar_overlay(getString(rs, 6));
            record.setNum_hours_wind(getShort(rs, 7));
            
            // add this S3PostAnalPrefsRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the S3PostAnalPrefsRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of S3PostAnalPrefsRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        S3PostAnalPrefsRecord record = null;

        // create a List to hold S3PostAnalPrefs Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM s3postanalprefs " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a S3PostAnalPrefsRecord
            // and store its address in oneRecord
            record = new S3PostAnalPrefsRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a S3PostAnalPrefsRecord object

            record.setUserid(getString(rs, 1));
            record.setState_overlay(getString(rs, 2));
            record.setCity_overlay(getString(rs, 3));
            record.setRiver_overlay(getString(rs, 4));
            record.setBasin_overlay(getString(rs, 5));
            record.setRadar_overlay(getString(rs, 6));
            record.setNum_hours_wind(getShort(rs, 7));
            
            // add this S3PostAnalPrefsRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the S3PostAnalPrefsRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a S3PostAnalPrefsRecord object and..
//-----------------------------------------------------------------
    public int insert(S3PostAnalPrefsRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO s3postanalprefs VALUES (?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getUserid());
        setString(insertStatement, 2, record.getState_overlay());
        setString(insertStatement, 3, record.getCity_overlay());
        setString(insertStatement, 4, record.getRiver_overlay());
        setString(insertStatement, 5, record.getBasin_overlay());
        setString(insertStatement, 6, record.getRadar_overlay());
        setShort(insertStatement, 7, record.getNum_hours_wind());
        
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
        String deleteStatement = "DELETE FROM s3postanalprefs " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a S3PostAnalPrefsRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(S3PostAnalPrefsRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE s3postanalprefs SET userid = ?, state_overlay = ?, city_overlay = ?, river_overlay = ?, basin_overlay = ?, radar_overlay = ?, num_hours_wind = ?        " + where );

        setString(updateStatement, 1, record.getUserid());
        setString(updateStatement, 2, record.getState_overlay());
        setString(updateStatement, 3, record.getCity_overlay());
        setString(updateStatement, 4, record.getRiver_overlay());
        setString(updateStatement, 5, record.getBasin_overlay());
        setString(updateStatement, 6, record.getRadar_overlay());
        setShort(updateStatement, 7, record.getNum_hours_wind());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(S3PostAnalPrefsRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM s3postanalprefs " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a S3PostAnalPrefsRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(S3PostAnalPrefsRecord oldRecord, S3PostAnalPrefsRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE s3postanalprefs SET userid = ?, state_overlay = ?, city_overlay = ?, river_overlay = ?, basin_overlay = ?, radar_overlay = ?, num_hours_wind = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getUserid());
        setString(updateStatement, 2, newRecord.getState_overlay());
        setString(updateStatement, 3, newRecord.getCity_overlay());
        setString(updateStatement, 4, newRecord.getRiver_overlay());
        setString(updateStatement, 5, newRecord.getBasin_overlay());
        setString(updateStatement, 6, newRecord.getRadar_overlay());
        setShort(updateStatement, 7, newRecord.getNum_hours_wind());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a S3PostAnalPrefsRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(S3PostAnalPrefsRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            S3PostAnalPrefsRecord oldRecord = (S3PostAnalPrefsRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of S3PostAnalPrefsTable class
