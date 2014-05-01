// filename: GeoAreaTable.java
// author  : DBGEN
// created : Tue May 31 17:52:23 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              geoarea table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class GeoAreaTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  GeoAreaTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public GeoAreaTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("geoarea");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of GeoAreaRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        GeoAreaRecord record = null;

        // create a List to hold GeoArea Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM geoarea " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a GeoAreaRecord
            // and store its address in oneRecord
            record = new GeoAreaRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a GeoAreaRecord object

            record.setArea_id(getString(rs, 1));
            record.setName(getString(rs, 2));
            record.setBoundary_type(getString(rs, 3));
            record.setInterior_lat(getDouble(rs, 4));
            record.setInterior_lon(getDouble(rs, 5));
            
            // add this GeoAreaRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the GeoAreaRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of GeoAreaRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        GeoAreaRecord record = null;

        // create a List to hold GeoArea Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM geoarea " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a GeoAreaRecord
            // and store its address in oneRecord
            record = new GeoAreaRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a GeoAreaRecord object

            record.setArea_id(getString(rs, 1));
            record.setName(getString(rs, 2));
            record.setBoundary_type(getString(rs, 3));
            record.setInterior_lat(getDouble(rs, 4));
            record.setInterior_lon(getDouble(rs, 5));
            
            // add this GeoAreaRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the GeoAreaRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a GeoAreaRecord object and..
//-----------------------------------------------------------------
    public int insert(GeoAreaRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO geoarea VALUES (?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getArea_id());
        setString(insertStatement, 2, record.getName());
        setString(insertStatement, 3, record.getBoundary_type());
        setDouble(insertStatement, 4, record.getInterior_lat());
        setDouble(insertStatement, 5, record.getInterior_lon());
        
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
        String deleteStatement = "DELETE FROM geoarea " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a GeoAreaRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(GeoAreaRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE geoarea SET area_id = ?, name = ?, boundary_type = ?, interior_lat = ?, interior_lon = ?        " + where );

        setString(updateStatement, 1, record.getArea_id());
        setString(updateStatement, 2, record.getName());
        setString(updateStatement, 3, record.getBoundary_type());
        setDouble(updateStatement, 4, record.getInterior_lat());
        setDouble(updateStatement, 5, record.getInterior_lon());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(GeoAreaRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM geoarea " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a GeoAreaRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(GeoAreaRecord oldRecord, GeoAreaRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE geoarea SET area_id = ?, name = ?, boundary_type = ?, interior_lat = ?, interior_lon = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getArea_id());
        setString(updateStatement, 2, newRecord.getName());
        setString(updateStatement, 3, newRecord.getBoundary_type());
        setDouble(updateStatement, 4, newRecord.getInterior_lat());
        setDouble(updateStatement, 5, newRecord.getInterior_lon());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a GeoAreaRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(GeoAreaRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            GeoAreaRecord oldRecord = (GeoAreaRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of GeoAreaTable class
