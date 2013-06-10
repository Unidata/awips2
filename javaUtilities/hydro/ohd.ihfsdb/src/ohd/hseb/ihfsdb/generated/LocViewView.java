// filename: LocViewView.java
// author  : DBGEN
// created : Tue May 31 17:52:31 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              locview table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class LocViewView extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  LocViewTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public LocViewView(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("locview");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of LocViewRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        LocViewRecord record = null;

        // create a List to hold LocView Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM locview " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a LocViewRecord
            // and store its address in oneRecord
            record = new LocViewRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a LocViewRecord object

            record.setLid(getString(rs, 1));
            record.setName(getString(rs, 2));
            record.setLat(getDouble(rs, 3));
            record.setLon(getDouble(rs, 4));
            record.setRb(getString(rs, 5));
            record.setState(getString(rs, 6));
            record.setCounty(getString(rs, 7));
            record.setType(getString(rs, 8));
            record.setWfo(getString(rs, 9));
            record.setHsa(getString(rs, 10));
            record.setPost(getInt(rs, 11));
            record.setStream(getString(rs, 12));
            record.setGsno(getString(rs, 13));
            
            // add this LocViewRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the LocViewRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of LocViewRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        LocViewRecord record = null;

        // create a List to hold LocView Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM locview " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a LocViewRecord
            // and store its address in oneRecord
            record = new LocViewRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a LocViewRecord object

            record.setLid(getString(rs, 1));
            record.setName(getString(rs, 2));
            record.setLat(getDouble(rs, 3));
            record.setLon(getDouble(rs, 4));
            record.setRb(getString(rs, 5));
            record.setState(getString(rs, 6));
            record.setCounty(getString(rs, 7));
            record.setType(getString(rs, 8));
            record.setWfo(getString(rs, 9));
            record.setHsa(getString(rs, 10));
            record.setPost(getInt(rs, 11));
            record.setStream(getString(rs, 12));
            record.setGsno(getString(rs, 13));
            
            // add this LocViewRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the LocViewRecord objects
        return recordList;

    } // end of selectNRecords method

} // end of LocViewTable class
