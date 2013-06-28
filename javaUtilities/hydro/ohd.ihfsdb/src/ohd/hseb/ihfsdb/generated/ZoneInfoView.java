// filename: ZoneInfoView.java
// author  : DBGEN
// created : Tue May 31 17:52:31 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              zoneinfo table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class ZoneInfoView extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  ZoneInfoTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public ZoneInfoView(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("zoneinfo");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of ZoneInfoRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        ZoneInfoRecord record = null;

        // create a List to hold ZoneInfo Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM zoneinfo " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a ZoneInfoRecord
            // and store its address in oneRecord
            record = new ZoneInfoRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a ZoneInfoRecord object

            record.setLid(getString(rs, 1));
            record.setState(getString(rs, 2));
            record.setZonenum(getString(rs, 3));
            record.setDescr(getString(rs, 4));
            
            // add this ZoneInfoRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the ZoneInfoRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of ZoneInfoRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        ZoneInfoRecord record = null;

        // create a List to hold ZoneInfo Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM zoneinfo " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a ZoneInfoRecord
            // and store its address in oneRecord
            record = new ZoneInfoRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a ZoneInfoRecord object

            record.setLid(getString(rs, 1));
            record.setState(getString(rs, 2));
            record.setZonenum(getString(rs, 3));
            record.setDescr(getString(rs, 4));
            
            // add this ZoneInfoRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the ZoneInfoRecord objects
        return recordList;

    } // end of selectNRecords method

} // end of ZoneInfoTable class
