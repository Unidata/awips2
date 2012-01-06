// filename: LocClassView.java
// author  : DBGEN
// created : Tue May 31 17:52:31 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              locclass table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class LocClassView extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  LocClassTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public LocClassView(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("locclass");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of LocClassRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        LocClassRecord record = null;

        // create a List to hold LocClass Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM locclass " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a LocClassRecord
            // and store its address in oneRecord
            record = new LocClassRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a LocClassRecord object

            record.setLid(getString(rs, 1));
            record.setName(getString(rs, 2));
            record.setLat(getDouble(rs, 3));
            record.setLon(getDouble(rs, 4));
            record.setWfo(getString(rs, 5));
            record.setHsa(getString(rs, 6));
            record.setPost(getInt(rs, 7));
            record.setDisp_class(getString(rs, 8));
            record.setIs_dcp(getString(rs, 9));
            record.setIs_observer(getString(rs, 10));
            record.setTelem_type(getString(rs, 11));
            
            // add this LocClassRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the LocClassRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of LocClassRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        LocClassRecord record = null;

        // create a List to hold LocClass Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM locclass " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a LocClassRecord
            // and store its address in oneRecord
            record = new LocClassRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a LocClassRecord object

            record.setLid(getString(rs, 1));
            record.setName(getString(rs, 2));
            record.setLat(getDouble(rs, 3));
            record.setLon(getDouble(rs, 4));
            record.setWfo(getString(rs, 5));
            record.setHsa(getString(rs, 6));
            record.setPost(getInt(rs, 7));
            record.setDisp_class(getString(rs, 8));
            record.setIs_dcp(getString(rs, 9));
            record.setIs_observer(getString(rs, 10));
            record.setTelem_type(getString(rs, 11));
            
            // add this LocClassRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the LocClassRecord objects
        return recordList;

    } // end of selectNRecords method

} // end of LocClassTable class
