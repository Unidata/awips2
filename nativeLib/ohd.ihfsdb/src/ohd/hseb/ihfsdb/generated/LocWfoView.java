// filename: LocWfoView.java
// author  : DBGEN
// created : Tue May 31 17:52:31 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              locwfo table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class LocWfoView extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  LocWfoTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public LocWfoView(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("locwfo");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of LocWfoRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        LocWfoRecord record = null;

        // create a List to hold LocWfo Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM locwfo " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a LocWfoRecord
            // and store its address in oneRecord
            record = new LocWfoRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a LocWfoRecord object

            record.setLid(getString(rs, 1));
            record.setCounty(getString(rs, 2));
            record.setState(getString(rs, 3));
            record.setWfo(getString(rs, 4));
            record.setPrimary_back(getString(rs, 5));
            record.setSecondary_back(getString(rs, 6));
            
            // add this LocWfoRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the LocWfoRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of LocWfoRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        LocWfoRecord record = null;

        // create a List to hold LocWfo Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM locwfo " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a LocWfoRecord
            // and store its address in oneRecord
            record = new LocWfoRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a LocWfoRecord object

            record.setLid(getString(rs, 1));
            record.setCounty(getString(rs, 2));
            record.setState(getString(rs, 3));
            record.setWfo(getString(rs, 4));
            record.setPrimary_back(getString(rs, 5));
            record.setSecondary_back(getString(rs, 6));
            
            // add this LocWfoRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the LocWfoRecord objects
        return recordList;

    } // end of selectNRecords method

} // end of LocWfoTable class
