// filename: LocRiverMonView.java
// author  : DBGEN
// created : Tue May 31 17:52:31 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              locrivermon table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class LocRiverMonView extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  LocRiverMonTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public LocRiverMonView(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("locrivermon");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of LocRiverMonRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        LocRiverMonRecord record = null;

        // create a List to hold LocRiverMon Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM locrivermon " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a LocRiverMonRecord
            // and store its address in oneRecord
            record = new LocRiverMonRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a LocRiverMonRecord object

            record.setLid(getString(rs, 1));
            record.setName(getString(rs, 2));
            record.setCounty(getString(rs, 3));
            record.setState(getString(rs, 4));
            record.setHsa(getString(rs, 5));
            record.setStream(getString(rs, 6));
            record.setBankfull(getDouble(rs, 7));
            record.setAction_stage(getDouble(rs, 8));
            record.setFlood_stage(getDouble(rs, 9));
            record.setFlood_flow(getDouble(rs, 10));
            record.setAction_flow(getDouble(rs, 11));
            record.setPrimary_pe(getString(rs, 12));
            record.setProximity(getString(rs, 13));
            record.setReach(getString(rs, 14));
            record.setMile(getDouble(rs, 15));
            record.setMinor(getDouble(rs, 16));
            record.setModerate(getDouble(rs, 17));
            record.setMajor(getDouble(rs, 18));
            
            // add this LocRiverMonRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the LocRiverMonRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of LocRiverMonRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        LocRiverMonRecord record = null;

        // create a List to hold LocRiverMon Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM locrivermon " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a LocRiverMonRecord
            // and store its address in oneRecord
            record = new LocRiverMonRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a LocRiverMonRecord object

            record.setLid(getString(rs, 1));
            record.setName(getString(rs, 2));
            record.setCounty(getString(rs, 3));
            record.setState(getString(rs, 4));
            record.setHsa(getString(rs, 5));
            record.setStream(getString(rs, 6));
            record.setBankfull(getDouble(rs, 7));
            record.setAction_stage(getDouble(rs, 8));
            record.setFlood_stage(getDouble(rs, 9));
            record.setFlood_flow(getDouble(rs, 10));
            record.setAction_flow(getDouble(rs, 11));
            record.setPrimary_pe(getString(rs, 12));
            record.setProximity(getString(rs, 13));
            record.setReach(getString(rs, 14));
            record.setMile(getDouble(rs, 15));
            record.setMinor(getDouble(rs, 16));
            record.setModerate(getDouble(rs, 17));
            record.setMajor(getDouble(rs, 18));
            
            // add this LocRiverMonRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the LocRiverMonRecord objects
        return recordList;

    } // end of selectNRecords method

} // end of LocRiverMonTable class
