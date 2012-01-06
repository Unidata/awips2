// filename: HvStationView.java
// author  : DBGEN
// created : Tue May 31 17:52:31 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              hvstation table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class HvStationView extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  HvStationTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public HvStationView(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("hvstation");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of HvStationRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        HvStationRecord record = null;

        // create a List to hold HvStation Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM hvstation " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a HvStationRecord
            // and store its address in oneRecord
            record = new HvStationRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a HvStationRecord object

            record.setLid(getString(rs, 1));
            record.setName(getString(rs, 2));
            record.setLat(getDouble(rs, 3));
            record.setLon(getDouble(rs, 4));
            record.setStream_name(getString(rs, 5));
            record.setPrimary_pe(getString(rs, 6));
            record.setFlood_stage(getDouble(rs, 7));
            record.setFlood_flow(getDouble(rs, 8));
            record.setAction_stage(getDouble(rs, 9));
            record.setAction_flow(getDouble(rs, 10));
            record.setDisp_class(getString(rs, 11));
            record.setIs_dcp(getString(rs, 12));
            record.setIs_observer(getString(rs, 13));
            record.setTelem_type(getString(rs, 14));
            
            // add this HvStationRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the HvStationRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of HvStationRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        HvStationRecord record = null;

        // create a List to hold HvStation Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM hvstation " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a HvStationRecord
            // and store its address in oneRecord
            record = new HvStationRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a HvStationRecord object

            record.setLid(getString(rs, 1));
            record.setName(getString(rs, 2));
            record.setLat(getDouble(rs, 3));
            record.setLon(getDouble(rs, 4));
            record.setStream_name(getString(rs, 5));
            record.setPrimary_pe(getString(rs, 6));
            record.setFlood_stage(getDouble(rs, 7));
            record.setFlood_flow(getDouble(rs, 8));
            record.setAction_stage(getDouble(rs, 9));
            record.setAction_flow(getDouble(rs, 10));
            record.setDisp_class(getString(rs, 11));
            record.setIs_dcp(getString(rs, 12));
            record.setIs_observer(getString(rs, 13));
            record.setTelem_type(getString(rs, 14));
            
            // add this HvStationRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the HvStationRecord objects
        return recordList;

    } // end of selectNRecords method

} // end of HvStationTable class
