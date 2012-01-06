// filename: FpInfoView.java
// author  : DBGEN
// created : Tue May 31 17:52:31 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              fpinfo table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class FpInfoView extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  FpInfoTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public FpInfoView(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("fpinfo");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of FpInfoRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        FpInfoRecord record = null;

        // create a List to hold FpInfo Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM fpinfo " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a FpInfoRecord
            // and store its address in oneRecord
            record = new FpInfoRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a FpInfoRecord object

            record.setLid(getString(rs, 1));
            record.setName(getString(rs, 2));
            record.setCounty(getString(rs, 3));
            record.setState(getString(rs, 4));
            record.setHsa(getString(rs, 5));
            record.setPrimary_back(getString(rs, 6));
            record.setSecondary_back(getString(rs, 7));
            record.setStream(getString(rs, 8));
            record.setBf(getDouble(rs, 9));
            record.setWstg(getDouble(rs, 10));
            record.setFs(getDouble(rs, 11));
            record.setFq(getDouble(rs, 12));
            record.setAction_flow(getDouble(rs, 13));
            record.setPe(getString(rs, 14));
            record.setUse_latest_fcst(getString(rs, 15));
            record.setProximity(getString(rs, 16));
            record.setReach(getString(rs, 17));
            record.setGroup_id(getString(rs, 18));
            record.setOrdinal(getInt(rs, 19));
            record.setChg_threshold(getDouble(rs, 20));
            record.setRec_type(getString(rs, 21));
            record.setBackhrs(getInt(rs, 22));
            record.setForwardhrs(getInt(rs, 23));
            record.setAdjustendhrs(getDouble(rs, 24));
            record.setMinor_stage(getDouble(rs, 25));
            record.setModerate_stage(getDouble(rs, 26));
            record.setMajor_stage(getDouble(rs, 27));
            record.setMinor_flow(getDouble(rs, 28));
            record.setModerate_flow(getDouble(rs, 29));
            record.setMajor_flow(getDouble(rs, 30));
            
            // add this FpInfoRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the FpInfoRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of FpInfoRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        FpInfoRecord record = null;

        // create a List to hold FpInfo Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM fpinfo " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a FpInfoRecord
            // and store its address in oneRecord
            record = new FpInfoRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a FpInfoRecord object

            record.setLid(getString(rs, 1));
            record.setName(getString(rs, 2));
            record.setCounty(getString(rs, 3));
            record.setState(getString(rs, 4));
            record.setHsa(getString(rs, 5));
            record.setPrimary_back(getString(rs, 6));
            record.setSecondary_back(getString(rs, 7));
            record.setStream(getString(rs, 8));
            record.setBf(getDouble(rs, 9));
            record.setWstg(getDouble(rs, 10));
            record.setFs(getDouble(rs, 11));
            record.setFq(getDouble(rs, 12));
            record.setAction_flow(getDouble(rs, 13));
            record.setPe(getString(rs, 14));
            record.setUse_latest_fcst(getString(rs, 15));
            record.setProximity(getString(rs, 16));
            record.setReach(getString(rs, 17));
            record.setGroup_id(getString(rs, 18));
            record.setOrdinal(getInt(rs, 19));
            record.setChg_threshold(getDouble(rs, 20));
            record.setRec_type(getString(rs, 21));
            record.setBackhrs(getInt(rs, 22));
            record.setForwardhrs(getInt(rs, 23));
            record.setAdjustendhrs(getDouble(rs, 24));
            record.setMinor_stage(getDouble(rs, 25));
            record.setModerate_stage(getDouble(rs, 26));
            record.setMajor_stage(getDouble(rs, 27));
            record.setMinor_flow(getDouble(rs, 28));
            record.setModerate_flow(getDouble(rs, 29));
            record.setMajor_flow(getDouble(rs, 30));
            
            // add this FpInfoRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the FpInfoRecord objects
        return recordList;

    } // end of selectNRecords method

} // end of FpInfoTable class
