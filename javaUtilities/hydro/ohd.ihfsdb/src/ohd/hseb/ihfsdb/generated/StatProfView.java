// filename: StatProfView.java
// author  : DBGEN
// created : Tue May 31 17:52:31 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              statprof table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class StatProfView extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  StatProfTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public StatProfView(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("statprof");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of StatProfRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        StatProfRecord record = null;

        // create a List to hold StatProf Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM statprof " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a StatProfRecord
            // and store its address in oneRecord
            record = new StatProfRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a StatProfRecord object

            record.setLid(getString(rs, 1));
            record.setName(getString(rs, 2));
            record.setPrimary_pe(getString(rs, 3));
            record.setStream(getString(rs, 4));
            record.setFs(getDouble(rs, 5));
            record.setWstg(getDouble(rs, 6));
            record.setFq(getDouble(rs, 7));
            record.setAction_flow(getDouble(rs, 8));
            record.setZd(getDouble(rs, 9));
            record.setMile(getDouble(rs, 10));
            record.setReach(getString(rs, 11));
            record.setProximity(getString(rs, 12));
            
            // add this StatProfRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the StatProfRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of StatProfRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        StatProfRecord record = null;

        // create a List to hold StatProf Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM statprof " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a StatProfRecord
            // and store its address in oneRecord
            record = new StatProfRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a StatProfRecord object

            record.setLid(getString(rs, 1));
            record.setName(getString(rs, 2));
            record.setPrimary_pe(getString(rs, 3));
            record.setStream(getString(rs, 4));
            record.setFs(getDouble(rs, 5));
            record.setWstg(getDouble(rs, 6));
            record.setFq(getDouble(rs, 7));
            record.setAction_flow(getDouble(rs, 8));
            record.setZd(getDouble(rs, 9));
            record.setMile(getDouble(rs, 10));
            record.setReach(getString(rs, 11));
            record.setProximity(getString(rs, 12));
            
            // add this StatProfRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the StatProfRecord objects
        return recordList;

    } // end of selectNRecords method

} // end of StatProfTable class
