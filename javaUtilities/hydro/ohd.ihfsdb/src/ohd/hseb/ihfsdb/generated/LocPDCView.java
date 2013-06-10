// filename: LocPDCView.java
// author  : DBGEN
// created : Tue May 31 17:52:31 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              locpdc table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class LocPDCView extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  LocPDCTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public LocPDCView(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("locpdc");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of LocPDCRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        LocPDCRecord record = null;

        // create a List to hold LocPDC Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM locpdc " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a LocPDCRecord
            // and store its address in oneRecord
            record = new LocPDCRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a LocPDCRecord object

            record.setLid(getString(rs, 1));
            record.setName(getString(rs, 2));
            record.setLat(getDouble(rs, 3));
            record.setLon(getDouble(rs, 4));
            record.setHsa(getString(rs, 5));
            record.setPost(getInt(rs, 6));
            record.setElev(getDouble(rs, 7));
            record.setPrimary_pe(getString(rs, 8));
            record.setFs(getDouble(rs, 9));
            record.setFq(getDouble(rs, 10));
            record.setDisp_class(getString(rs, 11));
            record.setIs_dcp(getString(rs, 12));
            record.setIs_observer(getString(rs, 13));
            record.setTelem_type(getString(rs, 14));
            
            // add this LocPDCRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the LocPDCRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of LocPDCRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        LocPDCRecord record = null;

        // create a List to hold LocPDC Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM locpdc " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a LocPDCRecord
            // and store its address in oneRecord
            record = new LocPDCRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a LocPDCRecord object

            record.setLid(getString(rs, 1));
            record.setName(getString(rs, 2));
            record.setLat(getDouble(rs, 3));
            record.setLon(getDouble(rs, 4));
            record.setHsa(getString(rs, 5));
            record.setPost(getInt(rs, 6));
            record.setElev(getDouble(rs, 7));
            record.setPrimary_pe(getString(rs, 8));
            record.setFs(getDouble(rs, 9));
            record.setFq(getDouble(rs, 10));
            record.setDisp_class(getString(rs, 11));
            record.setIs_dcp(getString(rs, 12));
            record.setIs_observer(getString(rs, 13));
            record.setTelem_type(getString(rs, 14));
            
            // add this LocPDCRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the LocPDCRecord objects
        return recordList;

    } // end of selectNRecords method

} // end of LocPDCTable class
