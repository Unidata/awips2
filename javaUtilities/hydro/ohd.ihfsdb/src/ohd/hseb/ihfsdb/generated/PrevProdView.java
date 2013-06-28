// filename: PrevProdView.java
// author  : DBGEN
// created : Tue May 31 17:52:31 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              prevprod table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class PrevProdView extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  PrevProdTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public PrevProdView(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("prevprod");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of PrevProdRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        PrevProdRecord record = null;

        // create a List to hold PrevProd Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM prevprod " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a PrevProdRecord
            // and store its address in oneRecord
            record = new PrevProdRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a PrevProdRecord object

            record.setProduct_id(getString(rs, 1));
            record.setProducttime(getTimeStamp(rs, 2));
            record.setPostingtime(getTimeStamp(rs, 3));
            record.setProdtype(getString(rs, 4));
            record.setIssnum(getInt(rs, 5));
            
            // add this PrevProdRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the PrevProdRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of PrevProdRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        PrevProdRecord record = null;

        // create a List to hold PrevProd Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM prevprod " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a PrevProdRecord
            // and store its address in oneRecord
            record = new PrevProdRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a PrevProdRecord object

            record.setProduct_id(getString(rs, 1));
            record.setProducttime(getTimeStamp(rs, 2));
            record.setPostingtime(getTimeStamp(rs, 3));
            record.setProdtype(getString(rs, 4));
            record.setIssnum(getInt(rs, 5));
            
            // add this PrevProdRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the PrevProdRecord objects
        return recordList;

    } // end of selectNRecords method

} // end of PrevProdTable class
