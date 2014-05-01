// filename: ServiceTableViewView.java
// author  : DBGEN
// created : Tue May 31 17:52:31 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              servicetableview table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class ServiceTableViewView extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  ServiceTableViewTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public ServiceTableViewView(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("servicetableview");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of ServiceTableViewRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        ServiceTableViewRecord record = null;

        // create a List to hold ServiceTableView Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM servicetableview " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a ServiceTableViewRecord
            // and store its address in oneRecord
            record = new ServiceTableViewRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a ServiceTableViewRecord object

            record.setLid(getString(rs, 1));
            record.setName(getString(rs, 2));
            record.setStream(getString(rs, 3));
            record.setState(getString(rs, 4));
            record.setCounty(getString(rs, 5));
            record.setHsa(getString(rs, 6));
            
            // add this ServiceTableViewRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the ServiceTableViewRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of ServiceTableViewRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        ServiceTableViewRecord record = null;

        // create a List to hold ServiceTableView Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM servicetableview " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a ServiceTableViewRecord
            // and store its address in oneRecord
            record = new ServiceTableViewRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a ServiceTableViewRecord object

            record.setLid(getString(rs, 1));
            record.setName(getString(rs, 2));
            record.setStream(getString(rs, 3));
            record.setState(getString(rs, 4));
            record.setCounty(getString(rs, 5));
            record.setHsa(getString(rs, 6));
            
            // add this ServiceTableViewRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the ServiceTableViewRecord objects
        return recordList;

    } // end of selectNRecords method

} // end of ServiceTableViewTable class
