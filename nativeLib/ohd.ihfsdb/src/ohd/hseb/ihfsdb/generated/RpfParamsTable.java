// filename: RpfParamsTable.java
// author  : DBGEN
// created : Tue May 31 17:52:27 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              rpfparams table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class RpfParamsTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  RpfParamsTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public RpfParamsTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("rpfparams");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of RpfParamsRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        RpfParamsRecord record = null;

        // create a List to hold RpfParams Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM rpfparams " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RpfParamsRecord
            // and store its address in oneRecord
            record = new RpfParamsRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RpfParamsRecord object

            record.setObshrs(getInt(rs, 1));
            record.setFcsthrs(getInt(rs, 2));
            record.setMissval(getString(rs, 3));
            record.setMisscat(getString(rs, 4));
            record.setMisstim(getString(rs, 5));
            record.setRvsexphrs(getInt(rs, 6));
            record.setFlsexphrs(getInt(rs, 7));
            record.setFlwexphrs(getInt(rs, 8));
            
            // add this RpfParamsRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the RpfParamsRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of RpfParamsRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        RpfParamsRecord record = null;

        // create a List to hold RpfParams Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM rpfparams " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RpfParamsRecord
            // and store its address in oneRecord
            record = new RpfParamsRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RpfParamsRecord object

            record.setObshrs(getInt(rs, 1));
            record.setFcsthrs(getInt(rs, 2));
            record.setMissval(getString(rs, 3));
            record.setMisscat(getString(rs, 4));
            record.setMisstim(getString(rs, 5));
            record.setRvsexphrs(getInt(rs, 6));
            record.setFlsexphrs(getInt(rs, 7));
            record.setFlwexphrs(getInt(rs, 8));
            
            // add this RpfParamsRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the RpfParamsRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a RpfParamsRecord object and..
//-----------------------------------------------------------------
    public int insert(RpfParamsRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO rpfparams VALUES (?, ?, ?, ?, ?, ?, ?, ?        )");

        setInt(insertStatement, 1, record.getObshrs());
        setInt(insertStatement, 2, record.getFcsthrs());
        setString(insertStatement, 3, record.getMissval());
        setString(insertStatement, 4, record.getMisscat());
        setString(insertStatement, 5, record.getMisstim());
        setInt(insertStatement, 6, record.getRvsexphrs());
        setInt(insertStatement, 7, record.getFlsexphrs());
        setInt(insertStatement, 8, record.getFlwexphrs());
        
        // get the number of records processed by the insert
        returnCode = insertStatement.executeUpdate();

        return returnCode;

    } // end of insert method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(String where) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM rpfparams " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RpfParamsRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RpfParamsRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE rpfparams SET obshrs = ?, fcsthrs = ?, missval = ?, misscat = ?, misstim = ?, rvsexphrs = ?, flsexphrs = ?, flwexphrs = ?        " + where );

        setInt(updateStatement, 1, record.getObshrs());
        setInt(updateStatement, 2, record.getFcsthrs());
        setString(updateStatement, 3, record.getMissval());
        setString(updateStatement, 4, record.getMisscat());
        setString(updateStatement, 5, record.getMisstim());
        setInt(updateStatement, 6, record.getRvsexphrs());
        setInt(updateStatement, 7, record.getFlsexphrs());
        setInt(updateStatement, 8, record.getFlwexphrs());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

} // end of RpfParamsTable class
