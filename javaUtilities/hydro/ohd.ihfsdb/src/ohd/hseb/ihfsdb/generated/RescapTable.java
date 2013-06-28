// filename: RescapTable.java
// author  : DBGEN
// created : Tue May 31 17:52:27 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              rescap table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class RescapTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  RescapTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public RescapTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("rescap");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of RescapRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        RescapRecord record = null;

        // create a List to hold Rescap Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM rescap " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RescapRecord
            // and store its address in oneRecord
            record = new RescapRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RescapRecord object

            record.setLid(getString(rs, 1));
            record.setElev(getDouble(rs, 2));
            record.setStorage(getDouble(rs, 3));
            
            // add this RescapRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the RescapRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of RescapRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        RescapRecord record = null;

        // create a List to hold Rescap Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM rescap " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RescapRecord
            // and store its address in oneRecord
            record = new RescapRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RescapRecord object

            record.setLid(getString(rs, 1));
            record.setElev(getDouble(rs, 2));
            record.setStorage(getDouble(rs, 3));
            
            // add this RescapRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the RescapRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a RescapRecord object and..
//-----------------------------------------------------------------
    public int insert(RescapRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO rescap VALUES (?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setDouble(insertStatement, 2, record.getElev());
        setDouble(insertStatement, 3, record.getStorage());
        
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
        String deleteStatement = "DELETE FROM rescap " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RescapRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RescapRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE rescap SET lid = ?, elev = ?, storage = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setDouble(updateStatement, 2, record.getElev());
        setDouble(updateStatement, 3, record.getStorage());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(RescapRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM rescap " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RescapRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RescapRecord oldRecord, RescapRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE rescap SET lid = ?, elev = ?, storage = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setDouble(updateStatement, 2, newRecord.getElev());
        setDouble(updateStatement, 3, newRecord.getStorage());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a RescapRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(RescapRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            RescapRecord oldRecord = (RescapRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of RescapTable class
