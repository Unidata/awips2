// filename: RatingShiftTable.java
// author  : DBGEN
// created : Tue May 31 17:52:26 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              ratingshift table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class RatingShiftTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  RatingShiftTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public RatingShiftTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("ratingshift");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of RatingShiftRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        RatingShiftRecord record = null;

        // create a List to hold RatingShift Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM ratingshift " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RatingShiftRecord
            // and store its address in oneRecord
            record = new RatingShiftRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RatingShiftRecord object

            record.setLid(getString(rs, 1));
            record.setDate(getDate(rs, 2));
            record.setShift_amount(getDouble(rs, 3));
            record.setActive(getString(rs, 4));
            
            // add this RatingShiftRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the RatingShiftRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of RatingShiftRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        RatingShiftRecord record = null;

        // create a List to hold RatingShift Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM ratingshift " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RatingShiftRecord
            // and store its address in oneRecord
            record = new RatingShiftRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RatingShiftRecord object

            record.setLid(getString(rs, 1));
            record.setDate(getDate(rs, 2));
            record.setShift_amount(getDouble(rs, 3));
            record.setActive(getString(rs, 4));
            
            // add this RatingShiftRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the RatingShiftRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a RatingShiftRecord object and..
//-----------------------------------------------------------------
    public int insert(RatingShiftRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO ratingshift VALUES (?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setDate(insertStatement, 2, record.getDate());
        setDouble(insertStatement, 3, record.getShift_amount());
        setString(insertStatement, 4, record.getActive());
        
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
        String deleteStatement = "DELETE FROM ratingshift " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RatingShiftRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RatingShiftRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE ratingshift SET lid = ?, date = ?, shift_amount = ?, active = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setDate(updateStatement, 2, record.getDate());
        setDouble(updateStatement, 3, record.getShift_amount());
        setString(updateStatement, 4, record.getActive());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(RatingShiftRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM ratingshift " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RatingShiftRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RatingShiftRecord oldRecord, RatingShiftRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE ratingshift SET lid = ?, date = ?, shift_amount = ?, active = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setDate(updateStatement, 2, newRecord.getDate());
        setDouble(updateStatement, 3, newRecord.getShift_amount());
        setString(updateStatement, 4, newRecord.getActive());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a RatingShiftRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(RatingShiftRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            RatingShiftRecord oldRecord = (RatingShiftRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of RatingShiftTable class
