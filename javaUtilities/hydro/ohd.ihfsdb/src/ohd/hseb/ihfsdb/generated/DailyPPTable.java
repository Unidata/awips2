// filename: DailyPPTable.java
// author  : DBGEN
// created : Tue May 31 17:52:20 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              dailypp table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class DailyPPTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  DailyPPTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public DailyPPTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("dailypp");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of DailyPPRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        DailyPPRecord record = null;

        // create a List to hold DailyPP Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM dailypp " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a DailyPPRecord
            // and store its address in oneRecord
            record = new DailyPPRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a DailyPPRecord object

            record.setLid(getString(rs, 1));
            record.setTs(getString(rs, 2));
            record.setObstime(getTimeStamp(rs, 3));
            record.setValue(getDouble(rs, 4));
            record.setQc(getString(rs, 5));
            record.setPostingtime(getTimeStamp(rs, 6));
            
            // add this DailyPPRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the DailyPPRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of DailyPPRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        DailyPPRecord record = null;

        // create a List to hold DailyPP Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM dailypp " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a DailyPPRecord
            // and store its address in oneRecord
            record = new DailyPPRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a DailyPPRecord object

            record.setLid(getString(rs, 1));
            record.setTs(getString(rs, 2));
            record.setObstime(getTimeStamp(rs, 3));
            record.setValue(getDouble(rs, 4));
            record.setQc(getString(rs, 5));
            record.setPostingtime(getTimeStamp(rs, 6));
            
            // add this DailyPPRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the DailyPPRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a DailyPPRecord object and..
//-----------------------------------------------------------------
    public int insert(DailyPPRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO dailypp VALUES (?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getTs());
        setTimeStamp(insertStatement, 3, record.getObstime());
        setDouble(insertStatement, 4, record.getValue());
        setString(insertStatement, 5, record.getQc());
        setTimeStamp(insertStatement, 6, record.getPostingtime());
        
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
        String deleteStatement = "DELETE FROM dailypp " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a DailyPPRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(DailyPPRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE dailypp SET lid = ?, ts = ?, obstime = ?, value = ?, qc = ?, postingtime = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setString(updateStatement, 2, record.getTs());
        setTimeStamp(updateStatement, 3, record.getObstime());
        setDouble(updateStatement, 4, record.getValue());
        setString(updateStatement, 5, record.getQc());
        setTimeStamp(updateStatement, 6, record.getPostingtime());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(DailyPPRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM dailypp " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a DailyPPRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(DailyPPRecord oldRecord, DailyPPRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE dailypp SET lid = ?, ts = ?, obstime = ?, value = ?, qc = ?, postingtime = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getTs());
        setTimeStamp(updateStatement, 3, newRecord.getObstime());
        setDouble(updateStatement, 4, newRecord.getValue());
        setString(updateStatement, 5, newRecord.getQc());
        setTimeStamp(updateStatement, 6, newRecord.getPostingtime());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a DailyPPRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(DailyPPRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            DailyPPRecord oldRecord = (DailyPPRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of DailyPPTable class
