// filename: PerfLogTable.java
// author  : DBGEN
// created : Tue May 31 17:52:25 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              perflog table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class PerfLogTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  PerfLogTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public PerfLogTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("perflog");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of PerfLogRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        PerfLogRecord record = null;

        // create a List to hold PerfLog Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM perflog " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a PerfLogRecord
            // and store its address in oneRecord
            record = new PerfLogRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a PerfLogRecord object

            record.setProcess(getString(rs, 1));
            record.setStart_time(getTimeStamp(rs, 2));
            record.setNum_processed(getInt(rs, 3));
            record.setNum_reads(getInt(rs, 4));
            record.setNum_inserts(getInt(rs, 5));
            record.setNum_updates(getInt(rs, 6));
            record.setNum_deletes(getInt(rs, 7));
            record.setElapsed_time(getDouble(rs, 8));
            record.setCpu_time(getDouble(rs, 9));
            record.setIo_time(getDouble(rs, 10));
            
            // add this PerfLogRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the PerfLogRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of PerfLogRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        PerfLogRecord record = null;

        // create a List to hold PerfLog Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM perflog " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a PerfLogRecord
            // and store its address in oneRecord
            record = new PerfLogRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a PerfLogRecord object

            record.setProcess(getString(rs, 1));
            record.setStart_time(getTimeStamp(rs, 2));
            record.setNum_processed(getInt(rs, 3));
            record.setNum_reads(getInt(rs, 4));
            record.setNum_inserts(getInt(rs, 5));
            record.setNum_updates(getInt(rs, 6));
            record.setNum_deletes(getInt(rs, 7));
            record.setElapsed_time(getDouble(rs, 8));
            record.setCpu_time(getDouble(rs, 9));
            record.setIo_time(getDouble(rs, 10));
            
            // add this PerfLogRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the PerfLogRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a PerfLogRecord object and..
//-----------------------------------------------------------------
    public int insert(PerfLogRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO perflog VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getProcess());
        setTimeStamp(insertStatement, 2, record.getStart_time());
        setInt(insertStatement, 3, record.getNum_processed());
        setInt(insertStatement, 4, record.getNum_reads());
        setInt(insertStatement, 5, record.getNum_inserts());
        setInt(insertStatement, 6, record.getNum_updates());
        setInt(insertStatement, 7, record.getNum_deletes());
        setDouble(insertStatement, 8, record.getElapsed_time());
        setDouble(insertStatement, 9, record.getCpu_time());
        setDouble(insertStatement, 10, record.getIo_time());
        
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
        String deleteStatement = "DELETE FROM perflog " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a PerfLogRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(PerfLogRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE perflog SET process = ?, start_time = ?, num_processed = ?, num_reads = ?, num_inserts = ?, num_updates = ?, num_deletes = ?, elapsed_time = ?, cpu_time = ?, io_time = ?        " + where );

        setString(updateStatement, 1, record.getProcess());
        setTimeStamp(updateStatement, 2, record.getStart_time());
        setInt(updateStatement, 3, record.getNum_processed());
        setInt(updateStatement, 4, record.getNum_reads());
        setInt(updateStatement, 5, record.getNum_inserts());
        setInt(updateStatement, 6, record.getNum_updates());
        setInt(updateStatement, 7, record.getNum_deletes());
        setDouble(updateStatement, 8, record.getElapsed_time());
        setDouble(updateStatement, 9, record.getCpu_time());
        setDouble(updateStatement, 10, record.getIo_time());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(PerfLogRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM perflog " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a PerfLogRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(PerfLogRecord oldRecord, PerfLogRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE perflog SET process = ?, start_time = ?, num_processed = ?, num_reads = ?, num_inserts = ?, num_updates = ?, num_deletes = ?, elapsed_time = ?, cpu_time = ?, io_time = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getProcess());
        setTimeStamp(updateStatement, 2, newRecord.getStart_time());
        setInt(updateStatement, 3, newRecord.getNum_processed());
        setInt(updateStatement, 4, newRecord.getNum_reads());
        setInt(updateStatement, 5, newRecord.getNum_inserts());
        setInt(updateStatement, 6, newRecord.getNum_updates());
        setInt(updateStatement, 7, newRecord.getNum_deletes());
        setDouble(updateStatement, 8, newRecord.getElapsed_time());
        setDouble(updateStatement, 9, newRecord.getCpu_time());
        setDouble(updateStatement, 10, newRecord.getIo_time());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a PerfLogRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(PerfLogRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            PerfLogRecord oldRecord = (PerfLogRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of PerfLogTable class
