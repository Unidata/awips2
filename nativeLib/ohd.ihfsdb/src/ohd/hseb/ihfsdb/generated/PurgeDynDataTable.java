// filename: PurgeDynDataTable.java
// author  : DBGEN
// created : Tue May 31 17:52:26 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              purgedyndata table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class PurgeDynDataTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  PurgeDynDataTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public PurgeDynDataTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("purgedyndata");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of PurgeDynDataRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        PurgeDynDataRecord record = null;

        // create a List to hold PurgeDynData Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM purgedyndata " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a PurgeDynDataRecord
            // and store its address in oneRecord
            record = new PurgeDynDataRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a PurgeDynDataRecord object

            record.setTable_name(getString(rs, 1));
            record.setTime_column_name(getString(rs, 2));
            record.setNum_hours_host(getInt(rs, 3));
            record.setNum_hours_backup(getInt(rs, 4));
            
            // add this PurgeDynDataRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the PurgeDynDataRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of PurgeDynDataRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        PurgeDynDataRecord record = null;

        // create a List to hold PurgeDynData Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM purgedyndata " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a PurgeDynDataRecord
            // and store its address in oneRecord
            record = new PurgeDynDataRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a PurgeDynDataRecord object

            record.setTable_name(getString(rs, 1));
            record.setTime_column_name(getString(rs, 2));
            record.setNum_hours_host(getInt(rs, 3));
            record.setNum_hours_backup(getInt(rs, 4));
            
            // add this PurgeDynDataRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the PurgeDynDataRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a PurgeDynDataRecord object and..
//-----------------------------------------------------------------
    public int insert(PurgeDynDataRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO purgedyndata VALUES (?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getTable_name());
        setString(insertStatement, 2, record.getTime_column_name());
        setInt(insertStatement, 3, record.getNum_hours_host());
        setInt(insertStatement, 4, record.getNum_hours_backup());
        
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
        String deleteStatement = "DELETE FROM purgedyndata " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a PurgeDynDataRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(PurgeDynDataRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE purgedyndata SET table_name = ?, time_column_name = ?, num_hours_host = ?, num_hours_backup = ?        " + where );

        setString(updateStatement, 1, record.getTable_name());
        setString(updateStatement, 2, record.getTime_column_name());
        setInt(updateStatement, 3, record.getNum_hours_host());
        setInt(updateStatement, 4, record.getNum_hours_backup());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(PurgeDynDataRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM purgedyndata " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a PurgeDynDataRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(PurgeDynDataRecord oldRecord, PurgeDynDataRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE purgedyndata SET table_name = ?, time_column_name = ?, num_hours_host = ?, num_hours_backup = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getTable_name());
        setString(updateStatement, 2, newRecord.getTime_column_name());
        setInt(updateStatement, 3, newRecord.getNum_hours_host());
        setInt(updateStatement, 4, newRecord.getNum_hours_backup());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a PurgeDynDataRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(PurgeDynDataRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            PurgeDynDataRecord oldRecord = (PurgeDynDataRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of PurgeDynDataTable class
