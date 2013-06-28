// filename: FloodstmtTable.java
// author  : DBGEN
// created : Tue May 31 17:52:22 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              floodstmt table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class FloodstmtTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  FloodstmtTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public FloodstmtTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("floodstmt");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of FloodstmtRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        FloodstmtRecord record = null;

        // create a List to hold Floodstmt Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM floodstmt " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a FloodstmtRecord
            // and store its address in oneRecord
            record = new FloodstmtRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a FloodstmtRecord object

            record.setLid(getString(rs, 1));
            record.setImpact_value(getDouble(rs, 2));
            record.setStatement(getString(rs, 3));
            record.setRf(getString(rs, 4));
            record.setDatestart(getString(rs, 5));
            record.setDateend(getString(rs, 6));
            record.setImpact_pe(getString(rs, 7));
            
            // add this FloodstmtRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the FloodstmtRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of FloodstmtRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        FloodstmtRecord record = null;

        // create a List to hold Floodstmt Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM floodstmt " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a FloodstmtRecord
            // and store its address in oneRecord
            record = new FloodstmtRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a FloodstmtRecord object

            record.setLid(getString(rs, 1));
            record.setImpact_value(getDouble(rs, 2));
            record.setStatement(getString(rs, 3));
            record.setRf(getString(rs, 4));
            record.setDatestart(getString(rs, 5));
            record.setDateend(getString(rs, 6));
            record.setImpact_pe(getString(rs, 7));
            
            // add this FloodstmtRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the FloodstmtRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a FloodstmtRecord object and..
//-----------------------------------------------------------------
    public int insert(FloodstmtRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO floodstmt VALUES (?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setDouble(insertStatement, 2, record.getImpact_value());
        setString(insertStatement, 3, record.getStatement());
        setString(insertStatement, 4, record.getRf());
        setString(insertStatement, 5, record.getDatestart());
        setString(insertStatement, 6, record.getDateend());
        setString(insertStatement, 7, record.getImpact_pe());
        
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
        String deleteStatement = "DELETE FROM floodstmt " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a FloodstmtRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(FloodstmtRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE floodstmt SET lid = ?, impact_value = ?, statement = ?, rf = ?, datestart = ?, dateend = ?, impact_pe = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setDouble(updateStatement, 2, record.getImpact_value());
        setString(updateStatement, 3, record.getStatement());
        setString(updateStatement, 4, record.getRf());
        setString(updateStatement, 5, record.getDatestart());
        setString(updateStatement, 6, record.getDateend());
        setString(updateStatement, 7, record.getImpact_pe());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(FloodstmtRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM floodstmt " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a FloodstmtRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(FloodstmtRecord oldRecord, FloodstmtRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE floodstmt SET lid = ?, impact_value = ?, statement = ?, rf = ?, datestart = ?, dateend = ?, impact_pe = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setDouble(updateStatement, 2, newRecord.getImpact_value());
        setString(updateStatement, 3, newRecord.getStatement());
        setString(updateStatement, 4, newRecord.getRf());
        setString(updateStatement, 5, newRecord.getDatestart());
        setString(updateStatement, 6, newRecord.getDateend());
        setString(updateStatement, 7, newRecord.getImpact_pe());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a FloodstmtRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(FloodstmtRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            FloodstmtRecord oldRecord = (FloodstmtRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of FloodstmtTable class
