// filename: LWstmtTable.java
// author  : DBGEN
// created : Tue May 31 17:52:24 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              lwstmt table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class LWstmtTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  LWstmtTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public LWstmtTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("lwstmt");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of LWstmtRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        LWstmtRecord record = null;

        // create a List to hold LWstmt Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM lwstmt " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a LWstmtRecord
            // and store its address in oneRecord
            record = new LWstmtRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a LWstmtRecord object

            record.setLid(getString(rs, 1));
            record.setPe(getString(rs, 2));
            record.setLower_value(getDouble(rs, 3));
            record.setUpper_value(getDouble(rs, 4));
            record.setCriteria_rank(getInt(rs, 5));
            record.setStatement(getString(rs, 6));
            record.setLw_criteria(getString(rs, 7));
            record.setLw_source(getString(rs, 8));
            
            // add this LWstmtRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the LWstmtRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of LWstmtRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        LWstmtRecord record = null;

        // create a List to hold LWstmt Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM lwstmt " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a LWstmtRecord
            // and store its address in oneRecord
            record = new LWstmtRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a LWstmtRecord object

            record.setLid(getString(rs, 1));
            record.setPe(getString(rs, 2));
            record.setLower_value(getDouble(rs, 3));
            record.setUpper_value(getDouble(rs, 4));
            record.setCriteria_rank(getInt(rs, 5));
            record.setStatement(getString(rs, 6));
            record.setLw_criteria(getString(rs, 7));
            record.setLw_source(getString(rs, 8));
            
            // add this LWstmtRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the LWstmtRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a LWstmtRecord object and..
//-----------------------------------------------------------------
    public int insert(LWstmtRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO lwstmt VALUES (?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getPe());
        setDouble(insertStatement, 3, record.getLower_value());
        setDouble(insertStatement, 4, record.getUpper_value());
        setInt(insertStatement, 5, record.getCriteria_rank());
        setString(insertStatement, 6, record.getStatement());
        setString(insertStatement, 7, record.getLw_criteria());
        setString(insertStatement, 8, record.getLw_source());
        
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
        String deleteStatement = "DELETE FROM lwstmt " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a LWstmtRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(LWstmtRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE lwstmt SET lid = ?, pe = ?, lower_value = ?, upper_value = ?, criteria_rank = ?, statement = ?, lw_criteria = ?, lw_source = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setString(updateStatement, 2, record.getPe());
        setDouble(updateStatement, 3, record.getLower_value());
        setDouble(updateStatement, 4, record.getUpper_value());
        setInt(updateStatement, 5, record.getCriteria_rank());
        setString(updateStatement, 6, record.getStatement());
        setString(updateStatement, 7, record.getLw_criteria());
        setString(updateStatement, 8, record.getLw_source());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(LWstmtRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM lwstmt " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a LWstmtRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(LWstmtRecord oldRecord, LWstmtRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE lwstmt SET lid = ?, pe = ?, lower_value = ?, upper_value = ?, criteria_rank = ?, statement = ?, lw_criteria = ?, lw_source = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getPe());
        setDouble(updateStatement, 3, newRecord.getLower_value());
        setDouble(updateStatement, 4, newRecord.getUpper_value());
        setInt(updateStatement, 5, newRecord.getCriteria_rank());
        setString(updateStatement, 6, newRecord.getStatement());
        setString(updateStatement, 7, newRecord.getLw_criteria());
        setString(updateStatement, 8, newRecord.getLw_source());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a LWstmtRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(LWstmtRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            LWstmtRecord oldRecord = (LWstmtRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of LWstmtTable class
