// filename: LineSegsTable.java
// author  : DBGEN
// created : Tue May 31 17:52:24 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              linesegs table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class LineSegsTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  LineSegsTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public LineSegsTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("linesegs");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of LineSegsRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        LineSegsRecord record = null;

        // create a List to hold LineSegs Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM linesegs " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a LineSegsRecord
            // and store its address in oneRecord
            record = new LineSegsRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a LineSegsRecord object

            record.setArea_id(getString(rs, 1));
            record.setHrap_row(getInt(rs, 2));
            record.setHrap_beg_col(getInt(rs, 3));
            record.setHrap_end_col(getInt(rs, 4));
            record.setArea(getDouble(rs, 5));
            
            // add this LineSegsRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the LineSegsRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of LineSegsRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        LineSegsRecord record = null;

        // create a List to hold LineSegs Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM linesegs " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a LineSegsRecord
            // and store its address in oneRecord
            record = new LineSegsRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a LineSegsRecord object

            record.setArea_id(getString(rs, 1));
            record.setHrap_row(getInt(rs, 2));
            record.setHrap_beg_col(getInt(rs, 3));
            record.setHrap_end_col(getInt(rs, 4));
            record.setArea(getDouble(rs, 5));
            
            // add this LineSegsRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the LineSegsRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a LineSegsRecord object and..
//-----------------------------------------------------------------
    public int insert(LineSegsRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO linesegs VALUES (?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getArea_id());
        setInt(insertStatement, 2, record.getHrap_row());
        setInt(insertStatement, 3, record.getHrap_beg_col());
        setInt(insertStatement, 4, record.getHrap_end_col());
        setDouble(insertStatement, 5, record.getArea());
        
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
        String deleteStatement = "DELETE FROM linesegs " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a LineSegsRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(LineSegsRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE linesegs SET area_id = ?, hrap_row = ?, hrap_beg_col = ?, hrap_end_col = ?, area = ?        " + where );

        setString(updateStatement, 1, record.getArea_id());
        setInt(updateStatement, 2, record.getHrap_row());
        setInt(updateStatement, 3, record.getHrap_beg_col());
        setInt(updateStatement, 4, record.getHrap_end_col());
        setDouble(updateStatement, 5, record.getArea());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(LineSegsRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM linesegs " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a LineSegsRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(LineSegsRecord oldRecord, LineSegsRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE linesegs SET area_id = ?, hrap_row = ?, hrap_beg_col = ?, hrap_end_col = ?, area = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getArea_id());
        setInt(updateStatement, 2, newRecord.getHrap_row());
        setInt(updateStatement, 3, newRecord.getHrap_beg_col());
        setInt(updateStatement, 4, newRecord.getHrap_end_col());
        setDouble(updateStatement, 5, newRecord.getArea());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a LineSegsRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(LineSegsRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            LineSegsRecord oldRecord = (LineSegsRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of LineSegsTable class
