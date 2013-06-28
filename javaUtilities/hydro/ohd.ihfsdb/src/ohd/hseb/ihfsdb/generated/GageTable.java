// filename: GageTable.java
// author  : DBGEN
// created : Tue May 31 17:52:23 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              gage table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class GageTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  GageTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public GageTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("gage");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of GageRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        GageRecord record = null;

        // create a List to hold Gage Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM gage " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a GageRecord
            // and store its address in oneRecord
            record = new GageRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a GageRecord object

            record.setLid(getString(rs, 1));
            record.setGbegin(getDate(rs, 2));
            record.setType(getString(rs, 3));
            record.setGend(getDate(rs, 4));
            record.setRemark(getString(rs, 5));
            record.setMaint(getString(rs, 6));
            record.setOwner(getString(rs, 7));
            
            // add this GageRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the GageRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of GageRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        GageRecord record = null;

        // create a List to hold Gage Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM gage " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a GageRecord
            // and store its address in oneRecord
            record = new GageRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a GageRecord object

            record.setLid(getString(rs, 1));
            record.setGbegin(getDate(rs, 2));
            record.setType(getString(rs, 3));
            record.setGend(getDate(rs, 4));
            record.setRemark(getString(rs, 5));
            record.setMaint(getString(rs, 6));
            record.setOwner(getString(rs, 7));
            
            // add this GageRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the GageRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a GageRecord object and..
//-----------------------------------------------------------------
    public int insert(GageRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO gage VALUES (?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setDate(insertStatement, 2, record.getGbegin());
        setString(insertStatement, 3, record.getType());
        setDate(insertStatement, 4, record.getGend());
        setString(insertStatement, 5, record.getRemark());
        setString(insertStatement, 6, record.getMaint());
        setString(insertStatement, 7, record.getOwner());
        
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
        String deleteStatement = "DELETE FROM gage " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a GageRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(GageRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE gage SET lid = ?, gbegin = ?, type = ?, gend = ?, remark = ?, maint = ?, owner = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setDate(updateStatement, 2, record.getGbegin());
        setString(updateStatement, 3, record.getType());
        setDate(updateStatement, 4, record.getGend());
        setString(updateStatement, 5, record.getRemark());
        setString(updateStatement, 6, record.getMaint());
        setString(updateStatement, 7, record.getOwner());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(GageRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM gage " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a GageRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(GageRecord oldRecord, GageRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE gage SET lid = ?, gbegin = ?, type = ?, gend = ?, remark = ?, maint = ?, owner = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setDate(updateStatement, 2, newRecord.getGbegin());
        setString(updateStatement, 3, newRecord.getType());
        setDate(updateStatement, 4, newRecord.getGend());
        setString(updateStatement, 5, newRecord.getRemark());
        setString(updateStatement, 6, newRecord.getMaint());
        setString(updateStatement, 7, newRecord.getOwner());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a GageRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(GageRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            GageRecord oldRecord = (GageRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of GageTable class
