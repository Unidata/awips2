// filename: OfficeNotesTable.java
// author  : DBGEN
// created : Tue May 31 17:52:25 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              officenotes table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class OfficeNotesTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  OfficeNotesTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public OfficeNotesTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("officenotes");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of OfficeNotesRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        OfficeNotesRecord record = null;

        // create a List to hold OfficeNotes Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM officenotes " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a OfficeNotesRecord
            // and store its address in oneRecord
            record = new OfficeNotesRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a OfficeNotesRecord object

            record.setTopic(getString(rs, 1));
            record.setId(getString(rs, 2));
            record.setDatatime(getTimeStamp(rs, 3));
            record.setPostingtime(getTimeStamp(rs, 4));
            record.setUpdatetime(getTimeStamp(rs, 5));
            record.setExpiretime(getTimeStamp(rs, 6));
            record.setNote(getString(rs, 7));
            
            // add this OfficeNotesRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the OfficeNotesRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of OfficeNotesRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        OfficeNotesRecord record = null;

        // create a List to hold OfficeNotes Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM officenotes " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a OfficeNotesRecord
            // and store its address in oneRecord
            record = new OfficeNotesRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a OfficeNotesRecord object

            record.setTopic(getString(rs, 1));
            record.setId(getString(rs, 2));
            record.setDatatime(getTimeStamp(rs, 3));
            record.setPostingtime(getTimeStamp(rs, 4));
            record.setUpdatetime(getTimeStamp(rs, 5));
            record.setExpiretime(getTimeStamp(rs, 6));
            record.setNote(getString(rs, 7));
            
            // add this OfficeNotesRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the OfficeNotesRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a OfficeNotesRecord object and..
//-----------------------------------------------------------------
    public int insert(OfficeNotesRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO officenotes VALUES (?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getTopic());
        setString(insertStatement, 2, record.getId());
        setTimeStamp(insertStatement, 3, record.getDatatime());
        setTimeStamp(insertStatement, 4, record.getPostingtime());
        setTimeStamp(insertStatement, 5, record.getUpdatetime());
        setTimeStamp(insertStatement, 6, record.getExpiretime());
        setString(insertStatement, 7, record.getNote());
        
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
        String deleteStatement = "DELETE FROM officenotes " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a OfficeNotesRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(OfficeNotesRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE officenotes SET topic = ?, id = ?, datatime = ?, postingtime = ?, updatetime = ?, expiretime = ?, note = ?        " + where );

        setString(updateStatement, 1, record.getTopic());
        setString(updateStatement, 2, record.getId());
        setTimeStamp(updateStatement, 3, record.getDatatime());
        setTimeStamp(updateStatement, 4, record.getPostingtime());
        setTimeStamp(updateStatement, 5, record.getUpdatetime());
        setTimeStamp(updateStatement, 6, record.getExpiretime());
        setString(updateStatement, 7, record.getNote());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(OfficeNotesRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM officenotes " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a OfficeNotesRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(OfficeNotesRecord oldRecord, OfficeNotesRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE officenotes SET topic = ?, id = ?, datatime = ?, postingtime = ?, updatetime = ?, expiretime = ?, note = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getTopic());
        setString(updateStatement, 2, newRecord.getId());
        setTimeStamp(updateStatement, 3, newRecord.getDatatime());
        setTimeStamp(updateStatement, 4, newRecord.getPostingtime());
        setTimeStamp(updateStatement, 5, newRecord.getUpdatetime());
        setTimeStamp(updateStatement, 6, newRecord.getExpiretime());
        setString(updateStatement, 7, newRecord.getNote());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a OfficeNotesRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(OfficeNotesRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            OfficeNotesRecord oldRecord = (OfficeNotesRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of OfficeNotesTable class
