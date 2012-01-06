// filename: UserPrefsTable.java
// author  : DBGEN
// created : Tue May 31 17:52:29 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              userprefs table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class UserPrefsTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  UserPrefsTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public UserPrefsTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("userprefs");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of UserPrefsRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        UserPrefsRecord record = null;

        // create a List to hold UserPrefs Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM userprefs " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a UserPrefsRecord
            // and store its address in oneRecord
            record = new UserPrefsRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a UserPrefsRecord object

            record.setUserid(getString(rs, 1));
            record.setTitle(getInt(rs, 2));
            record.setStatlist(getInt(rs, 3));
            record.setSortlist(getInt(rs, 4));
            record.setFieldlist(getInt(rs, 5));
            
            // add this UserPrefsRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the UserPrefsRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of UserPrefsRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        UserPrefsRecord record = null;

        // create a List to hold UserPrefs Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM userprefs " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a UserPrefsRecord
            // and store its address in oneRecord
            record = new UserPrefsRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a UserPrefsRecord object

            record.setUserid(getString(rs, 1));
            record.setTitle(getInt(rs, 2));
            record.setStatlist(getInt(rs, 3));
            record.setSortlist(getInt(rs, 4));
            record.setFieldlist(getInt(rs, 5));
            
            // add this UserPrefsRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the UserPrefsRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a UserPrefsRecord object and..
//-----------------------------------------------------------------
    public int insert(UserPrefsRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO userprefs VALUES (?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getUserid());
        setInt(insertStatement, 2, record.getTitle());
        setInt(insertStatement, 3, record.getStatlist());
        setInt(insertStatement, 4, record.getSortlist());
        setInt(insertStatement, 5, record.getFieldlist());
        
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
        String deleteStatement = "DELETE FROM userprefs " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a UserPrefsRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(UserPrefsRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE userprefs SET userid = ?, title = ?, statlist = ?, sortlist = ?, fieldlist = ?        " + where );

        setString(updateStatement, 1, record.getUserid());
        setInt(updateStatement, 2, record.getTitle());
        setInt(updateStatement, 3, record.getStatlist());
        setInt(updateStatement, 4, record.getSortlist());
        setInt(updateStatement, 5, record.getFieldlist());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(UserPrefsRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM userprefs " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a UserPrefsRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(UserPrefsRecord oldRecord, UserPrefsRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE userprefs SET userid = ?, title = ?, statlist = ?, sortlist = ?, fieldlist = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getUserid());
        setInt(updateStatement, 2, newRecord.getTitle());
        setInt(updateStatement, 3, newRecord.getStatlist());
        setInt(updateStatement, 4, newRecord.getSortlist());
        setInt(updateStatement, 5, newRecord.getFieldlist());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a UserPrefsRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(UserPrefsRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            UserPrefsRecord oldRecord = (UserPrefsRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of UserPrefsTable class
