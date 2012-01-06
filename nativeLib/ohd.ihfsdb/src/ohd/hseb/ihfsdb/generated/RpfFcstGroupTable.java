// filename: RpfFcstGroupTable.java
// author  : DBGEN
// created : Tue May 31 17:52:27 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              rpffcstgroup table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class RpfFcstGroupTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  RpfFcstGroupTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public RpfFcstGroupTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("rpffcstgroup");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of RpfFcstGroupRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        RpfFcstGroupRecord record = null;

        // create a List to hold RpfFcstGroup Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM rpffcstgroup " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RpfFcstGroupRecord
            // and store its address in oneRecord
            record = new RpfFcstGroupRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RpfFcstGroupRecord object

            record.setGroup_id(getString(rs, 1));
            record.setGroup_name(getString(rs, 2));
            record.setOrdinal(getInt(rs, 3));
            record.setRec_all_included(getString(rs, 4));
            
            // add this RpfFcstGroupRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the RpfFcstGroupRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of RpfFcstGroupRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        RpfFcstGroupRecord record = null;

        // create a List to hold RpfFcstGroup Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM rpffcstgroup " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RpfFcstGroupRecord
            // and store its address in oneRecord
            record = new RpfFcstGroupRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RpfFcstGroupRecord object

            record.setGroup_id(getString(rs, 1));
            record.setGroup_name(getString(rs, 2));
            record.setOrdinal(getInt(rs, 3));
            record.setRec_all_included(getString(rs, 4));
            
            // add this RpfFcstGroupRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the RpfFcstGroupRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a RpfFcstGroupRecord object and..
//-----------------------------------------------------------------
    public int insert(RpfFcstGroupRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO rpffcstgroup VALUES (?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getGroup_id());
        setString(insertStatement, 2, record.getGroup_name());
        setInt(insertStatement, 3, record.getOrdinal());
        setString(insertStatement, 4, record.getRec_all_included());
        
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
        String deleteStatement = "DELETE FROM rpffcstgroup " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RpfFcstGroupRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RpfFcstGroupRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE rpffcstgroup SET group_id = ?, group_name = ?, ordinal = ?, rec_all_included = ?        " + where );

        setString(updateStatement, 1, record.getGroup_id());
        setString(updateStatement, 2, record.getGroup_name());
        setInt(updateStatement, 3, record.getOrdinal());
        setString(updateStatement, 4, record.getRec_all_included());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(RpfFcstGroupRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM rpffcstgroup " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RpfFcstGroupRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RpfFcstGroupRecord oldRecord, RpfFcstGroupRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE rpffcstgroup SET group_id = ?, group_name = ?, ordinal = ?, rec_all_included = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getGroup_id());
        setString(updateStatement, 2, newRecord.getGroup_name());
        setInt(updateStatement, 3, newRecord.getOrdinal());
        setString(updateStatement, 4, newRecord.getRec_all_included());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a RpfFcstGroupRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(RpfFcstGroupRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            RpfFcstGroupRecord oldRecord = (RpfFcstGroupRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of RpfFcstGroupTable class
