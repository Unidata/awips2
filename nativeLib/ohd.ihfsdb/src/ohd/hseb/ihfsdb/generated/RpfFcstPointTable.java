// filename: RpfFcstPointTable.java
// author  : DBGEN
// created : Tue May 31 17:52:27 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              rpffcstpoint table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class RpfFcstPointTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  RpfFcstPointTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public RpfFcstPointTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("rpffcstpoint");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of RpfFcstPointRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        RpfFcstPointRecord record = null;

        // create a List to hold RpfFcstPoint Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM rpffcstpoint " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RpfFcstPointRecord
            // and store its address in oneRecord
            record = new RpfFcstPointRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RpfFcstPointRecord object

            record.setLid(getString(rs, 1));
            record.setGroup_id(getString(rs, 2));
            record.setOrdinal(getInt(rs, 3));
            record.setChg_threshold(getDouble(rs, 4));
            record.setRec_type(getString(rs, 5));
            record.setPrimary_back(getString(rs, 6));
            record.setSecondary_back(getString(rs, 7));
            record.setBackhrs(getInt(rs, 8));
            record.setForwardhrs(getInt(rs, 9));
            record.setAdjustendhrs(getDouble(rs, 10));
            
            // add this RpfFcstPointRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the RpfFcstPointRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of RpfFcstPointRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        RpfFcstPointRecord record = null;

        // create a List to hold RpfFcstPoint Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM rpffcstpoint " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RpfFcstPointRecord
            // and store its address in oneRecord
            record = new RpfFcstPointRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RpfFcstPointRecord object

            record.setLid(getString(rs, 1));
            record.setGroup_id(getString(rs, 2));
            record.setOrdinal(getInt(rs, 3));
            record.setChg_threshold(getDouble(rs, 4));
            record.setRec_type(getString(rs, 5));
            record.setPrimary_back(getString(rs, 6));
            record.setSecondary_back(getString(rs, 7));
            record.setBackhrs(getInt(rs, 8));
            record.setForwardhrs(getInt(rs, 9));
            record.setAdjustendhrs(getDouble(rs, 10));
            
            // add this RpfFcstPointRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the RpfFcstPointRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a RpfFcstPointRecord object and..
//-----------------------------------------------------------------
    public int insert(RpfFcstPointRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO rpffcstpoint VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getGroup_id());
        setInt(insertStatement, 3, record.getOrdinal());
        setDouble(insertStatement, 4, record.getChg_threshold());
        setString(insertStatement, 5, record.getRec_type());
        setString(insertStatement, 6, record.getPrimary_back());
        setString(insertStatement, 7, record.getSecondary_back());
        setInt(insertStatement, 8, record.getBackhrs());
        setInt(insertStatement, 9, record.getForwardhrs());
        setDouble(insertStatement, 10, record.getAdjustendhrs());
        
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
        String deleteStatement = "DELETE FROM rpffcstpoint " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RpfFcstPointRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RpfFcstPointRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE rpffcstpoint SET lid = ?, group_id = ?, ordinal = ?, chg_threshold = ?, rec_type = ?, primary_back = ?, secondary_back = ?, backhrs = ?, forwardhrs = ?, adjustendhrs = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setString(updateStatement, 2, record.getGroup_id());
        setInt(updateStatement, 3, record.getOrdinal());
        setDouble(updateStatement, 4, record.getChg_threshold());
        setString(updateStatement, 5, record.getRec_type());
        setString(updateStatement, 6, record.getPrimary_back());
        setString(updateStatement, 7, record.getSecondary_back());
        setInt(updateStatement, 8, record.getBackhrs());
        setInt(updateStatement, 9, record.getForwardhrs());
        setDouble(updateStatement, 10, record.getAdjustendhrs());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(RpfFcstPointRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM rpffcstpoint " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RpfFcstPointRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RpfFcstPointRecord oldRecord, RpfFcstPointRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE rpffcstpoint SET lid = ?, group_id = ?, ordinal = ?, chg_threshold = ?, rec_type = ?, primary_back = ?, secondary_back = ?, backhrs = ?, forwardhrs = ?, adjustendhrs = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getGroup_id());
        setInt(updateStatement, 3, newRecord.getOrdinal());
        setDouble(updateStatement, 4, newRecord.getChg_threshold());
        setString(updateStatement, 5, newRecord.getRec_type());
        setString(updateStatement, 6, newRecord.getPrimary_back());
        setString(updateStatement, 7, newRecord.getSecondary_back());
        setInt(updateStatement, 8, newRecord.getBackhrs());
        setInt(updateStatement, 9, newRecord.getForwardhrs());
        setDouble(updateStatement, 10, newRecord.getAdjustendhrs());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a RpfFcstPointRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(RpfFcstPointRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            RpfFcstPointRecord oldRecord = (RpfFcstPointRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of RpfFcstPointTable class
