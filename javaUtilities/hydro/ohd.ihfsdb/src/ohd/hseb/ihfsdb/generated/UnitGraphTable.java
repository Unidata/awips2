// filename: UnitGraphTable.java
// author  : DBGEN
// created : Tue May 31 17:52:29 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              unitgraph table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class UnitGraphTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  UnitGraphTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public UnitGraphTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("unitgraph");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of UnitGraphRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        UnitGraphRecord record = null;

        // create a List to hold UnitGraph Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM unitgraph " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a UnitGraphRecord
            // and store its address in oneRecord
            record = new UnitGraphRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a UnitGraphRecord object

            record.setLid(getString(rs, 1));
            record.setArea_id(getString(rs, 2));
            record.setModel(getString(rs, 3));
            record.setDur(getInt(rs, 4));
            record.setOrdinal(getInt(rs, 5));
            record.setDischarge(getDouble(rs, 6));
            
            // add this UnitGraphRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the UnitGraphRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of UnitGraphRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        UnitGraphRecord record = null;

        // create a List to hold UnitGraph Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM unitgraph " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a UnitGraphRecord
            // and store its address in oneRecord
            record = new UnitGraphRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a UnitGraphRecord object

            record.setLid(getString(rs, 1));
            record.setArea_id(getString(rs, 2));
            record.setModel(getString(rs, 3));
            record.setDur(getInt(rs, 4));
            record.setOrdinal(getInt(rs, 5));
            record.setDischarge(getDouble(rs, 6));
            
            // add this UnitGraphRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the UnitGraphRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a UnitGraphRecord object and..
//-----------------------------------------------------------------
    public int insert(UnitGraphRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO unitgraph VALUES (?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getArea_id());
        setString(insertStatement, 3, record.getModel());
        setInt(insertStatement, 4, record.getDur());
        setInt(insertStatement, 5, record.getOrdinal());
        setDouble(insertStatement, 6, record.getDischarge());
        
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
        String deleteStatement = "DELETE FROM unitgraph " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a UnitGraphRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(UnitGraphRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE unitgraph SET lid = ?, area_id = ?, model = ?, dur = ?, ordinal = ?, discharge = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setString(updateStatement, 2, record.getArea_id());
        setString(updateStatement, 3, record.getModel());
        setInt(updateStatement, 4, record.getDur());
        setInt(updateStatement, 5, record.getOrdinal());
        setDouble(updateStatement, 6, record.getDischarge());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(UnitGraphRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM unitgraph " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a UnitGraphRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(UnitGraphRecord oldRecord, UnitGraphRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE unitgraph SET lid = ?, area_id = ?, model = ?, dur = ?, ordinal = ?, discharge = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getArea_id());
        setString(updateStatement, 3, newRecord.getModel());
        setInt(updateStatement, 4, newRecord.getDur());
        setInt(updateStatement, 5, newRecord.getOrdinal());
        setDouble(updateStatement, 6, newRecord.getDischarge());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a UnitGraphRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(UnitGraphRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            UnitGraphRecord oldRecord = (UnitGraphRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of UnitGraphTable class
