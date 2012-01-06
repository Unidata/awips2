// filename: ShefPeTable.java
// author  : DBGEN
// created : Tue May 31 17:52:28 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              shefpe table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class ShefPeTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  ShefPeTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public ShefPeTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("shefpe");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of ShefPeRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        ShefPeRecord record = null;

        // create a List to hold ShefPe Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM shefpe " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a ShefPeRecord
            // and store its address in oneRecord
            record = new ShefPeRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a ShefPeRecord object

            record.setPe(getString(rs, 1));
            record.setName(getString(rs, 2));
            record.setEng_unit(getString(rs, 3));
            record.setMet_unit(getString(rs, 4));
            
            // add this ShefPeRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the ShefPeRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of ShefPeRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        ShefPeRecord record = null;

        // create a List to hold ShefPe Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM shefpe " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a ShefPeRecord
            // and store its address in oneRecord
            record = new ShefPeRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a ShefPeRecord object

            record.setPe(getString(rs, 1));
            record.setName(getString(rs, 2));
            record.setEng_unit(getString(rs, 3));
            record.setMet_unit(getString(rs, 4));
            
            // add this ShefPeRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the ShefPeRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a ShefPeRecord object and..
//-----------------------------------------------------------------
    public int insert(ShefPeRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO shefpe VALUES (?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getPe());
        setString(insertStatement, 2, record.getName());
        setString(insertStatement, 3, record.getEng_unit());
        setString(insertStatement, 4, record.getMet_unit());
        
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
        String deleteStatement = "DELETE FROM shefpe " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a ShefPeRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(ShefPeRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE shefpe SET pe = ?, name = ?, eng_unit = ?, met_unit = ?        " + where );

        setString(updateStatement, 1, record.getPe());
        setString(updateStatement, 2, record.getName());
        setString(updateStatement, 3, record.getEng_unit());
        setString(updateStatement, 4, record.getMet_unit());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(ShefPeRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM shefpe " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a ShefPeRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(ShefPeRecord oldRecord, ShefPeRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE shefpe SET pe = ?, name = ?, eng_unit = ?, met_unit = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getPe());
        setString(updateStatement, 2, newRecord.getName());
        setString(updateStatement, 3, newRecord.getEng_unit());
        setString(updateStatement, 4, newRecord.getMet_unit());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a ShefPeRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(ShefPeRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            ShefPeRecord oldRecord = (ShefPeRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of ShefPeTable class
