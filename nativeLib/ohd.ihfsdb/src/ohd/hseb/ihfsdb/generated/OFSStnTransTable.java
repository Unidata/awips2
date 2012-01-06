// filename: OFSStnTransTable.java
// author  : DBGEN
// created : Tue May 31 17:52:25 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              ofsstntrans table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class OFSStnTransTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  OFSStnTransTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public OFSStnTransTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("ofsstntrans");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of OFSStnTransRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        OFSStnTransRecord record = null;

        // create a List to hold OFSStnTrans Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM ofsstntrans " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a OFSStnTransRecord
            // and store its address in oneRecord
            record = new OFSStnTransRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a OFSStnTransRecord object

            record.setLid(getString(rs, 1));
            record.setOfs_data_type(getString(rs, 2));
            record.setShef_source_code(getString(rs, 3));
            record.setOfs_lid(getString(rs, 4));
            
            // add this OFSStnTransRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the OFSStnTransRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of OFSStnTransRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        OFSStnTransRecord record = null;

        // create a List to hold OFSStnTrans Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM ofsstntrans " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a OFSStnTransRecord
            // and store its address in oneRecord
            record = new OFSStnTransRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a OFSStnTransRecord object

            record.setLid(getString(rs, 1));
            record.setOfs_data_type(getString(rs, 2));
            record.setShef_source_code(getString(rs, 3));
            record.setOfs_lid(getString(rs, 4));
            
            // add this OFSStnTransRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the OFSStnTransRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a OFSStnTransRecord object and..
//-----------------------------------------------------------------
    public int insert(OFSStnTransRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO ofsstntrans VALUES (?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getOfs_data_type());
        setString(insertStatement, 3, record.getShef_source_code());
        setString(insertStatement, 4, record.getOfs_lid());
        
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
        String deleteStatement = "DELETE FROM ofsstntrans " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a OFSStnTransRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(OFSStnTransRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE ofsstntrans SET lid = ?, ofs_data_type = ?, shef_source_code = ?, ofs_lid = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setString(updateStatement, 2, record.getOfs_data_type());
        setString(updateStatement, 3, record.getShef_source_code());
        setString(updateStatement, 4, record.getOfs_lid());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(OFSStnTransRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM ofsstntrans " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a OFSStnTransRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(OFSStnTransRecord oldRecord, OFSStnTransRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE ofsstntrans SET lid = ?, ofs_data_type = ?, shef_source_code = ?, ofs_lid = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getOfs_data_type());
        setString(updateStatement, 3, newRecord.getShef_source_code());
        setString(updateStatement, 4, newRecord.getOfs_lid());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a OFSStnTransRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(OFSStnTransRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            OFSStnTransRecord oldRecord = (OFSStnTransRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of OFSStnTransTable class
