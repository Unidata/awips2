// filename: OFSDataTransTable.java
// author  : DBGEN
// created : Tue May 31 17:52:25 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              ofsdatatrans table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class OFSDataTransTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  OFSDataTransTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public OFSDataTransTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("ofsdatatrans");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of OFSDataTransRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        OFSDataTransRecord record = null;

        // create a List to hold OFSDataTrans Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM ofsdatatrans " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a OFSDataTransRecord
            // and store its address in oneRecord
            record = new OFSDataTransRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a OFSDataTransRecord object

            record.setPe(getString(rs, 1));
            record.setDur(getShort(rs, 2));
            record.setExtremum(getString(rs, 3));
            record.setOfs_data_type(getString(rs, 4));
            record.setFwd_time_window(getReal(rs, 5));
            record.setBkw_time_window(getReal(rs, 6));
            
            // add this OFSDataTransRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the OFSDataTransRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of OFSDataTransRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        OFSDataTransRecord record = null;

        // create a List to hold OFSDataTrans Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM ofsdatatrans " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a OFSDataTransRecord
            // and store its address in oneRecord
            record = new OFSDataTransRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a OFSDataTransRecord object

            record.setPe(getString(rs, 1));
            record.setDur(getShort(rs, 2));
            record.setExtremum(getString(rs, 3));
            record.setOfs_data_type(getString(rs, 4));
            record.setFwd_time_window(getReal(rs, 5));
            record.setBkw_time_window(getReal(rs, 6));
            
            // add this OFSDataTransRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the OFSDataTransRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a OFSDataTransRecord object and..
//-----------------------------------------------------------------
    public int insert(OFSDataTransRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO ofsdatatrans VALUES (?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getPe());
        setShort(insertStatement, 2, record.getDur());
        setString(insertStatement, 3, record.getExtremum());
        setString(insertStatement, 4, record.getOfs_data_type());
        setReal(insertStatement, 5, record.getFwd_time_window());
        setReal(insertStatement, 6, record.getBkw_time_window());
        
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
        String deleteStatement = "DELETE FROM ofsdatatrans " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a OFSDataTransRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(OFSDataTransRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE ofsdatatrans SET pe = ?, dur = ?, extremum = ?, ofs_data_type = ?, fwd_time_window = ?, bkw_time_window = ?        " + where );

        setString(updateStatement, 1, record.getPe());
        setShort(updateStatement, 2, record.getDur());
        setString(updateStatement, 3, record.getExtremum());
        setString(updateStatement, 4, record.getOfs_data_type());
        setReal(updateStatement, 5, record.getFwd_time_window());
        setReal(updateStatement, 6, record.getBkw_time_window());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(OFSDataTransRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM ofsdatatrans " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a OFSDataTransRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(OFSDataTransRecord oldRecord, OFSDataTransRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE ofsdatatrans SET pe = ?, dur = ?, extremum = ?, ofs_data_type = ?, fwd_time_window = ?, bkw_time_window = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getPe());
        setShort(updateStatement, 2, newRecord.getDur());
        setString(updateStatement, 3, newRecord.getExtremum());
        setString(updateStatement, 4, newRecord.getOfs_data_type());
        setReal(updateStatement, 5, newRecord.getFwd_time_window());
        setReal(updateStatement, 6, newRecord.getBkw_time_window());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a OFSDataTransRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(OFSDataTransRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            OFSDataTransRecord oldRecord = (OFSDataTransRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of OFSDataTransTable class
