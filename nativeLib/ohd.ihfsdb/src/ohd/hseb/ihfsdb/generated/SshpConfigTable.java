// filename: SshpConfigTable.java
// author  : DBGEN
// created : Tue May 31 17:52:29 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              sshpconfig table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class SshpConfigTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  SshpConfigTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public SshpConfigTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("sshpconfig");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of SshpConfigRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        SshpConfigRecord record = null;

        // create a List to hold SshpConfig Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM sshpconfig " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a SshpConfigRecord
            // and store its address in oneRecord
            record = new SshpConfigRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a SshpConfigRecord object

            record.setLid(getString(rs, 1));
            record.setBasin_id(getString(rs, 2));
            record.setPostingtime(getTimeStamp(rs, 3));
            record.setModel_pref(getString(rs, 4));
            record.setAuto_process(getString(rs, 5));
            record.setSource_pref(getString(rs, 6));
            record.setUse_static_evap(getString(rs, 7));
            record.setUse_blend(getString(rs, 8));
            record.setBlend_method(getString(rs, 9));
            record.setBlend_hours(getInt(rs, 10));
            
            // add this SshpConfigRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the SshpConfigRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of SshpConfigRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        SshpConfigRecord record = null;

        // create a List to hold SshpConfig Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM sshpconfig " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a SshpConfigRecord
            // and store its address in oneRecord
            record = new SshpConfigRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a SshpConfigRecord object

            record.setLid(getString(rs, 1));
            record.setBasin_id(getString(rs, 2));
            record.setPostingtime(getTimeStamp(rs, 3));
            record.setModel_pref(getString(rs, 4));
            record.setAuto_process(getString(rs, 5));
            record.setSource_pref(getString(rs, 6));
            record.setUse_static_evap(getString(rs, 7));
            record.setUse_blend(getString(rs, 8));
            record.setBlend_method(getString(rs, 9));
            record.setBlend_hours(getInt(rs, 10));
            
            // add this SshpConfigRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the SshpConfigRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a SshpConfigRecord object and..
//-----------------------------------------------------------------
    public int insert(SshpConfigRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO sshpconfig VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getBasin_id());
        setTimeStamp(insertStatement, 3, record.getPostingtime());
        setString(insertStatement, 4, record.getModel_pref());
        setString(insertStatement, 5, record.getAuto_process());
        setString(insertStatement, 6, record.getSource_pref());
        setString(insertStatement, 7, record.getUse_static_evap());
        setString(insertStatement, 8, record.getUse_blend());
        setString(insertStatement, 9, record.getBlend_method());
        setInt(insertStatement, 10, record.getBlend_hours());
        
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
        String deleteStatement = "DELETE FROM sshpconfig " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a SshpConfigRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(SshpConfigRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE sshpconfig SET lid = ?, basin_id = ?, postingtime = ?, model_pref = ?, auto_process = ?, source_pref = ?, use_static_evap = ?, use_blend = ?, blend_method = ?, blend_hours = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setString(updateStatement, 2, record.getBasin_id());
        setTimeStamp(updateStatement, 3, record.getPostingtime());
        setString(updateStatement, 4, record.getModel_pref());
        setString(updateStatement, 5, record.getAuto_process());
        setString(updateStatement, 6, record.getSource_pref());
        setString(updateStatement, 7, record.getUse_static_evap());
        setString(updateStatement, 8, record.getUse_blend());
        setString(updateStatement, 9, record.getBlend_method());
        setInt(updateStatement, 10, record.getBlend_hours());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(SshpConfigRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM sshpconfig " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a SshpConfigRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(SshpConfigRecord oldRecord, SshpConfigRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE sshpconfig SET lid = ?, basin_id = ?, postingtime = ?, model_pref = ?, auto_process = ?, source_pref = ?, use_static_evap = ?, use_blend = ?, blend_method = ?, blend_hours = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getBasin_id());
        setTimeStamp(updateStatement, 3, newRecord.getPostingtime());
        setString(updateStatement, 4, newRecord.getModel_pref());
        setString(updateStatement, 5, newRecord.getAuto_process());
        setString(updateStatement, 6, newRecord.getSource_pref());
        setString(updateStatement, 7, newRecord.getUse_static_evap());
        setString(updateStatement, 8, newRecord.getUse_blend());
        setString(updateStatement, 9, newRecord.getBlend_method());
        setInt(updateStatement, 10, newRecord.getBlend_hours());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a SshpConfigRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(SshpConfigRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            SshpConfigRecord oldRecord = (SshpConfigRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of SshpConfigTable class
