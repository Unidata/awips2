// filename: LocDataLimitsTable.java
// author  : DBGEN
// created : Tue May 31 17:52:24 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              locdatalimits table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class LocDataLimitsTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  LocDataLimitsTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public LocDataLimitsTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("locdatalimits");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of LocDataLimitsRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        LocDataLimitsRecord record = null;

        // create a List to hold LocDataLimits Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM locdatalimits " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a LocDataLimitsRecord
            // and store its address in oneRecord
            record = new LocDataLimitsRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a LocDataLimitsRecord object

            record.setLid(getString(rs, 1));
            record.setPe(getString(rs, 2));
            record.setDur(getShort(rs, 3));
            record.setMonthdaystart(getString(rs, 4));
            record.setMonthdayend(getString(rs, 5));
            record.setGross_range_min(getDouble(rs, 6));
            record.setGross_range_max(getDouble(rs, 7));
            record.setReason_range_min(getDouble(rs, 8));
            record.setReason_range_max(getDouble(rs, 9));
            record.setRoc_max(getDouble(rs, 10));
            record.setAlert_upper_limit(getDouble(rs, 11));
            record.setAlert_roc_limit(getDouble(rs, 12));
            record.setAlarm_upper_limit(getDouble(rs, 13));
            record.setAlarm_roc_limit(getDouble(rs, 14));
            record.setAlert_lower_limit(getDouble(rs, 15));
            record.setAlarm_lower_limit(getDouble(rs, 16));
            record.setAlert_diff_limit(getDouble(rs, 17));
            record.setAlarm_diff_limit(getDouble(rs, 18));
            
            // add this LocDataLimitsRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the LocDataLimitsRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of LocDataLimitsRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        LocDataLimitsRecord record = null;

        // create a List to hold LocDataLimits Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM locdatalimits " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a LocDataLimitsRecord
            // and store its address in oneRecord
            record = new LocDataLimitsRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a LocDataLimitsRecord object

            record.setLid(getString(rs, 1));
            record.setPe(getString(rs, 2));
            record.setDur(getShort(rs, 3));
            record.setMonthdaystart(getString(rs, 4));
            record.setMonthdayend(getString(rs, 5));
            record.setGross_range_min(getDouble(rs, 6));
            record.setGross_range_max(getDouble(rs, 7));
            record.setReason_range_min(getDouble(rs, 8));
            record.setReason_range_max(getDouble(rs, 9));
            record.setRoc_max(getDouble(rs, 10));
            record.setAlert_upper_limit(getDouble(rs, 11));
            record.setAlert_roc_limit(getDouble(rs, 12));
            record.setAlarm_upper_limit(getDouble(rs, 13));
            record.setAlarm_roc_limit(getDouble(rs, 14));
            record.setAlert_lower_limit(getDouble(rs, 15));
            record.setAlarm_lower_limit(getDouble(rs, 16));
            record.setAlert_diff_limit(getDouble(rs, 17));
            record.setAlarm_diff_limit(getDouble(rs, 18));
            
            // add this LocDataLimitsRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the LocDataLimitsRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a LocDataLimitsRecord object and..
//-----------------------------------------------------------------
    public int insert(LocDataLimitsRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO locdatalimits VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getPe());
        setShort(insertStatement, 3, record.getDur());
        setString(insertStatement, 4, record.getMonthdaystart());
        setString(insertStatement, 5, record.getMonthdayend());
        setDouble(insertStatement, 6, record.getGross_range_min());
        setDouble(insertStatement, 7, record.getGross_range_max());
        setDouble(insertStatement, 8, record.getReason_range_min());
        setDouble(insertStatement, 9, record.getReason_range_max());
        setDouble(insertStatement, 10, record.getRoc_max());
        setDouble(insertStatement, 11, record.getAlert_upper_limit());
        setDouble(insertStatement, 12, record.getAlert_roc_limit());
        setDouble(insertStatement, 13, record.getAlarm_upper_limit());
        setDouble(insertStatement, 14, record.getAlarm_roc_limit());
        setDouble(insertStatement, 15, record.getAlert_lower_limit());
        setDouble(insertStatement, 16, record.getAlarm_lower_limit());
        setDouble(insertStatement, 17, record.getAlert_diff_limit());
        setDouble(insertStatement, 18, record.getAlarm_diff_limit());
        
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
        String deleteStatement = "DELETE FROM locdatalimits " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a LocDataLimitsRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(LocDataLimitsRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE locdatalimits SET lid = ?, pe = ?, dur = ?, monthdaystart = ?, monthdayend = ?, gross_range_min = ?, gross_range_max = ?, reason_range_min = ?, reason_range_max = ?, roc_max = ?, alert_upper_limit = ?, alert_roc_limit = ?, alarm_upper_limit = ?, alarm_roc_limit = ?, alert_lower_limit = ?, alarm_lower_limit = ?, alert_diff_limit = ?, alarm_diff_limit = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setString(updateStatement, 2, record.getPe());
        setShort(updateStatement, 3, record.getDur());
        setString(updateStatement, 4, record.getMonthdaystart());
        setString(updateStatement, 5, record.getMonthdayend());
        setDouble(updateStatement, 6, record.getGross_range_min());
        setDouble(updateStatement, 7, record.getGross_range_max());
        setDouble(updateStatement, 8, record.getReason_range_min());
        setDouble(updateStatement, 9, record.getReason_range_max());
        setDouble(updateStatement, 10, record.getRoc_max());
        setDouble(updateStatement, 11, record.getAlert_upper_limit());
        setDouble(updateStatement, 12, record.getAlert_roc_limit());
        setDouble(updateStatement, 13, record.getAlarm_upper_limit());
        setDouble(updateStatement, 14, record.getAlarm_roc_limit());
        setDouble(updateStatement, 15, record.getAlert_lower_limit());
        setDouble(updateStatement, 16, record.getAlarm_lower_limit());
        setDouble(updateStatement, 17, record.getAlert_diff_limit());
        setDouble(updateStatement, 18, record.getAlarm_diff_limit());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(LocDataLimitsRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM locdatalimits " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a LocDataLimitsRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(LocDataLimitsRecord oldRecord, LocDataLimitsRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE locdatalimits SET lid = ?, pe = ?, dur = ?, monthdaystart = ?, monthdayend = ?, gross_range_min = ?, gross_range_max = ?, reason_range_min = ?, reason_range_max = ?, roc_max = ?, alert_upper_limit = ?, alert_roc_limit = ?, alarm_upper_limit = ?, alarm_roc_limit = ?, alert_lower_limit = ?, alarm_lower_limit = ?, alert_diff_limit = ?, alarm_diff_limit = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getPe());
        setShort(updateStatement, 3, newRecord.getDur());
        setString(updateStatement, 4, newRecord.getMonthdaystart());
        setString(updateStatement, 5, newRecord.getMonthdayend());
        setDouble(updateStatement, 6, newRecord.getGross_range_min());
        setDouble(updateStatement, 7, newRecord.getGross_range_max());
        setDouble(updateStatement, 8, newRecord.getReason_range_min());
        setDouble(updateStatement, 9, newRecord.getReason_range_max());
        setDouble(updateStatement, 10, newRecord.getRoc_max());
        setDouble(updateStatement, 11, newRecord.getAlert_upper_limit());
        setDouble(updateStatement, 12, newRecord.getAlert_roc_limit());
        setDouble(updateStatement, 13, newRecord.getAlarm_upper_limit());
        setDouble(updateStatement, 14, newRecord.getAlarm_roc_limit());
        setDouble(updateStatement, 15, newRecord.getAlert_lower_limit());
        setDouble(updateStatement, 16, newRecord.getAlarm_lower_limit());
        setDouble(updateStatement, 17, newRecord.getAlert_diff_limit());
        setDouble(updateStatement, 18, newRecord.getAlarm_diff_limit());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a LocDataLimitsRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(LocDataLimitsRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            LocDataLimitsRecord oldRecord = (LocDataLimitsRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of LocDataLimitsTable class
