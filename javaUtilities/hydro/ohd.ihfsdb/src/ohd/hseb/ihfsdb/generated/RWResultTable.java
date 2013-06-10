// filename: RWResultTable.java
// author  : DBGEN
// created : Tue May 31 17:52:28 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              rwresult table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class RWResultTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  RWResultTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public RWResultTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("rwresult");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of RWResultRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        RWResultRecord record = null;

        // create a List to hold RWResult Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM rwresult " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RWResultRecord
            // and store its address in oneRecord
            record = new RWResultRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RWResultRecord object

            record.setRfc(getString(rs, 1));
            record.setObstime(getTimeStamp(rs, 2));
            record.setNum_gag_avail(getShort(rs, 3));
            record.setNum_rad_avail(getInt(rs, 4));
            record.setNum_pseudo_gages(getInt(rs, 5));
            record.setSat_avail(getString(rs, 6));
            record.setMapx_field_type(getString(rs, 7));
            record.setDraw_precip(getString(rs, 8));
            record.setAuto_save(getString(rs, 9));
            record.setLast_exec_time(getTimeStamp(rs, 10));
            record.setLast_save_time(getTimeStamp(rs, 11));
            
            // add this RWResultRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the RWResultRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of RWResultRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        RWResultRecord record = null;

        // create a List to hold RWResult Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM rwresult " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RWResultRecord
            // and store its address in oneRecord
            record = new RWResultRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RWResultRecord object

            record.setRfc(getString(rs, 1));
            record.setObstime(getTimeStamp(rs, 2));
            record.setNum_gag_avail(getShort(rs, 3));
            record.setNum_rad_avail(getInt(rs, 4));
            record.setNum_pseudo_gages(getInt(rs, 5));
            record.setSat_avail(getString(rs, 6));
            record.setMapx_field_type(getString(rs, 7));
            record.setDraw_precip(getString(rs, 8));
            record.setAuto_save(getString(rs, 9));
            record.setLast_exec_time(getTimeStamp(rs, 10));
            record.setLast_save_time(getTimeStamp(rs, 11));
            
            // add this RWResultRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the RWResultRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a RWResultRecord object and..
//-----------------------------------------------------------------
    public int insert(RWResultRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO rwresult VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getRfc());
        setTimeStamp(insertStatement, 2, record.getObstime());
        setShort(insertStatement, 3, record.getNum_gag_avail());
        setInt(insertStatement, 4, record.getNum_rad_avail());
        setInt(insertStatement, 5, record.getNum_pseudo_gages());
        setString(insertStatement, 6, record.getSat_avail());
        setString(insertStatement, 7, record.getMapx_field_type());
        setString(insertStatement, 8, record.getDraw_precip());
        setString(insertStatement, 9, record.getAuto_save());
        setTimeStamp(insertStatement, 10, record.getLast_exec_time());
        setTimeStamp(insertStatement, 11, record.getLast_save_time());
        
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
        String deleteStatement = "DELETE FROM rwresult " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RWResultRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RWResultRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE rwresult SET rfc = ?, obstime = ?, num_gag_avail = ?, num_rad_avail = ?, num_pseudo_gages = ?, sat_avail = ?, mapx_field_type = ?, draw_precip = ?, auto_save = ?, last_exec_time = ?, last_save_time = ?        " + where );

        setString(updateStatement, 1, record.getRfc());
        setTimeStamp(updateStatement, 2, record.getObstime());
        setShort(updateStatement, 3, record.getNum_gag_avail());
        setInt(updateStatement, 4, record.getNum_rad_avail());
        setInt(updateStatement, 5, record.getNum_pseudo_gages());
        setString(updateStatement, 6, record.getSat_avail());
        setString(updateStatement, 7, record.getMapx_field_type());
        setString(updateStatement, 8, record.getDraw_precip());
        setString(updateStatement, 9, record.getAuto_save());
        setTimeStamp(updateStatement, 10, record.getLast_exec_time());
        setTimeStamp(updateStatement, 11, record.getLast_save_time());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(RWResultRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM rwresult " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RWResultRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RWResultRecord oldRecord, RWResultRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE rwresult SET rfc = ?, obstime = ?, num_gag_avail = ?, num_rad_avail = ?, num_pseudo_gages = ?, sat_avail = ?, mapx_field_type = ?, draw_precip = ?, auto_save = ?, last_exec_time = ?, last_save_time = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getRfc());
        setTimeStamp(updateStatement, 2, newRecord.getObstime());
        setShort(updateStatement, 3, newRecord.getNum_gag_avail());
        setInt(updateStatement, 4, newRecord.getNum_rad_avail());
        setInt(updateStatement, 5, newRecord.getNum_pseudo_gages());
        setString(updateStatement, 6, newRecord.getSat_avail());
        setString(updateStatement, 7, newRecord.getMapx_field_type());
        setString(updateStatement, 8, newRecord.getDraw_precip());
        setString(updateStatement, 9, newRecord.getAuto_save());
        setTimeStamp(updateStatement, 10, newRecord.getLast_exec_time());
        setTimeStamp(updateStatement, 11, newRecord.getLast_save_time());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a RWResultRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(RWResultRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            RWResultRecord oldRecord = (RWResultRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of RWResultTable class
