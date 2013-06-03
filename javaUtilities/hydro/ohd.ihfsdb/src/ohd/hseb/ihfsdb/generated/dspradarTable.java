// filename: dspradarTable.java
// author  : DBGEN
// created : Tue May 31 17:52:21 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              dspradar table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class dspradarTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  dspradarTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public dspradarTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("dspradar");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of dspradarRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        dspradarRecord record = null;

        // create a List to hold dspradar Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM dspradar " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a dspradarRecord
            // and store its address in oneRecord
            record = new dspradarRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a dspradarRecord object

            record.setRadid(getString(rs, 1));
            record.setObstime(getTimeStamp(rs, 2));
            record.setVolcovpat(getShort(rs, 3));
            record.setOpermode(getShort(rs, 4));
            record.setMinval(getReal(rs, 5));
            record.setMaxval(getReal(rs, 6));
            record.setNum_data_lev(getReal(rs, 7));
            record.setScale_factor(getReal(rs, 8));
            record.setBegin_time(getTimeStamp(rs, 9));
            record.setEnd_time(getTimeStamp(rs, 10));
            record.setJ_beg_date(getShort(rs, 11));
            record.setJ_beg_time(getShort(rs, 12));
            record.setJ_end_date(getShort(rs, 13));
            record.setJ_end_time(getShort(rs, 14));
            record.setMean_field_bias(getShort(rs, 15));
            record.setSample_size(getShort(rs, 16));
            record.setGrid_filename(getString(rs, 17));
            
            // add this dspradarRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the dspradarRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of dspradarRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        dspradarRecord record = null;

        // create a List to hold dspradar Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM dspradar " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a dspradarRecord
            // and store its address in oneRecord
            record = new dspradarRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a dspradarRecord object

            record.setRadid(getString(rs, 1));
            record.setObstime(getTimeStamp(rs, 2));
            record.setVolcovpat(getShort(rs, 3));
            record.setOpermode(getShort(rs, 4));
            record.setMinval(getReal(rs, 5));
            record.setMaxval(getReal(rs, 6));
            record.setNum_data_lev(getReal(rs, 7));
            record.setScale_factor(getReal(rs, 8));
            record.setBegin_time(getTimeStamp(rs, 9));
            record.setEnd_time(getTimeStamp(rs, 10));
            record.setJ_beg_date(getShort(rs, 11));
            record.setJ_beg_time(getShort(rs, 12));
            record.setJ_end_date(getShort(rs, 13));
            record.setJ_end_time(getShort(rs, 14));
            record.setMean_field_bias(getShort(rs, 15));
            record.setSample_size(getShort(rs, 16));
            record.setGrid_filename(getString(rs, 17));
            
            // add this dspradarRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the dspradarRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a dspradarRecord object and..
//-----------------------------------------------------------------
    public int insert(dspradarRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO dspradar VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getRadid());
        setTimeStamp(insertStatement, 2, record.getObstime());
        setShort(insertStatement, 3, record.getVolcovpat());
        setShort(insertStatement, 4, record.getOpermode());
        setReal(insertStatement, 5, record.getMinval());
        setReal(insertStatement, 6, record.getMaxval());
        setReal(insertStatement, 7, record.getNum_data_lev());
        setReal(insertStatement, 8, record.getScale_factor());
        setTimeStamp(insertStatement, 9, record.getBegin_time());
        setTimeStamp(insertStatement, 10, record.getEnd_time());
        setShort(insertStatement, 11, record.getJ_beg_date());
        setShort(insertStatement, 12, record.getJ_beg_time());
        setShort(insertStatement, 13, record.getJ_end_date());
        setShort(insertStatement, 14, record.getJ_end_time());
        setShort(insertStatement, 15, record.getMean_field_bias());
        setShort(insertStatement, 16, record.getSample_size());
        setString(insertStatement, 17, record.getGrid_filename());
        
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
        String deleteStatement = "DELETE FROM dspradar " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a dspradarRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(dspradarRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE dspradar SET radid = ?, obstime = ?, volcovpat = ?, opermode = ?, minval = ?, maxval = ?, num_data_lev = ?, scale_factor = ?, begin_time = ?, end_time = ?, j_beg_date = ?, j_beg_time = ?, j_end_date = ?, j_end_time = ?, mean_field_bias = ?, sample_size = ?, grid_filename = ?        " + where );

        setString(updateStatement, 1, record.getRadid());
        setTimeStamp(updateStatement, 2, record.getObstime());
        setShort(updateStatement, 3, record.getVolcovpat());
        setShort(updateStatement, 4, record.getOpermode());
        setReal(updateStatement, 5, record.getMinval());
        setReal(updateStatement, 6, record.getMaxval());
        setReal(updateStatement, 7, record.getNum_data_lev());
        setReal(updateStatement, 8, record.getScale_factor());
        setTimeStamp(updateStatement, 9, record.getBegin_time());
        setTimeStamp(updateStatement, 10, record.getEnd_time());
        setShort(updateStatement, 11, record.getJ_beg_date());
        setShort(updateStatement, 12, record.getJ_beg_time());
        setShort(updateStatement, 13, record.getJ_end_date());
        setShort(updateStatement, 14, record.getJ_end_time());
        setShort(updateStatement, 15, record.getMean_field_bias());
        setShort(updateStatement, 16, record.getSample_size());
        setString(updateStatement, 17, record.getGrid_filename());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(dspradarRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM dspradar " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a dspradarRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(dspradarRecord oldRecord, dspradarRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE dspradar SET radid = ?, obstime = ?, volcovpat = ?, opermode = ?, minval = ?, maxval = ?, num_data_lev = ?, scale_factor = ?, begin_time = ?, end_time = ?, j_beg_date = ?, j_beg_time = ?, j_end_date = ?, j_end_time = ?, mean_field_bias = ?, sample_size = ?, grid_filename = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getRadid());
        setTimeStamp(updateStatement, 2, newRecord.getObstime());
        setShort(updateStatement, 3, newRecord.getVolcovpat());
        setShort(updateStatement, 4, newRecord.getOpermode());
        setReal(updateStatement, 5, newRecord.getMinval());
        setReal(updateStatement, 6, newRecord.getMaxval());
        setReal(updateStatement, 7, newRecord.getNum_data_lev());
        setReal(updateStatement, 8, newRecord.getScale_factor());
        setTimeStamp(updateStatement, 9, newRecord.getBegin_time());
        setTimeStamp(updateStatement, 10, newRecord.getEnd_time());
        setShort(updateStatement, 11, newRecord.getJ_beg_date());
        setShort(updateStatement, 12, newRecord.getJ_beg_time());
        setShort(updateStatement, 13, newRecord.getJ_end_date());
        setShort(updateStatement, 14, newRecord.getJ_end_time());
        setShort(updateStatement, 15, newRecord.getMean_field_bias());
        setShort(updateStatement, 16, newRecord.getSample_size());
        setString(updateStatement, 17, newRecord.getGrid_filename());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a dspradarRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(dspradarRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            dspradarRecord oldRecord = (dspradarRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of dspradarTable class
