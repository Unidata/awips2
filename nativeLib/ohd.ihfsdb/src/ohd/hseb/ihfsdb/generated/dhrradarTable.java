// filename: dhrradarTable.java
// author  : DBGEN
// created : Tue May 31 17:52:21 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              dhrradar table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class dhrradarTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  dhrradarTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public dhrradarTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("dhrradar");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of dhrradarRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        dhrradarRecord record = null;

        // create a List to hold dhrradar Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM dhrradar " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a dhrradarRecord
            // and store its address in oneRecord
            record = new dhrradarRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a dhrradarRecord object

            record.setRadid(getString(rs, 1));
            record.setObstime(getTimeStamp(rs, 2));
            record.setVolcovpat(getShort(rs, 3));
            record.setOpermode(getShort(rs, 4));
            record.setDbzmin(getReal(rs, 5));
            record.setDbzinc(getReal(rs, 6));
            record.setDbzcnt(getReal(rs, 7));
            record.setJ_date(getShort(rs, 8));
            record.setJ_time(getShort(rs, 9));
            record.setMean_field_bias(getShort(rs, 10));
            record.setSample_size(getShort(rs, 11));
            record.setGrid_filename(getString(rs, 12));
            
            // add this dhrradarRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the dhrradarRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of dhrradarRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        dhrradarRecord record = null;

        // create a List to hold dhrradar Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM dhrradar " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a dhrradarRecord
            // and store its address in oneRecord
            record = new dhrradarRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a dhrradarRecord object

            record.setRadid(getString(rs, 1));
            record.setObstime(getTimeStamp(rs, 2));
            record.setVolcovpat(getShort(rs, 3));
            record.setOpermode(getShort(rs, 4));
            record.setDbzmin(getReal(rs, 5));
            record.setDbzinc(getReal(rs, 6));
            record.setDbzcnt(getReal(rs, 7));
            record.setJ_date(getShort(rs, 8));
            record.setJ_time(getShort(rs, 9));
            record.setMean_field_bias(getShort(rs, 10));
            record.setSample_size(getShort(rs, 11));
            record.setGrid_filename(getString(rs, 12));
            
            // add this dhrradarRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the dhrradarRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a dhrradarRecord object and..
//-----------------------------------------------------------------
    public int insert(dhrradarRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO dhrradar VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getRadid());
        setTimeStamp(insertStatement, 2, record.getObstime());
        setShort(insertStatement, 3, record.getVolcovpat());
        setShort(insertStatement, 4, record.getOpermode());
        setReal(insertStatement, 5, record.getDbzmin());
        setReal(insertStatement, 6, record.getDbzinc());
        setReal(insertStatement, 7, record.getDbzcnt());
        setShort(insertStatement, 8, record.getJ_date());
        setShort(insertStatement, 9, record.getJ_time());
        setShort(insertStatement, 10, record.getMean_field_bias());
        setShort(insertStatement, 11, record.getSample_size());
        setString(insertStatement, 12, record.getGrid_filename());
        
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
        String deleteStatement = "DELETE FROM dhrradar " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a dhrradarRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(dhrradarRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE dhrradar SET radid = ?, obstime = ?, volcovpat = ?, opermode = ?, dbzmin = ?, dbzinc = ?, dbzcnt = ?, j_date = ?, j_time = ?, mean_field_bias = ?, sample_size = ?, grid_filename = ?        " + where );

        setString(updateStatement, 1, record.getRadid());
        setTimeStamp(updateStatement, 2, record.getObstime());
        setShort(updateStatement, 3, record.getVolcovpat());
        setShort(updateStatement, 4, record.getOpermode());
        setReal(updateStatement, 5, record.getDbzmin());
        setReal(updateStatement, 6, record.getDbzinc());
        setReal(updateStatement, 7, record.getDbzcnt());
        setShort(updateStatement, 8, record.getJ_date());
        setShort(updateStatement, 9, record.getJ_time());
        setShort(updateStatement, 10, record.getMean_field_bias());
        setShort(updateStatement, 11, record.getSample_size());
        setString(updateStatement, 12, record.getGrid_filename());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(dhrradarRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM dhrradar " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a dhrradarRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(dhrradarRecord oldRecord, dhrradarRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE dhrradar SET radid = ?, obstime = ?, volcovpat = ?, opermode = ?, dbzmin = ?, dbzinc = ?, dbzcnt = ?, j_date = ?, j_time = ?, mean_field_bias = ?, sample_size = ?, grid_filename = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getRadid());
        setTimeStamp(updateStatement, 2, newRecord.getObstime());
        setShort(updateStatement, 3, newRecord.getVolcovpat());
        setShort(updateStatement, 4, newRecord.getOpermode());
        setReal(updateStatement, 5, newRecord.getDbzmin());
        setReal(updateStatement, 6, newRecord.getDbzinc());
        setReal(updateStatement, 7, newRecord.getDbzcnt());
        setShort(updateStatement, 8, newRecord.getJ_date());
        setShort(updateStatement, 9, newRecord.getJ_time());
        setShort(updateStatement, 10, newRecord.getMean_field_bias());
        setShort(updateStatement, 11, newRecord.getSample_size());
        setString(updateStatement, 12, newRecord.getGrid_filename());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a dhrradarRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(dhrradarRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            dhrradarRecord oldRecord = (dhrradarRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of dhrradarTable class
