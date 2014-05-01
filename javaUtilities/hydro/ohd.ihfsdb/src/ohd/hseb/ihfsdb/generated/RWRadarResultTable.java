// filename: RWRadarResultTable.java
// author  : DBGEN
// created : Tue May 31 17:52:28 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              rwradarresult table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class RWRadarResultTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  RWRadarResultTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public RWRadarResultTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("rwradarresult");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of RWRadarResultRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        RWRadarResultRecord record = null;

        // create a List to hold RWRadarResult Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM rwradarresult " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RWRadarResultRecord
            // and store its address in oneRecord
            record = new RWRadarResultRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RWRadarResultRecord object

            record.setRadid(getString(rs, 1));
            record.setObstime(getTimeStamp(rs, 2));
            record.setNum_gages(getShort(rs, 3));
            record.setRad_avail(getString(rs, 4));
            record.setRw_bias_val_used(getDouble(rs, 5));
            record.setMem_span_used(getDouble(rs, 6));
            record.setEdit_bias(getString(rs, 7));
            record.setIgnore_radar(getString(rs, 8));
            
            // add this RWRadarResultRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the RWRadarResultRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of RWRadarResultRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        RWRadarResultRecord record = null;

        // create a List to hold RWRadarResult Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM rwradarresult " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RWRadarResultRecord
            // and store its address in oneRecord
            record = new RWRadarResultRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RWRadarResultRecord object

            record.setRadid(getString(rs, 1));
            record.setObstime(getTimeStamp(rs, 2));
            record.setNum_gages(getShort(rs, 3));
            record.setRad_avail(getString(rs, 4));
            record.setRw_bias_val_used(getDouble(rs, 5));
            record.setMem_span_used(getDouble(rs, 6));
            record.setEdit_bias(getString(rs, 7));
            record.setIgnore_radar(getString(rs, 8));
            
            // add this RWRadarResultRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the RWRadarResultRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a RWRadarResultRecord object and..
//-----------------------------------------------------------------
    public int insert(RWRadarResultRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO rwradarresult VALUES (?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getRadid());
        setTimeStamp(insertStatement, 2, record.getObstime());
        setShort(insertStatement, 3, record.getNum_gages());
        setString(insertStatement, 4, record.getRad_avail());
        setDouble(insertStatement, 5, record.getRw_bias_val_used());
        setDouble(insertStatement, 6, record.getMem_span_used());
        setString(insertStatement, 7, record.getEdit_bias());
        setString(insertStatement, 8, record.getIgnore_radar());
        
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
        String deleteStatement = "DELETE FROM rwradarresult " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RWRadarResultRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RWRadarResultRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE rwradarresult SET radid = ?, obstime = ?, num_gages = ?, rad_avail = ?, rw_bias_val_used = ?, mem_span_used = ?, edit_bias = ?, ignore_radar = ?        " + where );

        setString(updateStatement, 1, record.getRadid());
        setTimeStamp(updateStatement, 2, record.getObstime());
        setShort(updateStatement, 3, record.getNum_gages());
        setString(updateStatement, 4, record.getRad_avail());
        setDouble(updateStatement, 5, record.getRw_bias_val_used());
        setDouble(updateStatement, 6, record.getMem_span_used());
        setString(updateStatement, 7, record.getEdit_bias());
        setString(updateStatement, 8, record.getIgnore_radar());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(RWRadarResultRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM rwradarresult " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RWRadarResultRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RWRadarResultRecord oldRecord, RWRadarResultRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE rwradarresult SET radid = ?, obstime = ?, num_gages = ?, rad_avail = ?, rw_bias_val_used = ?, mem_span_used = ?, edit_bias = ?, ignore_radar = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getRadid());
        setTimeStamp(updateStatement, 2, newRecord.getObstime());
        setShort(updateStatement, 3, newRecord.getNum_gages());
        setString(updateStatement, 4, newRecord.getRad_avail());
        setDouble(updateStatement, 5, newRecord.getRw_bias_val_used());
        setDouble(updateStatement, 6, newRecord.getMem_span_used());
        setString(updateStatement, 7, newRecord.getEdit_bias());
        setString(updateStatement, 8, newRecord.getIgnore_radar());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a RWRadarResultRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(RWRadarResultRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            RWRadarResultRecord oldRecord = (RWRadarResultRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of RWRadarResultTable class
