// filename: RejectedDataTable.java
// author  : DBGEN
// created : Tue May 31 17:52:26 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              rejecteddata table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class RejectedDataTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  RejectedDataTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public RejectedDataTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("rejecteddata");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of RejectedDataRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        RejectedDataRecord record = null;

        // create a List to hold RejectedData Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM rejecteddata " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RejectedDataRecord
            // and store its address in oneRecord
            record = new RejectedDataRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RejectedDataRecord object

            record.setLid(getString(rs, 1));
            record.setPe(getString(rs, 2));
            record.setDur(getShort(rs, 3));
            record.setTs(getString(rs, 4));
            record.setExtremum(getString(rs, 5));
            record.setProbability(getReal(rs, 6));
            record.setValidtime(getTimeStamp(rs, 7));
            record.setBasistime(getTimeStamp(rs, 8));
            record.setPostingtime(getTimeStamp(rs, 9));
            record.setValue(getDouble(rs, 10));
            record.setRevision(getShort(rs, 11));
            record.setShef_qual_code(getString(rs, 12));
            record.setProduct_id(getString(rs, 13));
            record.setProducttime(getTimeStamp(rs, 14));
            record.setQuality_code(getInt(rs, 15));
            record.setReject_type(getString(rs, 16));
            record.setUserid(getString(rs, 17));
            
            // add this RejectedDataRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the RejectedDataRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of RejectedDataRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        RejectedDataRecord record = null;

        // create a List to hold RejectedData Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM rejecteddata " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RejectedDataRecord
            // and store its address in oneRecord
            record = new RejectedDataRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RejectedDataRecord object

            record.setLid(getString(rs, 1));
            record.setPe(getString(rs, 2));
            record.setDur(getShort(rs, 3));
            record.setTs(getString(rs, 4));
            record.setExtremum(getString(rs, 5));
            record.setProbability(getReal(rs, 6));
            record.setValidtime(getTimeStamp(rs, 7));
            record.setBasistime(getTimeStamp(rs, 8));
            record.setPostingtime(getTimeStamp(rs, 9));
            record.setValue(getDouble(rs, 10));
            record.setRevision(getShort(rs, 11));
            record.setShef_qual_code(getString(rs, 12));
            record.setProduct_id(getString(rs, 13));
            record.setProducttime(getTimeStamp(rs, 14));
            record.setQuality_code(getInt(rs, 15));
            record.setReject_type(getString(rs, 16));
            record.setUserid(getString(rs, 17));
            
            // add this RejectedDataRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the RejectedDataRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a RejectedDataRecord object and..
//-----------------------------------------------------------------
    public int insert(RejectedDataRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO rejecteddata VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getPe());
        setShort(insertStatement, 3, record.getDur());
        setString(insertStatement, 4, record.getTs());
        setString(insertStatement, 5, record.getExtremum());
        setReal(insertStatement, 6, record.getProbability());
        setTimeStamp(insertStatement, 7, record.getValidtime());
        setTimeStamp(insertStatement, 8, record.getBasistime());
        setTimeStamp(insertStatement, 9, record.getPostingtime());
        setDouble(insertStatement, 10, record.getValue());
        setShort(insertStatement, 11, record.getRevision());
        setString(insertStatement, 12, record.getShef_qual_code());
        setString(insertStatement, 13, record.getProduct_id());
        setTimeStamp(insertStatement, 14, record.getProducttime());
        setInt(insertStatement, 15, record.getQuality_code());
        setString(insertStatement, 16, record.getReject_type());
        setString(insertStatement, 17, record.getUserid());
        
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
        String deleteStatement = "DELETE FROM rejecteddata " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RejectedDataRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RejectedDataRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE rejecteddata SET lid = ?, pe = ?, dur = ?, ts = ?, extremum = ?, probability = ?, validtime = ?, basistime = ?, postingtime = ?, value = ?, revision = ?, shef_qual_code = ?, product_id = ?, producttime = ?, quality_code = ?, reject_type = ?, userid = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setString(updateStatement, 2, record.getPe());
        setShort(updateStatement, 3, record.getDur());
        setString(updateStatement, 4, record.getTs());
        setString(updateStatement, 5, record.getExtremum());
        setReal(updateStatement, 6, record.getProbability());
        setTimeStamp(updateStatement, 7, record.getValidtime());
        setTimeStamp(updateStatement, 8, record.getBasistime());
        setTimeStamp(updateStatement, 9, record.getPostingtime());
        setDouble(updateStatement, 10, record.getValue());
        setShort(updateStatement, 11, record.getRevision());
        setString(updateStatement, 12, record.getShef_qual_code());
        setString(updateStatement, 13, record.getProduct_id());
        setTimeStamp(updateStatement, 14, record.getProducttime());
        setInt(updateStatement, 15, record.getQuality_code());
        setString(updateStatement, 16, record.getReject_type());
        setString(updateStatement, 17, record.getUserid());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(RejectedDataRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM rejecteddata " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RejectedDataRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RejectedDataRecord oldRecord, RejectedDataRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE rejecteddata SET lid = ?, pe = ?, dur = ?, ts = ?, extremum = ?, probability = ?, validtime = ?, basistime = ?, postingtime = ?, value = ?, revision = ?, shef_qual_code = ?, product_id = ?, producttime = ?, quality_code = ?, reject_type = ?, userid = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getPe());
        setShort(updateStatement, 3, newRecord.getDur());
        setString(updateStatement, 4, newRecord.getTs());
        setString(updateStatement, 5, newRecord.getExtremum());
        setReal(updateStatement, 6, newRecord.getProbability());
        setTimeStamp(updateStatement, 7, newRecord.getValidtime());
        setTimeStamp(updateStatement, 8, newRecord.getBasistime());
        setTimeStamp(updateStatement, 9, newRecord.getPostingtime());
        setDouble(updateStatement, 10, newRecord.getValue());
        setShort(updateStatement, 11, newRecord.getRevision());
        setString(updateStatement, 12, newRecord.getShef_qual_code());
        setString(updateStatement, 13, newRecord.getProduct_id());
        setTimeStamp(updateStatement, 14, newRecord.getProducttime());
        setInt(updateStatement, 15, newRecord.getQuality_code());
        setString(updateStatement, 16, newRecord.getReject_type());
        setString(updateStatement, 17, newRecord.getUserid());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a RejectedDataRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(RejectedDataRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            RejectedDataRecord oldRecord = (RejectedDataRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of RejectedDataTable class
