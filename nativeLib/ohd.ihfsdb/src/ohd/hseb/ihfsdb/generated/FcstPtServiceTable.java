// filename: FcstPtServiceTable.java
// author  : DBGEN
// created : Tue May 31 17:52:22 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              fcstptservice table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class FcstPtServiceTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  FcstPtServiceTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public FcstPtServiceTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("fcstptservice");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of FcstPtServiceRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        FcstPtServiceRecord record = null;

        // create a List to hold FcstPtService Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM fcstptservice " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a FcstPtServiceRecord
            // and store its address in oneRecord
            record = new FcstPtServiceRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a FcstPtServiceRecord object

            record.setLid(getString(rs, 1));
            record.setFlood_thres(getDouble(rs, 2));
            record.setExceed_prob(getShort(rs, 3));
            record.setService_type(getString(rs, 4));
            record.setAnal_start_date(getDate(rs, 5));
            record.setAnal_end_date(getDate(rs, 6));
            record.setImpl_date(getDate(rs, 7));
            record.setWeb_date(getDate(rs, 8));
            record.setVerif_resp_type(getString(rs, 9));
            record.setDrainage_area(getDouble(rs, 10));
            
            // add this FcstPtServiceRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the FcstPtServiceRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of FcstPtServiceRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        FcstPtServiceRecord record = null;

        // create a List to hold FcstPtService Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM fcstptservice " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a FcstPtServiceRecord
            // and store its address in oneRecord
            record = new FcstPtServiceRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a FcstPtServiceRecord object

            record.setLid(getString(rs, 1));
            record.setFlood_thres(getDouble(rs, 2));
            record.setExceed_prob(getShort(rs, 3));
            record.setService_type(getString(rs, 4));
            record.setAnal_start_date(getDate(rs, 5));
            record.setAnal_end_date(getDate(rs, 6));
            record.setImpl_date(getDate(rs, 7));
            record.setWeb_date(getDate(rs, 8));
            record.setVerif_resp_type(getString(rs, 9));
            record.setDrainage_area(getDouble(rs, 10));
            
            // add this FcstPtServiceRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the FcstPtServiceRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a FcstPtServiceRecord object and..
//-----------------------------------------------------------------
    public int insert(FcstPtServiceRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO fcstptservice VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setDouble(insertStatement, 2, record.getFlood_thres());
        setShort(insertStatement, 3, record.getExceed_prob());
        setString(insertStatement, 4, record.getService_type());
        setDate(insertStatement, 5, record.getAnal_start_date());
        setDate(insertStatement, 6, record.getAnal_end_date());
        setDate(insertStatement, 7, record.getImpl_date());
        setDate(insertStatement, 8, record.getWeb_date());
        setString(insertStatement, 9, record.getVerif_resp_type());
        setDouble(insertStatement, 10, record.getDrainage_area());
        
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
        String deleteStatement = "DELETE FROM fcstptservice " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a FcstPtServiceRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(FcstPtServiceRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE fcstptservice SET lid = ?, flood_thres = ?, exceed_prob = ?, service_type = ?, anal_start_date = ?, anal_end_date = ?, impl_date = ?, web_date = ?, verif_resp_type = ?, drainage_area = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setDouble(updateStatement, 2, record.getFlood_thres());
        setShort(updateStatement, 3, record.getExceed_prob());
        setString(updateStatement, 4, record.getService_type());
        setDate(updateStatement, 5, record.getAnal_start_date());
        setDate(updateStatement, 6, record.getAnal_end_date());
        setDate(updateStatement, 7, record.getImpl_date());
        setDate(updateStatement, 8, record.getWeb_date());
        setString(updateStatement, 9, record.getVerif_resp_type());
        setDouble(updateStatement, 10, record.getDrainage_area());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(FcstPtServiceRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM fcstptservice " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a FcstPtServiceRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(FcstPtServiceRecord oldRecord, FcstPtServiceRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE fcstptservice SET lid = ?, flood_thres = ?, exceed_prob = ?, service_type = ?, anal_start_date = ?, anal_end_date = ?, impl_date = ?, web_date = ?, verif_resp_type = ?, drainage_area = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setDouble(updateStatement, 2, newRecord.getFlood_thres());
        setShort(updateStatement, 3, newRecord.getExceed_prob());
        setString(updateStatement, 4, newRecord.getService_type());
        setDate(updateStatement, 5, newRecord.getAnal_start_date());
        setDate(updateStatement, 6, newRecord.getAnal_end_date());
        setDate(updateStatement, 7, newRecord.getImpl_date());
        setDate(updateStatement, 8, newRecord.getWeb_date());
        setString(updateStatement, 9, newRecord.getVerif_resp_type());
        setDouble(updateStatement, 10, newRecord.getDrainage_area());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a FcstPtServiceRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(FcstPtServiceRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            FcstPtServiceRecord oldRecord = (FcstPtServiceRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of FcstPtServiceTable class
