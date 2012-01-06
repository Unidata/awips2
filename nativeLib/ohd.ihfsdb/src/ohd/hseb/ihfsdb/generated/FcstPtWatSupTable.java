// filename: FcstPtWatSupTable.java
// author  : DBGEN
// created : Tue May 31 17:52:22 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              fcstptwatsup table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class FcstPtWatSupTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  FcstPtWatSupTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public FcstPtWatSupTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("fcstptwatsup");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of FcstPtWatSupRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        FcstPtWatSupRecord record = null;

        // create a List to hold FcstPtWatSup Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM fcstptwatsup " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a FcstPtWatSupRecord
            // and store its address in oneRecord
            record = new FcstPtWatSupRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a FcstPtWatSupRecord object

            record.setLid(getString(rs, 1));
            record.setWatsup_method(getString(rs, 2));
            record.setWatsup_coord_agency(getString(rs, 3));
            record.setFrequpd_normal(getString(rs, 4));
            record.setPeriod_req(getString(rs, 5));
            record.setWatsup_crit(getString(rs, 6));
            record.setWatsup_resp_agency(getString(rs, 7));
            record.setCustomer_desc(getString(rs, 8));
            record.setImpl_date(getDate(rs, 9));
            record.setWeb_date(getDate(rs, 10));
            
            // add this FcstPtWatSupRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the FcstPtWatSupRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of FcstPtWatSupRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        FcstPtWatSupRecord record = null;

        // create a List to hold FcstPtWatSup Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM fcstptwatsup " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a FcstPtWatSupRecord
            // and store its address in oneRecord
            record = new FcstPtWatSupRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a FcstPtWatSupRecord object

            record.setLid(getString(rs, 1));
            record.setWatsup_method(getString(rs, 2));
            record.setWatsup_coord_agency(getString(rs, 3));
            record.setFrequpd_normal(getString(rs, 4));
            record.setPeriod_req(getString(rs, 5));
            record.setWatsup_crit(getString(rs, 6));
            record.setWatsup_resp_agency(getString(rs, 7));
            record.setCustomer_desc(getString(rs, 8));
            record.setImpl_date(getDate(rs, 9));
            record.setWeb_date(getDate(rs, 10));
            
            // add this FcstPtWatSupRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the FcstPtWatSupRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a FcstPtWatSupRecord object and..
//-----------------------------------------------------------------
    public int insert(FcstPtWatSupRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO fcstptwatsup VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getWatsup_method());
        setString(insertStatement, 3, record.getWatsup_coord_agency());
        setString(insertStatement, 4, record.getFrequpd_normal());
        setString(insertStatement, 5, record.getPeriod_req());
        setString(insertStatement, 6, record.getWatsup_crit());
        setString(insertStatement, 7, record.getWatsup_resp_agency());
        setString(insertStatement, 8, record.getCustomer_desc());
        setDate(insertStatement, 9, record.getImpl_date());
        setDate(insertStatement, 10, record.getWeb_date());
        
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
        String deleteStatement = "DELETE FROM fcstptwatsup " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a FcstPtWatSupRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(FcstPtWatSupRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE fcstptwatsup SET lid = ?, watsup_method = ?, watsup_coord_agency = ?, frequpd_normal = ?, period_req = ?, watsup_crit = ?, watsup_resp_agency = ?, customer_desc = ?, impl_date = ?, web_date = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setString(updateStatement, 2, record.getWatsup_method());
        setString(updateStatement, 3, record.getWatsup_coord_agency());
        setString(updateStatement, 4, record.getFrequpd_normal());
        setString(updateStatement, 5, record.getPeriod_req());
        setString(updateStatement, 6, record.getWatsup_crit());
        setString(updateStatement, 7, record.getWatsup_resp_agency());
        setString(updateStatement, 8, record.getCustomer_desc());
        setDate(updateStatement, 9, record.getImpl_date());
        setDate(updateStatement, 10, record.getWeb_date());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(FcstPtWatSupRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM fcstptwatsup " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a FcstPtWatSupRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(FcstPtWatSupRecord oldRecord, FcstPtWatSupRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE fcstptwatsup SET lid = ?, watsup_method = ?, watsup_coord_agency = ?, frequpd_normal = ?, period_req = ?, watsup_crit = ?, watsup_resp_agency = ?, customer_desc = ?, impl_date = ?, web_date = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getWatsup_method());
        setString(updateStatement, 3, newRecord.getWatsup_coord_agency());
        setString(updateStatement, 4, newRecord.getFrequpd_normal());
        setString(updateStatement, 5, newRecord.getPeriod_req());
        setString(updateStatement, 6, newRecord.getWatsup_crit());
        setString(updateStatement, 7, newRecord.getWatsup_resp_agency());
        setString(updateStatement, 8, newRecord.getCustomer_desc());
        setDate(updateStatement, 9, newRecord.getImpl_date());
        setDate(updateStatement, 10, newRecord.getWeb_date());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a FcstPtWatSupRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(FcstPtWatSupRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            FcstPtWatSupRecord oldRecord = (FcstPtWatSupRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of FcstPtWatSupTable class
