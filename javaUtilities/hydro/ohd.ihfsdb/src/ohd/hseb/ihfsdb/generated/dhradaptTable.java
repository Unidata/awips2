// filename: dhradaptTable.java
// author  : DBGEN
// created : Tue May 31 17:52:20 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              dhradapt table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class dhradaptTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  dhradaptTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public dhradaptTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("dhradapt");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of dhradaptRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        dhradaptRecord record = null;

        // create a List to hold dhradapt Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM dhradapt " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a dhradaptRecord
            // and store its address in oneRecord
            record = new dhradaptRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a dhradaptRecord object

            record.setRadid(getString(rs, 1));
            record.setObstime(getTimeStamp(rs, 2));
            record.setMin_reflth(getReal(rs, 3));
            record.setMax_reflth(getReal(rs, 4));
            record.setRef_tltest(getReal(rs, 5));
            record.setRng_tltin(getReal(rs, 6));
            record.setRng_tltout(getReal(rs, 7));
            record.setMax_birng(getReal(rs, 8));
            record.setMin_birng(getReal(rs, 9));
            record.setMin_echoar(getReal(rs, 10));
            record.setMin_awrefl(getReal(rs, 11));
            record.setMax_pctred(getReal(rs, 12));
            record.setMlt_zrcoef(getReal(rs, 13));
            record.setPwr_zrcoef(getReal(rs, 14));
            record.setMin_zrefl(getReal(rs, 15));
            record.setMax_zrefl(getReal(rs, 16));
            record.setMax_stmspd(getReal(rs, 17));
            record.setMax_timdif(getReal(rs, 18));
            record.setMin_artcon(getReal(rs, 19));
            record.setTim_p1cont(getReal(rs, 20));
            record.setTim_p2cont(getReal(rs, 21));
            record.setMax_ecarch(getReal(rs, 22));
            record.setRng_cutoff(getReal(rs, 23));
            record.setRng_e1coef(getReal(rs, 24));
            record.setRng_e2coef(getReal(rs, 25));
            record.setRng_e3coef(getReal(rs, 26));
            record.setMin_prate(getReal(rs, 27));
            record.setMax_prate(getReal(rs, 28));
            record.setTim_restrt(getReal(rs, 29));
            record.setMax_timint(getReal(rs, 30));
            record.setMin_timprd(getReal(rs, 31));
            record.setThr_hlyout(getReal(rs, 32));
            record.setEnd_timgag(getReal(rs, 33));
            record.setMax_prdval(getReal(rs, 34));
            record.setMax_hlyval(getReal(rs, 35));
            record.setTim_biest(getReal(rs, 36));
            record.setThr_nosets(getReal(rs, 37));
            record.setRes_bias(getReal(rs, 38));
            record.setLongest_lag(getReal(rs, 39));
            record.setBias_applied(getString(rs, 40));
            
            // add this dhradaptRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the dhradaptRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of dhradaptRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        dhradaptRecord record = null;

        // create a List to hold dhradapt Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM dhradapt " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a dhradaptRecord
            // and store its address in oneRecord
            record = new dhradaptRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a dhradaptRecord object

            record.setRadid(getString(rs, 1));
            record.setObstime(getTimeStamp(rs, 2));
            record.setMin_reflth(getReal(rs, 3));
            record.setMax_reflth(getReal(rs, 4));
            record.setRef_tltest(getReal(rs, 5));
            record.setRng_tltin(getReal(rs, 6));
            record.setRng_tltout(getReal(rs, 7));
            record.setMax_birng(getReal(rs, 8));
            record.setMin_birng(getReal(rs, 9));
            record.setMin_echoar(getReal(rs, 10));
            record.setMin_awrefl(getReal(rs, 11));
            record.setMax_pctred(getReal(rs, 12));
            record.setMlt_zrcoef(getReal(rs, 13));
            record.setPwr_zrcoef(getReal(rs, 14));
            record.setMin_zrefl(getReal(rs, 15));
            record.setMax_zrefl(getReal(rs, 16));
            record.setMax_stmspd(getReal(rs, 17));
            record.setMax_timdif(getReal(rs, 18));
            record.setMin_artcon(getReal(rs, 19));
            record.setTim_p1cont(getReal(rs, 20));
            record.setTim_p2cont(getReal(rs, 21));
            record.setMax_ecarch(getReal(rs, 22));
            record.setRng_cutoff(getReal(rs, 23));
            record.setRng_e1coef(getReal(rs, 24));
            record.setRng_e2coef(getReal(rs, 25));
            record.setRng_e3coef(getReal(rs, 26));
            record.setMin_prate(getReal(rs, 27));
            record.setMax_prate(getReal(rs, 28));
            record.setTim_restrt(getReal(rs, 29));
            record.setMax_timint(getReal(rs, 30));
            record.setMin_timprd(getReal(rs, 31));
            record.setThr_hlyout(getReal(rs, 32));
            record.setEnd_timgag(getReal(rs, 33));
            record.setMax_prdval(getReal(rs, 34));
            record.setMax_hlyval(getReal(rs, 35));
            record.setTim_biest(getReal(rs, 36));
            record.setThr_nosets(getReal(rs, 37));
            record.setRes_bias(getReal(rs, 38));
            record.setLongest_lag(getReal(rs, 39));
            record.setBias_applied(getString(rs, 40));
            
            // add this dhradaptRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the dhradaptRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a dhradaptRecord object and..
//-----------------------------------------------------------------
    public int insert(dhradaptRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO dhradapt VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getRadid());
        setTimeStamp(insertStatement, 2, record.getObstime());
        setReal(insertStatement, 3, record.getMin_reflth());
        setReal(insertStatement, 4, record.getMax_reflth());
        setReal(insertStatement, 5, record.getRef_tltest());
        setReal(insertStatement, 6, record.getRng_tltin());
        setReal(insertStatement, 7, record.getRng_tltout());
        setReal(insertStatement, 8, record.getMax_birng());
        setReal(insertStatement, 9, record.getMin_birng());
        setReal(insertStatement, 10, record.getMin_echoar());
        setReal(insertStatement, 11, record.getMin_awrefl());
        setReal(insertStatement, 12, record.getMax_pctred());
        setReal(insertStatement, 13, record.getMlt_zrcoef());
        setReal(insertStatement, 14, record.getPwr_zrcoef());
        setReal(insertStatement, 15, record.getMin_zrefl());
        setReal(insertStatement, 16, record.getMax_zrefl());
        setReal(insertStatement, 17, record.getMax_stmspd());
        setReal(insertStatement, 18, record.getMax_timdif());
        setReal(insertStatement, 19, record.getMin_artcon());
        setReal(insertStatement, 20, record.getTim_p1cont());
        setReal(insertStatement, 21, record.getTim_p2cont());
        setReal(insertStatement, 22, record.getMax_ecarch());
        setReal(insertStatement, 23, record.getRng_cutoff());
        setReal(insertStatement, 24, record.getRng_e1coef());
        setReal(insertStatement, 25, record.getRng_e2coef());
        setReal(insertStatement, 26, record.getRng_e3coef());
        setReal(insertStatement, 27, record.getMin_prate());
        setReal(insertStatement, 28, record.getMax_prate());
        setReal(insertStatement, 29, record.getTim_restrt());
        setReal(insertStatement, 30, record.getMax_timint());
        setReal(insertStatement, 31, record.getMin_timprd());
        setReal(insertStatement, 32, record.getThr_hlyout());
        setReal(insertStatement, 33, record.getEnd_timgag());
        setReal(insertStatement, 34, record.getMax_prdval());
        setReal(insertStatement, 35, record.getMax_hlyval());
        setReal(insertStatement, 36, record.getTim_biest());
        setReal(insertStatement, 37, record.getThr_nosets());
        setReal(insertStatement, 38, record.getRes_bias());
        setReal(insertStatement, 39, record.getLongest_lag());
        setString(insertStatement, 40, record.getBias_applied());
        
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
        String deleteStatement = "DELETE FROM dhradapt " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a dhradaptRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(dhradaptRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE dhradapt SET radid = ?, obstime = ?, min_reflth = ?, max_reflth = ?, ref_tltest = ?, rng_tltin = ?, rng_tltout = ?, max_birng = ?, min_birng = ?, min_echoar = ?, min_awrefl = ?, max_pctred = ?, mlt_zrcoef = ?, pwr_zrcoef = ?, min_zrefl = ?, max_zrefl = ?, max_stmspd = ?, max_timdif = ?, min_artcon = ?, tim_p1cont = ?, tim_p2cont = ?, max_ecarch = ?, rng_cutoff = ?, rng_e1coef = ?, rng_e2coef = ?, rng_e3coef = ?, min_prate = ?, max_prate = ?, tim_restrt = ?, max_timint = ?, min_timprd = ?, thr_hlyout = ?, end_timgag = ?, max_prdval = ?, max_hlyval = ?, tim_biest = ?, thr_nosets = ?, res_bias = ?, longest_lag = ?, bias_applied = ?        " + where );

        setString(updateStatement, 1, record.getRadid());
        setTimeStamp(updateStatement, 2, record.getObstime());
        setReal(updateStatement, 3, record.getMin_reflth());
        setReal(updateStatement, 4, record.getMax_reflth());
        setReal(updateStatement, 5, record.getRef_tltest());
        setReal(updateStatement, 6, record.getRng_tltin());
        setReal(updateStatement, 7, record.getRng_tltout());
        setReal(updateStatement, 8, record.getMax_birng());
        setReal(updateStatement, 9, record.getMin_birng());
        setReal(updateStatement, 10, record.getMin_echoar());
        setReal(updateStatement, 11, record.getMin_awrefl());
        setReal(updateStatement, 12, record.getMax_pctred());
        setReal(updateStatement, 13, record.getMlt_zrcoef());
        setReal(updateStatement, 14, record.getPwr_zrcoef());
        setReal(updateStatement, 15, record.getMin_zrefl());
        setReal(updateStatement, 16, record.getMax_zrefl());
        setReal(updateStatement, 17, record.getMax_stmspd());
        setReal(updateStatement, 18, record.getMax_timdif());
        setReal(updateStatement, 19, record.getMin_artcon());
        setReal(updateStatement, 20, record.getTim_p1cont());
        setReal(updateStatement, 21, record.getTim_p2cont());
        setReal(updateStatement, 22, record.getMax_ecarch());
        setReal(updateStatement, 23, record.getRng_cutoff());
        setReal(updateStatement, 24, record.getRng_e1coef());
        setReal(updateStatement, 25, record.getRng_e2coef());
        setReal(updateStatement, 26, record.getRng_e3coef());
        setReal(updateStatement, 27, record.getMin_prate());
        setReal(updateStatement, 28, record.getMax_prate());
        setReal(updateStatement, 29, record.getTim_restrt());
        setReal(updateStatement, 30, record.getMax_timint());
        setReal(updateStatement, 31, record.getMin_timprd());
        setReal(updateStatement, 32, record.getThr_hlyout());
        setReal(updateStatement, 33, record.getEnd_timgag());
        setReal(updateStatement, 34, record.getMax_prdval());
        setReal(updateStatement, 35, record.getMax_hlyval());
        setReal(updateStatement, 36, record.getTim_biest());
        setReal(updateStatement, 37, record.getThr_nosets());
        setReal(updateStatement, 38, record.getRes_bias());
        setReal(updateStatement, 39, record.getLongest_lag());
        setString(updateStatement, 40, record.getBias_applied());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(dhradaptRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM dhradapt " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a dhradaptRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(dhradaptRecord oldRecord, dhradaptRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE dhradapt SET radid = ?, obstime = ?, min_reflth = ?, max_reflth = ?, ref_tltest = ?, rng_tltin = ?, rng_tltout = ?, max_birng = ?, min_birng = ?, min_echoar = ?, min_awrefl = ?, max_pctred = ?, mlt_zrcoef = ?, pwr_zrcoef = ?, min_zrefl = ?, max_zrefl = ?, max_stmspd = ?, max_timdif = ?, min_artcon = ?, tim_p1cont = ?, tim_p2cont = ?, max_ecarch = ?, rng_cutoff = ?, rng_e1coef = ?, rng_e2coef = ?, rng_e3coef = ?, min_prate = ?, max_prate = ?, tim_restrt = ?, max_timint = ?, min_timprd = ?, thr_hlyout = ?, end_timgag = ?, max_prdval = ?, max_hlyval = ?, tim_biest = ?, thr_nosets = ?, res_bias = ?, longest_lag = ?, bias_applied = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getRadid());
        setTimeStamp(updateStatement, 2, newRecord.getObstime());
        setReal(updateStatement, 3, newRecord.getMin_reflth());
        setReal(updateStatement, 4, newRecord.getMax_reflth());
        setReal(updateStatement, 5, newRecord.getRef_tltest());
        setReal(updateStatement, 6, newRecord.getRng_tltin());
        setReal(updateStatement, 7, newRecord.getRng_tltout());
        setReal(updateStatement, 8, newRecord.getMax_birng());
        setReal(updateStatement, 9, newRecord.getMin_birng());
        setReal(updateStatement, 10, newRecord.getMin_echoar());
        setReal(updateStatement, 11, newRecord.getMin_awrefl());
        setReal(updateStatement, 12, newRecord.getMax_pctred());
        setReal(updateStatement, 13, newRecord.getMlt_zrcoef());
        setReal(updateStatement, 14, newRecord.getPwr_zrcoef());
        setReal(updateStatement, 15, newRecord.getMin_zrefl());
        setReal(updateStatement, 16, newRecord.getMax_zrefl());
        setReal(updateStatement, 17, newRecord.getMax_stmspd());
        setReal(updateStatement, 18, newRecord.getMax_timdif());
        setReal(updateStatement, 19, newRecord.getMin_artcon());
        setReal(updateStatement, 20, newRecord.getTim_p1cont());
        setReal(updateStatement, 21, newRecord.getTim_p2cont());
        setReal(updateStatement, 22, newRecord.getMax_ecarch());
        setReal(updateStatement, 23, newRecord.getRng_cutoff());
        setReal(updateStatement, 24, newRecord.getRng_e1coef());
        setReal(updateStatement, 25, newRecord.getRng_e2coef());
        setReal(updateStatement, 26, newRecord.getRng_e3coef());
        setReal(updateStatement, 27, newRecord.getMin_prate());
        setReal(updateStatement, 28, newRecord.getMax_prate());
        setReal(updateStatement, 29, newRecord.getTim_restrt());
        setReal(updateStatement, 30, newRecord.getMax_timint());
        setReal(updateStatement, 31, newRecord.getMin_timprd());
        setReal(updateStatement, 32, newRecord.getThr_hlyout());
        setReal(updateStatement, 33, newRecord.getEnd_timgag());
        setReal(updateStatement, 34, newRecord.getMax_prdval());
        setReal(updateStatement, 35, newRecord.getMax_hlyval());
        setReal(updateStatement, 36, newRecord.getTim_biest());
        setReal(updateStatement, 37, newRecord.getThr_nosets());
        setReal(updateStatement, 38, newRecord.getRes_bias());
        setReal(updateStatement, 39, newRecord.getLongest_lag());
        setString(updateStatement, 40, newRecord.getBias_applied());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a dhradaptRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(dhradaptRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            dhradaptRecord oldRecord = (dhradaptRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of dhradaptTable class
