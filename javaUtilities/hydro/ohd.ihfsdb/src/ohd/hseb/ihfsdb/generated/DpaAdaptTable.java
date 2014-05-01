// filename: DpaAdaptTable.java
// author  : DBGEN
// created : Tue May 31 17:52:21 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              dpaadapt table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class DpaAdaptTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  DpaAdaptTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public DpaAdaptTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("dpaadapt");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of DpaAdaptRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        DpaAdaptRecord record = null;

        // create a List to hold DpaAdapt Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM dpaadapt " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a DpaAdaptRecord
            // and store its address in oneRecord
            record = new DpaAdaptRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a DpaAdaptRecord object

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
            record.setBeam_width(getReal(rs, 17));
            record.setBlockage_thresh(getReal(rs, 18));
            record.setClutter_thresh(getReal(rs, 19));
            record.setWeight_thresh(getReal(rs, 20));
            record.setHybrid_scan_thresh(getReal(rs, 21));
            record.setLow_reflect_thresh(getReal(rs, 22));
            record.setDetect_reflect_thr(getReal(rs, 23));
            record.setDetect_area_thresh(getReal(rs, 24));
            record.setDetect_time_thresh(getReal(rs, 25));
            record.setExclusion_zones(getReal(rs, 26));
            record.setMax_stmspd(getReal(rs, 27));
            record.setMax_timdif(getReal(rs, 28));
            record.setMin_artcon(getReal(rs, 29));
            record.setTim_p1cont(getReal(rs, 30));
            record.setTim_p2cont(getReal(rs, 31));
            record.setMax_ecarch(getReal(rs, 32));
            record.setRng_cutoff(getReal(rs, 33));
            record.setRng_e1coef(getReal(rs, 34));
            record.setRng_e2coef(getReal(rs, 35));
            record.setRng_e3coef(getReal(rs, 36));
            record.setMin_prate(getReal(rs, 37));
            record.setMax_prate(getReal(rs, 38));
            record.setTim_restrt(getReal(rs, 39));
            record.setMax_timint(getReal(rs, 40));
            record.setMin_timprd(getReal(rs, 41));
            record.setThr_hlyout(getReal(rs, 42));
            record.setEnd_timgag(getReal(rs, 43));
            record.setMax_prdval(getReal(rs, 44));
            record.setMax_hlyval(getReal(rs, 45));
            record.setTim_biest(getReal(rs, 46));
            record.setThr_nosets(getReal(rs, 47));
            record.setRes_bias(getReal(rs, 48));
            record.setLongest_lag(getReal(rs, 49));
            record.setBias_applied(getString(rs, 50));
            
            // add this DpaAdaptRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the DpaAdaptRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of DpaAdaptRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        DpaAdaptRecord record = null;

        // create a List to hold DpaAdapt Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM dpaadapt " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a DpaAdaptRecord
            // and store its address in oneRecord
            record = new DpaAdaptRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a DpaAdaptRecord object

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
            record.setBeam_width(getReal(rs, 17));
            record.setBlockage_thresh(getReal(rs, 18));
            record.setClutter_thresh(getReal(rs, 19));
            record.setWeight_thresh(getReal(rs, 20));
            record.setHybrid_scan_thresh(getReal(rs, 21));
            record.setLow_reflect_thresh(getReal(rs, 22));
            record.setDetect_reflect_thr(getReal(rs, 23));
            record.setDetect_area_thresh(getReal(rs, 24));
            record.setDetect_time_thresh(getReal(rs, 25));
            record.setExclusion_zones(getReal(rs, 26));
            record.setMax_stmspd(getReal(rs, 27));
            record.setMax_timdif(getReal(rs, 28));
            record.setMin_artcon(getReal(rs, 29));
            record.setTim_p1cont(getReal(rs, 30));
            record.setTim_p2cont(getReal(rs, 31));
            record.setMax_ecarch(getReal(rs, 32));
            record.setRng_cutoff(getReal(rs, 33));
            record.setRng_e1coef(getReal(rs, 34));
            record.setRng_e2coef(getReal(rs, 35));
            record.setRng_e3coef(getReal(rs, 36));
            record.setMin_prate(getReal(rs, 37));
            record.setMax_prate(getReal(rs, 38));
            record.setTim_restrt(getReal(rs, 39));
            record.setMax_timint(getReal(rs, 40));
            record.setMin_timprd(getReal(rs, 41));
            record.setThr_hlyout(getReal(rs, 42));
            record.setEnd_timgag(getReal(rs, 43));
            record.setMax_prdval(getReal(rs, 44));
            record.setMax_hlyval(getReal(rs, 45));
            record.setTim_biest(getReal(rs, 46));
            record.setThr_nosets(getReal(rs, 47));
            record.setRes_bias(getReal(rs, 48));
            record.setLongest_lag(getReal(rs, 49));
            record.setBias_applied(getString(rs, 50));
            
            // add this DpaAdaptRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the DpaAdaptRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a DpaAdaptRecord object and..
//-----------------------------------------------------------------
    public int insert(DpaAdaptRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO dpaadapt VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

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
        setReal(insertStatement, 17, record.getBeam_width());
        setReal(insertStatement, 18, record.getBlockage_thresh());
        setReal(insertStatement, 19, record.getClutter_thresh());
        setReal(insertStatement, 20, record.getWeight_thresh());
        setReal(insertStatement, 21, record.getHybrid_scan_thresh());
        setReal(insertStatement, 22, record.getLow_reflect_thresh());
        setReal(insertStatement, 23, record.getDetect_reflect_thr());
        setReal(insertStatement, 24, record.getDetect_area_thresh());
        setReal(insertStatement, 25, record.getDetect_time_thresh());
        setReal(insertStatement, 26, record.getExclusion_zones());
        setReal(insertStatement, 27, record.getMax_stmspd());
        setReal(insertStatement, 28, record.getMax_timdif());
        setReal(insertStatement, 29, record.getMin_artcon());
        setReal(insertStatement, 30, record.getTim_p1cont());
        setReal(insertStatement, 31, record.getTim_p2cont());
        setReal(insertStatement, 32, record.getMax_ecarch());
        setReal(insertStatement, 33, record.getRng_cutoff());
        setReal(insertStatement, 34, record.getRng_e1coef());
        setReal(insertStatement, 35, record.getRng_e2coef());
        setReal(insertStatement, 36, record.getRng_e3coef());
        setReal(insertStatement, 37, record.getMin_prate());
        setReal(insertStatement, 38, record.getMax_prate());
        setReal(insertStatement, 39, record.getTim_restrt());
        setReal(insertStatement, 40, record.getMax_timint());
        setReal(insertStatement, 41, record.getMin_timprd());
        setReal(insertStatement, 42, record.getThr_hlyout());
        setReal(insertStatement, 43, record.getEnd_timgag());
        setReal(insertStatement, 44, record.getMax_prdval());
        setReal(insertStatement, 45, record.getMax_hlyval());
        setReal(insertStatement, 46, record.getTim_biest());
        setReal(insertStatement, 47, record.getThr_nosets());
        setReal(insertStatement, 48, record.getRes_bias());
        setReal(insertStatement, 49, record.getLongest_lag());
        setString(insertStatement, 50, record.getBias_applied());
        
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
        String deleteStatement = "DELETE FROM dpaadapt " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a DpaAdaptRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(DpaAdaptRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE dpaadapt SET radid = ?, obstime = ?, min_reflth = ?, max_reflth = ?, ref_tltest = ?, rng_tltin = ?, rng_tltout = ?, max_birng = ?, min_birng = ?, min_echoar = ?, min_awrefl = ?, max_pctred = ?, mlt_zrcoef = ?, pwr_zrcoef = ?, min_zrefl = ?, max_zrefl = ?, beam_width = ?, blockage_thresh = ?, clutter_thresh = ?, weight_thresh = ?, hybrid_scan_thresh = ?, low_reflect_thresh = ?, detect_reflect_thr = ?, detect_area_thresh = ?, detect_time_thresh = ?, exclusion_zones = ?, max_stmspd = ?, max_timdif = ?, min_artcon = ?, tim_p1cont = ?, tim_p2cont = ?, max_ecarch = ?, rng_cutoff = ?, rng_e1coef = ?, rng_e2coef = ?, rng_e3coef = ?, min_prate = ?, max_prate = ?, tim_restrt = ?, max_timint = ?, min_timprd = ?, thr_hlyout = ?, end_timgag = ?, max_prdval = ?, max_hlyval = ?, tim_biest = ?, thr_nosets = ?, res_bias = ?, longest_lag = ?, bias_applied = ?        " + where );

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
        setReal(updateStatement, 17, record.getBeam_width());
        setReal(updateStatement, 18, record.getBlockage_thresh());
        setReal(updateStatement, 19, record.getClutter_thresh());
        setReal(updateStatement, 20, record.getWeight_thresh());
        setReal(updateStatement, 21, record.getHybrid_scan_thresh());
        setReal(updateStatement, 22, record.getLow_reflect_thresh());
        setReal(updateStatement, 23, record.getDetect_reflect_thr());
        setReal(updateStatement, 24, record.getDetect_area_thresh());
        setReal(updateStatement, 25, record.getDetect_time_thresh());
        setReal(updateStatement, 26, record.getExclusion_zones());
        setReal(updateStatement, 27, record.getMax_stmspd());
        setReal(updateStatement, 28, record.getMax_timdif());
        setReal(updateStatement, 29, record.getMin_artcon());
        setReal(updateStatement, 30, record.getTim_p1cont());
        setReal(updateStatement, 31, record.getTim_p2cont());
        setReal(updateStatement, 32, record.getMax_ecarch());
        setReal(updateStatement, 33, record.getRng_cutoff());
        setReal(updateStatement, 34, record.getRng_e1coef());
        setReal(updateStatement, 35, record.getRng_e2coef());
        setReal(updateStatement, 36, record.getRng_e3coef());
        setReal(updateStatement, 37, record.getMin_prate());
        setReal(updateStatement, 38, record.getMax_prate());
        setReal(updateStatement, 39, record.getTim_restrt());
        setReal(updateStatement, 40, record.getMax_timint());
        setReal(updateStatement, 41, record.getMin_timprd());
        setReal(updateStatement, 42, record.getThr_hlyout());
        setReal(updateStatement, 43, record.getEnd_timgag());
        setReal(updateStatement, 44, record.getMax_prdval());
        setReal(updateStatement, 45, record.getMax_hlyval());
        setReal(updateStatement, 46, record.getTim_biest());
        setReal(updateStatement, 47, record.getThr_nosets());
        setReal(updateStatement, 48, record.getRes_bias());
        setReal(updateStatement, 49, record.getLongest_lag());
        setString(updateStatement, 50, record.getBias_applied());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(DpaAdaptRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM dpaadapt " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a DpaAdaptRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(DpaAdaptRecord oldRecord, DpaAdaptRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE dpaadapt SET radid = ?, obstime = ?, min_reflth = ?, max_reflth = ?, ref_tltest = ?, rng_tltin = ?, rng_tltout = ?, max_birng = ?, min_birng = ?, min_echoar = ?, min_awrefl = ?, max_pctred = ?, mlt_zrcoef = ?, pwr_zrcoef = ?, min_zrefl = ?, max_zrefl = ?, beam_width = ?, blockage_thresh = ?, clutter_thresh = ?, weight_thresh = ?, hybrid_scan_thresh = ?, low_reflect_thresh = ?, detect_reflect_thr = ?, detect_area_thresh = ?, detect_time_thresh = ?, exclusion_zones = ?, max_stmspd = ?, max_timdif = ?, min_artcon = ?, tim_p1cont = ?, tim_p2cont = ?, max_ecarch = ?, rng_cutoff = ?, rng_e1coef = ?, rng_e2coef = ?, rng_e3coef = ?, min_prate = ?, max_prate = ?, tim_restrt = ?, max_timint = ?, min_timprd = ?, thr_hlyout = ?, end_timgag = ?, max_prdval = ?, max_hlyval = ?, tim_biest = ?, thr_nosets = ?, res_bias = ?, longest_lag = ?, bias_applied = ?        " + oldRecord.getWhereString() );

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
        setReal(updateStatement, 17, newRecord.getBeam_width());
        setReal(updateStatement, 18, newRecord.getBlockage_thresh());
        setReal(updateStatement, 19, newRecord.getClutter_thresh());
        setReal(updateStatement, 20, newRecord.getWeight_thresh());
        setReal(updateStatement, 21, newRecord.getHybrid_scan_thresh());
        setReal(updateStatement, 22, newRecord.getLow_reflect_thresh());
        setReal(updateStatement, 23, newRecord.getDetect_reflect_thr());
        setReal(updateStatement, 24, newRecord.getDetect_area_thresh());
        setReal(updateStatement, 25, newRecord.getDetect_time_thresh());
        setReal(updateStatement, 26, newRecord.getExclusion_zones());
        setReal(updateStatement, 27, newRecord.getMax_stmspd());
        setReal(updateStatement, 28, newRecord.getMax_timdif());
        setReal(updateStatement, 29, newRecord.getMin_artcon());
        setReal(updateStatement, 30, newRecord.getTim_p1cont());
        setReal(updateStatement, 31, newRecord.getTim_p2cont());
        setReal(updateStatement, 32, newRecord.getMax_ecarch());
        setReal(updateStatement, 33, newRecord.getRng_cutoff());
        setReal(updateStatement, 34, newRecord.getRng_e1coef());
        setReal(updateStatement, 35, newRecord.getRng_e2coef());
        setReal(updateStatement, 36, newRecord.getRng_e3coef());
        setReal(updateStatement, 37, newRecord.getMin_prate());
        setReal(updateStatement, 38, newRecord.getMax_prate());
        setReal(updateStatement, 39, newRecord.getTim_restrt());
        setReal(updateStatement, 40, newRecord.getMax_timint());
        setReal(updateStatement, 41, newRecord.getMin_timprd());
        setReal(updateStatement, 42, newRecord.getThr_hlyout());
        setReal(updateStatement, 43, newRecord.getEnd_timgag());
        setReal(updateStatement, 44, newRecord.getMax_prdval());
        setReal(updateStatement, 45, newRecord.getMax_hlyval());
        setReal(updateStatement, 46, newRecord.getTim_biest());
        setReal(updateStatement, 47, newRecord.getThr_nosets());
        setReal(updateStatement, 48, newRecord.getRes_bias());
        setReal(updateStatement, 49, newRecord.getLongest_lag());
        setString(updateStatement, 50, newRecord.getBias_applied());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a DpaAdaptRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(DpaAdaptRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            DpaAdaptRecord oldRecord = (DpaAdaptRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of DpaAdaptTable class
