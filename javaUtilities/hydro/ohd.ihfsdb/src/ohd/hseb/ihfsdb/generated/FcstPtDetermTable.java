// filename: FcstPtDetermTable.java
// author  : DBGEN
// created : Tue May 31 17:52:22 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              fcstptdeterm table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class FcstPtDetermTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  FcstPtDetermTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public FcstPtDetermTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("fcstptdeterm");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of FcstPtDetermRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        FcstPtDetermRecord record = null;

        // create a List to hold FcstPtDeterm Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM fcstptdeterm " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a FcstPtDetermRecord
            // and store its address in oneRecord
            record = new FcstPtDetermRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a FcstPtDetermRecord object

            record.setLid(getString(rs, 1));
            record.setSnow_method(getString(rs, 2));
            record.setHydrol_method(getString(rs, 3));
            record.setReservoir_model(getString(rs, 4));
            record.setUpstream_seg(getString(rs, 5));
            record.setHydraul_method(getString(rs, 6));
            record.setDef_issue_crit(getString(rs, 7));
            record.setHours_qpf(getShort(rs, 8));
            record.setFrequpd_normal(getString(rs, 9));
            record.setFrequpd_flood(getString(rs, 10));
            record.setFrequpd_drought(getString(rs, 11));
            record.setFcst_horizon(getString(rs, 12));
            record.setHours_qtf(getShort(rs, 13));
            record.setHours_qzf(getShort(rs, 14));
            record.setNum_elev_zones(getShort(rs, 15));
            record.setConsumptive_use(getString(rs, 16));
            record.setChannel_loss(getString(rs, 17));
            record.setFcst_gen_method(getString(rs, 18));
            record.setImpl_date(getDate(rs, 19));
            record.setWeb_date(getDate(rs, 20));
            record.setVar_usage(getString(rs, 21));
            
            // add this FcstPtDetermRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the FcstPtDetermRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of FcstPtDetermRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        FcstPtDetermRecord record = null;

        // create a List to hold FcstPtDeterm Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM fcstptdeterm " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a FcstPtDetermRecord
            // and store its address in oneRecord
            record = new FcstPtDetermRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a FcstPtDetermRecord object

            record.setLid(getString(rs, 1));
            record.setSnow_method(getString(rs, 2));
            record.setHydrol_method(getString(rs, 3));
            record.setReservoir_model(getString(rs, 4));
            record.setUpstream_seg(getString(rs, 5));
            record.setHydraul_method(getString(rs, 6));
            record.setDef_issue_crit(getString(rs, 7));
            record.setHours_qpf(getShort(rs, 8));
            record.setFrequpd_normal(getString(rs, 9));
            record.setFrequpd_flood(getString(rs, 10));
            record.setFrequpd_drought(getString(rs, 11));
            record.setFcst_horizon(getString(rs, 12));
            record.setHours_qtf(getShort(rs, 13));
            record.setHours_qzf(getShort(rs, 14));
            record.setNum_elev_zones(getShort(rs, 15));
            record.setConsumptive_use(getString(rs, 16));
            record.setChannel_loss(getString(rs, 17));
            record.setFcst_gen_method(getString(rs, 18));
            record.setImpl_date(getDate(rs, 19));
            record.setWeb_date(getDate(rs, 20));
            record.setVar_usage(getString(rs, 21));
            
            // add this FcstPtDetermRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the FcstPtDetermRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a FcstPtDetermRecord object and..
//-----------------------------------------------------------------
    public int insert(FcstPtDetermRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO fcstptdeterm VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getSnow_method());
        setString(insertStatement, 3, record.getHydrol_method());
        setString(insertStatement, 4, record.getReservoir_model());
        setString(insertStatement, 5, record.getUpstream_seg());
        setString(insertStatement, 6, record.getHydraul_method());
        setString(insertStatement, 7, record.getDef_issue_crit());
        setShort(insertStatement, 8, record.getHours_qpf());
        setString(insertStatement, 9, record.getFrequpd_normal());
        setString(insertStatement, 10, record.getFrequpd_flood());
        setString(insertStatement, 11, record.getFrequpd_drought());
        setString(insertStatement, 12, record.getFcst_horizon());
        setShort(insertStatement, 13, record.getHours_qtf());
        setShort(insertStatement, 14, record.getHours_qzf());
        setShort(insertStatement, 15, record.getNum_elev_zones());
        setString(insertStatement, 16, record.getConsumptive_use());
        setString(insertStatement, 17, record.getChannel_loss());
        setString(insertStatement, 18, record.getFcst_gen_method());
        setDate(insertStatement, 19, record.getImpl_date());
        setDate(insertStatement, 20, record.getWeb_date());
        setString(insertStatement, 21, record.getVar_usage());
        
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
        String deleteStatement = "DELETE FROM fcstptdeterm " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a FcstPtDetermRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(FcstPtDetermRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE fcstptdeterm SET lid = ?, snow_method = ?, hydrol_method = ?, reservoir_model = ?, upstream_seg = ?, hydraul_method = ?, def_issue_crit = ?, hours_qpf = ?, frequpd_normal = ?, frequpd_flood = ?, frequpd_drought = ?, fcst_horizon = ?, hours_qtf = ?, hours_qzf = ?, num_elev_zones = ?, consumptive_use = ?, channel_loss = ?, fcst_gen_method = ?, impl_date = ?, web_date = ?, var_usage = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setString(updateStatement, 2, record.getSnow_method());
        setString(updateStatement, 3, record.getHydrol_method());
        setString(updateStatement, 4, record.getReservoir_model());
        setString(updateStatement, 5, record.getUpstream_seg());
        setString(updateStatement, 6, record.getHydraul_method());
        setString(updateStatement, 7, record.getDef_issue_crit());
        setShort(updateStatement, 8, record.getHours_qpf());
        setString(updateStatement, 9, record.getFrequpd_normal());
        setString(updateStatement, 10, record.getFrequpd_flood());
        setString(updateStatement, 11, record.getFrequpd_drought());
        setString(updateStatement, 12, record.getFcst_horizon());
        setShort(updateStatement, 13, record.getHours_qtf());
        setShort(updateStatement, 14, record.getHours_qzf());
        setShort(updateStatement, 15, record.getNum_elev_zones());
        setString(updateStatement, 16, record.getConsumptive_use());
        setString(updateStatement, 17, record.getChannel_loss());
        setString(updateStatement, 18, record.getFcst_gen_method());
        setDate(updateStatement, 19, record.getImpl_date());
        setDate(updateStatement, 20, record.getWeb_date());
        setString(updateStatement, 21, record.getVar_usage());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(FcstPtDetermRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM fcstptdeterm " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a FcstPtDetermRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(FcstPtDetermRecord oldRecord, FcstPtDetermRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE fcstptdeterm SET lid = ?, snow_method = ?, hydrol_method = ?, reservoir_model = ?, upstream_seg = ?, hydraul_method = ?, def_issue_crit = ?, hours_qpf = ?, frequpd_normal = ?, frequpd_flood = ?, frequpd_drought = ?, fcst_horizon = ?, hours_qtf = ?, hours_qzf = ?, num_elev_zones = ?, consumptive_use = ?, channel_loss = ?, fcst_gen_method = ?, impl_date = ?, web_date = ?, var_usage = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getSnow_method());
        setString(updateStatement, 3, newRecord.getHydrol_method());
        setString(updateStatement, 4, newRecord.getReservoir_model());
        setString(updateStatement, 5, newRecord.getUpstream_seg());
        setString(updateStatement, 6, newRecord.getHydraul_method());
        setString(updateStatement, 7, newRecord.getDef_issue_crit());
        setShort(updateStatement, 8, newRecord.getHours_qpf());
        setString(updateStatement, 9, newRecord.getFrequpd_normal());
        setString(updateStatement, 10, newRecord.getFrequpd_flood());
        setString(updateStatement, 11, newRecord.getFrequpd_drought());
        setString(updateStatement, 12, newRecord.getFcst_horizon());
        setShort(updateStatement, 13, newRecord.getHours_qtf());
        setShort(updateStatement, 14, newRecord.getHours_qzf());
        setShort(updateStatement, 15, newRecord.getNum_elev_zones());
        setString(updateStatement, 16, newRecord.getConsumptive_use());
        setString(updateStatement, 17, newRecord.getChannel_loss());
        setString(updateStatement, 18, newRecord.getFcst_gen_method());
        setDate(updateStatement, 19, newRecord.getImpl_date());
        setDate(updateStatement, 20, newRecord.getWeb_date());
        setString(updateStatement, 21, newRecord.getVar_usage());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a FcstPtDetermRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(FcstPtDetermRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            FcstPtDetermRecord oldRecord = (FcstPtDetermRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of FcstPtDetermTable class
