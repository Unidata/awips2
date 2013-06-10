// filename: RiverstatTable.java
// author  : DBGEN
// created : Tue May 31 17:52:27 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              riverstat table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class RiverstatTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  RiverstatTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public RiverstatTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("riverstat");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of RiverstatRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        RiverstatRecord record = null;

        // create a List to hold Riverstat Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM riverstat " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RiverstatRecord
            // and store its address in oneRecord
            record = new RiverstatRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RiverstatRecord object

            record.setLid(getString(rs, 1));
            record.setPrimary_pe(getString(rs, 2));
            record.setBf(getDouble(rs, 3));
            record.setCb(getDouble(rs, 4));
            record.setDa(getDouble(rs, 5));
            record.setResponse_time(getDouble(rs, 6));
            record.setThreshold_runoff(getDouble(rs, 7));
            record.setFq(getDouble(rs, 8));
            record.setFs(getDouble(rs, 9));
            record.setGsno(getString(rs, 10));
            record.setLevel(getString(rs, 11));
            record.setMile(getDouble(rs, 12));
            record.setPool(getDouble(rs, 13));
            record.setPor(getString(rs, 14));
            record.setRated(getString(rs, 15));
            record.setLat(getDouble(rs, 16));
            record.setLon(getDouble(rs, 17));
            record.setRemark(getString(rs, 18));
            record.setRrevise(getDate(rs, 19));
            record.setRsource(getString(rs, 20));
            record.setStream(getString(rs, 21));
            record.setTide(getString(rs, 22));
            record.setBackwater(getString(rs, 23));
            record.setVdatum(getString(rs, 24));
            record.setAction_flow(getDouble(rs, 25));
            record.setWstg(getDouble(rs, 26));
            record.setZd(getDouble(rs, 27));
            record.setRatedat(getDate(rs, 28));
            record.setUsgs_ratenum(getString(rs, 29));
            record.setUhgdur(getInt(rs, 30));
            record.setUse_latest_fcst(getString(rs, 31));
            
            // add this RiverstatRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the RiverstatRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of RiverstatRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        RiverstatRecord record = null;

        // create a List to hold Riverstat Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM riverstat " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RiverstatRecord
            // and store its address in oneRecord
            record = new RiverstatRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RiverstatRecord object

            record.setLid(getString(rs, 1));
            record.setPrimary_pe(getString(rs, 2));
            record.setBf(getDouble(rs, 3));
            record.setCb(getDouble(rs, 4));
            record.setDa(getDouble(rs, 5));
            record.setResponse_time(getDouble(rs, 6));
            record.setThreshold_runoff(getDouble(rs, 7));
            record.setFq(getDouble(rs, 8));
            record.setFs(getDouble(rs, 9));
            record.setGsno(getString(rs, 10));
            record.setLevel(getString(rs, 11));
            record.setMile(getDouble(rs, 12));
            record.setPool(getDouble(rs, 13));
            record.setPor(getString(rs, 14));
            record.setRated(getString(rs, 15));
            record.setLat(getDouble(rs, 16));
            record.setLon(getDouble(rs, 17));
            record.setRemark(getString(rs, 18));
            record.setRrevise(getDate(rs, 19));
            record.setRsource(getString(rs, 20));
            record.setStream(getString(rs, 21));
            record.setTide(getString(rs, 22));
            record.setBackwater(getString(rs, 23));
            record.setVdatum(getString(rs, 24));
            record.setAction_flow(getDouble(rs, 25));
            record.setWstg(getDouble(rs, 26));
            record.setZd(getDouble(rs, 27));
            record.setRatedat(getDate(rs, 28));
            record.setUsgs_ratenum(getString(rs, 29));
            record.setUhgdur(getInt(rs, 30));
            record.setUse_latest_fcst(getString(rs, 31));
            
            // add this RiverstatRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the RiverstatRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a RiverstatRecord object and..
//-----------------------------------------------------------------
    public int insert(RiverstatRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO riverstat VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getPrimary_pe());
        setDouble(insertStatement, 3, record.getBf());
        setDouble(insertStatement, 4, record.getCb());
        setDouble(insertStatement, 5, record.getDa());
        setDouble(insertStatement, 6, record.getResponse_time());
        setDouble(insertStatement, 7, record.getThreshold_runoff());
        setDouble(insertStatement, 8, record.getFq());
        setDouble(insertStatement, 9, record.getFs());
        setString(insertStatement, 10, record.getGsno());
        setString(insertStatement, 11, record.getLevel());
        setDouble(insertStatement, 12, record.getMile());
        setDouble(insertStatement, 13, record.getPool());
        setString(insertStatement, 14, record.getPor());
        setString(insertStatement, 15, record.getRated());
        setDouble(insertStatement, 16, record.getLat());
        setDouble(insertStatement, 17, record.getLon());
        setString(insertStatement, 18, record.getRemark());
        setDate(insertStatement, 19, record.getRrevise());
        setString(insertStatement, 20, record.getRsource());
        setString(insertStatement, 21, record.getStream());
        setString(insertStatement, 22, record.getTide());
        setString(insertStatement, 23, record.getBackwater());
        setString(insertStatement, 24, record.getVdatum());
        setDouble(insertStatement, 25, record.getAction_flow());
        setDouble(insertStatement, 26, record.getWstg());
        setDouble(insertStatement, 27, record.getZd());
        setDate(insertStatement, 28, record.getRatedat());
        setString(insertStatement, 29, record.getUsgs_ratenum());
        setInt(insertStatement, 30, record.getUhgdur());
        setString(insertStatement, 31, record.getUse_latest_fcst());
        
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
        String deleteStatement = "DELETE FROM riverstat " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RiverstatRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RiverstatRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE riverstat SET lid = ?, primary_pe = ?, bf = ?, cb = ?, da = ?, response_time = ?, threshold_runoff = ?, fq = ?, fs = ?, gsno = ?, level = ?, mile = ?, pool = ?, por = ?, rated = ?, lat = ?, lon = ?, remark = ?, rrevise = ?, rsource = ?, stream = ?, tide = ?, backwater = ?, vdatum = ?, action_flow = ?, wstg = ?, zd = ?, ratedat = ?, usgs_ratenum = ?, uhgdur = ?, use_latest_fcst = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setString(updateStatement, 2, record.getPrimary_pe());
        setDouble(updateStatement, 3, record.getBf());
        setDouble(updateStatement, 4, record.getCb());
        setDouble(updateStatement, 5, record.getDa());
        setDouble(updateStatement, 6, record.getResponse_time());
        setDouble(updateStatement, 7, record.getThreshold_runoff());
        setDouble(updateStatement, 8, record.getFq());
        setDouble(updateStatement, 9, record.getFs());
        setString(updateStatement, 10, record.getGsno());
        setString(updateStatement, 11, record.getLevel());
        setDouble(updateStatement, 12, record.getMile());
        setDouble(updateStatement, 13, record.getPool());
        setString(updateStatement, 14, record.getPor());
        setString(updateStatement, 15, record.getRated());
        setDouble(updateStatement, 16, record.getLat());
        setDouble(updateStatement, 17, record.getLon());
        setString(updateStatement, 18, record.getRemark());
        setDate(updateStatement, 19, record.getRrevise());
        setString(updateStatement, 20, record.getRsource());
        setString(updateStatement, 21, record.getStream());
        setString(updateStatement, 22, record.getTide());
        setString(updateStatement, 23, record.getBackwater());
        setString(updateStatement, 24, record.getVdatum());
        setDouble(updateStatement, 25, record.getAction_flow());
        setDouble(updateStatement, 26, record.getWstg());
        setDouble(updateStatement, 27, record.getZd());
        setDate(updateStatement, 28, record.getRatedat());
        setString(updateStatement, 29, record.getUsgs_ratenum());
        setInt(updateStatement, 30, record.getUhgdur());
        setString(updateStatement, 31, record.getUse_latest_fcst());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(RiverstatRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM riverstat " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RiverstatRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RiverstatRecord oldRecord, RiverstatRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE riverstat SET lid = ?, primary_pe = ?, bf = ?, cb = ?, da = ?, response_time = ?, threshold_runoff = ?, fq = ?, fs = ?, gsno = ?, level = ?, mile = ?, pool = ?, por = ?, rated = ?, lat = ?, lon = ?, remark = ?, rrevise = ?, rsource = ?, stream = ?, tide = ?, backwater = ?, vdatum = ?, action_flow = ?, wstg = ?, zd = ?, ratedat = ?, usgs_ratenum = ?, uhgdur = ?, use_latest_fcst = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getPrimary_pe());
        setDouble(updateStatement, 3, newRecord.getBf());
        setDouble(updateStatement, 4, newRecord.getCb());
        setDouble(updateStatement, 5, newRecord.getDa());
        setDouble(updateStatement, 6, newRecord.getResponse_time());
        setDouble(updateStatement, 7, newRecord.getThreshold_runoff());
        setDouble(updateStatement, 8, newRecord.getFq());
        setDouble(updateStatement, 9, newRecord.getFs());
        setString(updateStatement, 10, newRecord.getGsno());
        setString(updateStatement, 11, newRecord.getLevel());
        setDouble(updateStatement, 12, newRecord.getMile());
        setDouble(updateStatement, 13, newRecord.getPool());
        setString(updateStatement, 14, newRecord.getPor());
        setString(updateStatement, 15, newRecord.getRated());
        setDouble(updateStatement, 16, newRecord.getLat());
        setDouble(updateStatement, 17, newRecord.getLon());
        setString(updateStatement, 18, newRecord.getRemark());
        setDate(updateStatement, 19, newRecord.getRrevise());
        setString(updateStatement, 20, newRecord.getRsource());
        setString(updateStatement, 21, newRecord.getStream());
        setString(updateStatement, 22, newRecord.getTide());
        setString(updateStatement, 23, newRecord.getBackwater());
        setString(updateStatement, 24, newRecord.getVdatum());
        setDouble(updateStatement, 25, newRecord.getAction_flow());
        setDouble(updateStatement, 26, newRecord.getWstg());
        setDouble(updateStatement, 27, newRecord.getZd());
        setDate(updateStatement, 28, newRecord.getRatedat());
        setString(updateStatement, 29, newRecord.getUsgs_ratenum());
        setInt(updateStatement, 30, newRecord.getUhgdur());
        setString(updateStatement, 31, newRecord.getUse_latest_fcst());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a RiverstatRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(RiverstatRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            RiverstatRecord oldRecord = (RiverstatRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of RiverstatTable class
