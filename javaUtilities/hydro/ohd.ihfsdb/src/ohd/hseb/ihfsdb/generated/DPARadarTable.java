// filename: DPARadarTable.java
// author  : DBGEN
// created : Tue May 31 17:52:21 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              dparadar table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class DPARadarTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  DPARadarTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public DPARadarTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("dparadar");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of DPARadarRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        DPARadarRecord record = null;

        // create a List to hold DPARadar Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM dparadar " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a DPARadarRecord
            // and store its address in oneRecord
            record = new DPARadarRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a DPARadarRecord object

            record.setRadid(getString(rs, 1));
            record.setObstime(getTimeStamp(rs, 2));
            record.setMinoff(getShort(rs, 3));
            record.setMaxvalh(getReal(rs, 4));
            record.setMaxvald(getReal(rs, 5));
            record.setS1_bias_value(getReal(rs, 6));
            record.setProducttime(getTimeStamp(rs, 7));
            record.setNisolbin(getShort(rs, 8));
            record.setNoutint(getShort(rs, 9));
            record.setNoutrep(getShort(rs, 10));
            record.setAreared(getReal(rs, 11));
            record.setBiscanr(getReal(rs, 12));
            record.setBlock_bins_reject(getInt(rs, 13));
            record.setClutter_bins_rej(getInt(rs, 14));
            record.setBins_smoothed(getInt(rs, 15));
            record.setScan_bins_filled(getReal(rs, 16));
            record.setHigh_elev_angle(getReal(rs, 17));
            record.setScan_rain_area(getReal(rs, 18));
            record.setNbadscan(getShort(rs, 19));
            record.setNhourout(getShort(rs, 20));
            record.setVolcovpat(getShort(rs, 21));
            record.setOpermode(getShort(rs, 22));
            record.setMissper(getString(rs, 23));
            record.setSupplmess(getShort(rs, 24));
            record.setGrid_filename(getString(rs, 25));
            
            // add this DPARadarRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the DPARadarRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of DPARadarRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        DPARadarRecord record = null;

        // create a List to hold DPARadar Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM dparadar " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a DPARadarRecord
            // and store its address in oneRecord
            record = new DPARadarRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a DPARadarRecord object

            record.setRadid(getString(rs, 1));
            record.setObstime(getTimeStamp(rs, 2));
            record.setMinoff(getShort(rs, 3));
            record.setMaxvalh(getReal(rs, 4));
            record.setMaxvald(getReal(rs, 5));
            record.setS1_bias_value(getReal(rs, 6));
            record.setProducttime(getTimeStamp(rs, 7));
            record.setNisolbin(getShort(rs, 8));
            record.setNoutint(getShort(rs, 9));
            record.setNoutrep(getShort(rs, 10));
            record.setAreared(getReal(rs, 11));
            record.setBiscanr(getReal(rs, 12));
            record.setBlock_bins_reject(getInt(rs, 13));
            record.setClutter_bins_rej(getInt(rs, 14));
            record.setBins_smoothed(getInt(rs, 15));
            record.setScan_bins_filled(getReal(rs, 16));
            record.setHigh_elev_angle(getReal(rs, 17));
            record.setScan_rain_area(getReal(rs, 18));
            record.setNbadscan(getShort(rs, 19));
            record.setNhourout(getShort(rs, 20));
            record.setVolcovpat(getShort(rs, 21));
            record.setOpermode(getShort(rs, 22));
            record.setMissper(getString(rs, 23));
            record.setSupplmess(getShort(rs, 24));
            record.setGrid_filename(getString(rs, 25));
            
            // add this DPARadarRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the DPARadarRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a DPARadarRecord object and..
//-----------------------------------------------------------------
    public int insert(DPARadarRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO dparadar VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getRadid());
        setTimeStamp(insertStatement, 2, record.getObstime());
        setShort(insertStatement, 3, record.getMinoff());
        setReal(insertStatement, 4, record.getMaxvalh());
        setReal(insertStatement, 5, record.getMaxvald());
        setReal(insertStatement, 6, record.getS1_bias_value());
        setTimeStamp(insertStatement, 7, record.getProducttime());
        setShort(insertStatement, 8, record.getNisolbin());
        setShort(insertStatement, 9, record.getNoutint());
        setShort(insertStatement, 10, record.getNoutrep());
        setReal(insertStatement, 11, record.getAreared());
        setReal(insertStatement, 12, record.getBiscanr());
        setInt(insertStatement, 13, record.getBlock_bins_reject());
        setInt(insertStatement, 14, record.getClutter_bins_rej());
        setInt(insertStatement, 15, record.getBins_smoothed());
        setReal(insertStatement, 16, record.getScan_bins_filled());
        setReal(insertStatement, 17, record.getHigh_elev_angle());
        setReal(insertStatement, 18, record.getScan_rain_area());
        setShort(insertStatement, 19, record.getNbadscan());
        setShort(insertStatement, 20, record.getNhourout());
        setShort(insertStatement, 21, record.getVolcovpat());
        setShort(insertStatement, 22, record.getOpermode());
        setString(insertStatement, 23, record.getMissper());
        setShort(insertStatement, 24, record.getSupplmess());
        setString(insertStatement, 25, record.getGrid_filename());
        
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
        String deleteStatement = "DELETE FROM dparadar " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a DPARadarRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(DPARadarRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE dparadar SET radid = ?, obstime = ?, minoff = ?, maxvalh = ?, maxvald = ?, s1_bias_value = ?, producttime = ?, nisolbin = ?, noutint = ?, noutrep = ?, areared = ?, biscanr = ?, block_bins_reject = ?, clutter_bins_rej = ?, bins_smoothed = ?, scan_bins_filled = ?, high_elev_angle = ?, scan_rain_area = ?, nbadscan = ?, nhourout = ?, volcovpat = ?, opermode = ?, missper = ?, supplmess = ?, grid_filename = ?        " + where );

        setString(updateStatement, 1, record.getRadid());
        setTimeStamp(updateStatement, 2, record.getObstime());
        setShort(updateStatement, 3, record.getMinoff());
        setReal(updateStatement, 4, record.getMaxvalh());
        setReal(updateStatement, 5, record.getMaxvald());
        setReal(updateStatement, 6, record.getS1_bias_value());
        setTimeStamp(updateStatement, 7, record.getProducttime());
        setShort(updateStatement, 8, record.getNisolbin());
        setShort(updateStatement, 9, record.getNoutint());
        setShort(updateStatement, 10, record.getNoutrep());
        setReal(updateStatement, 11, record.getAreared());
        setReal(updateStatement, 12, record.getBiscanr());
        setInt(updateStatement, 13, record.getBlock_bins_reject());
        setInt(updateStatement, 14, record.getClutter_bins_rej());
        setInt(updateStatement, 15, record.getBins_smoothed());
        setReal(updateStatement, 16, record.getScan_bins_filled());
        setReal(updateStatement, 17, record.getHigh_elev_angle());
        setReal(updateStatement, 18, record.getScan_rain_area());
        setShort(updateStatement, 19, record.getNbadscan());
        setShort(updateStatement, 20, record.getNhourout());
        setShort(updateStatement, 21, record.getVolcovpat());
        setShort(updateStatement, 22, record.getOpermode());
        setString(updateStatement, 23, record.getMissper());
        setShort(updateStatement, 24, record.getSupplmess());
        setString(updateStatement, 25, record.getGrid_filename());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(DPARadarRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM dparadar " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a DPARadarRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(DPARadarRecord oldRecord, DPARadarRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE dparadar SET radid = ?, obstime = ?, minoff = ?, maxvalh = ?, maxvald = ?, s1_bias_value = ?, producttime = ?, nisolbin = ?, noutint = ?, noutrep = ?, areared = ?, biscanr = ?, block_bins_reject = ?, clutter_bins_rej = ?, bins_smoothed = ?, scan_bins_filled = ?, high_elev_angle = ?, scan_rain_area = ?, nbadscan = ?, nhourout = ?, volcovpat = ?, opermode = ?, missper = ?, supplmess = ?, grid_filename = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getRadid());
        setTimeStamp(updateStatement, 2, newRecord.getObstime());
        setShort(updateStatement, 3, newRecord.getMinoff());
        setReal(updateStatement, 4, newRecord.getMaxvalh());
        setReal(updateStatement, 5, newRecord.getMaxvald());
        setReal(updateStatement, 6, newRecord.getS1_bias_value());
        setTimeStamp(updateStatement, 7, newRecord.getProducttime());
        setShort(updateStatement, 8, newRecord.getNisolbin());
        setShort(updateStatement, 9, newRecord.getNoutint());
        setShort(updateStatement, 10, newRecord.getNoutrep());
        setReal(updateStatement, 11, newRecord.getAreared());
        setReal(updateStatement, 12, newRecord.getBiscanr());
        setInt(updateStatement, 13, newRecord.getBlock_bins_reject());
        setInt(updateStatement, 14, newRecord.getClutter_bins_rej());
        setInt(updateStatement, 15, newRecord.getBins_smoothed());
        setReal(updateStatement, 16, newRecord.getScan_bins_filled());
        setReal(updateStatement, 17, newRecord.getHigh_elev_angle());
        setReal(updateStatement, 18, newRecord.getScan_rain_area());
        setShort(updateStatement, 19, newRecord.getNbadscan());
        setShort(updateStatement, 20, newRecord.getNhourout());
        setShort(updateStatement, 21, newRecord.getVolcovpat());
        setShort(updateStatement, 22, newRecord.getOpermode());
        setString(updateStatement, 23, newRecord.getMissper());
        setShort(updateStatement, 24, newRecord.getSupplmess());
        setString(updateStatement, 25, newRecord.getGrid_filename());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a DPARadarRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(DPARadarRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            DPARadarRecord oldRecord = (DPARadarRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of DPARadarTable class
