// filename: FcstPtESPTable.java
// author  : DBGEN
// created : Tue May 31 17:52:22 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              fcstptesp table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class FcstPtESPTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  FcstPtESPTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public FcstPtESPTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("fcstptesp");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of FcstPtESPRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        FcstPtESPRecord record = null;

        // create a List to hold FcstPtESP Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM fcstptesp " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a FcstPtESPRecord
            // and store its address in oneRecord
            record = new FcstPtESPRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a FcstPtESPRecord object

            record.setLid(getString(rs, 1));
            record.setSnow_method(getString(rs, 2));
            record.setHydrol_method(getString(rs, 3));
            record.setReservoir_model(getString(rs, 4));
            record.setUpstream_seg(getString(rs, 5));
            record.setHydraul_method(getString(rs, 6));
            record.setFlowtype(getString(rs, 7));
            record.setFcsttype(getString(rs, 8));
            record.setFrequpd_normal(getString(rs, 9));
            record.setFrequpd_flood(getString(rs, 10));
            record.setFrequpd_drought(getString(rs, 11));
            record.setFcst_horizon(getString(rs, 12));
            record.setNummonclim(getShort(rs, 13));
            record.setNumdayhyd(getShort(rs, 14));
            record.setNum_elev_zones(getShort(rs, 15));
            record.setConsumptive_use(getString(rs, 16));
            record.setChannel_loss(getString(rs, 17));
            record.setPost_processor(getString(rs, 18));
            record.setImpl_date(getDate(rs, 19));
            record.setExternal_date(getDate(rs, 20));
            record.setWeb_date(getDate(rs, 21));
            record.setVar_usage(getString(rs, 22));
            
            // add this FcstPtESPRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the FcstPtESPRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of FcstPtESPRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        FcstPtESPRecord record = null;

        // create a List to hold FcstPtESP Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM fcstptesp " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a FcstPtESPRecord
            // and store its address in oneRecord
            record = new FcstPtESPRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a FcstPtESPRecord object

            record.setLid(getString(rs, 1));
            record.setSnow_method(getString(rs, 2));
            record.setHydrol_method(getString(rs, 3));
            record.setReservoir_model(getString(rs, 4));
            record.setUpstream_seg(getString(rs, 5));
            record.setHydraul_method(getString(rs, 6));
            record.setFlowtype(getString(rs, 7));
            record.setFcsttype(getString(rs, 8));
            record.setFrequpd_normal(getString(rs, 9));
            record.setFrequpd_flood(getString(rs, 10));
            record.setFrequpd_drought(getString(rs, 11));
            record.setFcst_horizon(getString(rs, 12));
            record.setNummonclim(getShort(rs, 13));
            record.setNumdayhyd(getShort(rs, 14));
            record.setNum_elev_zones(getShort(rs, 15));
            record.setConsumptive_use(getString(rs, 16));
            record.setChannel_loss(getString(rs, 17));
            record.setPost_processor(getString(rs, 18));
            record.setImpl_date(getDate(rs, 19));
            record.setExternal_date(getDate(rs, 20));
            record.setWeb_date(getDate(rs, 21));
            record.setVar_usage(getString(rs, 22));
            
            // add this FcstPtESPRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the FcstPtESPRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a FcstPtESPRecord object and..
//-----------------------------------------------------------------
    public int insert(FcstPtESPRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO fcstptesp VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getSnow_method());
        setString(insertStatement, 3, record.getHydrol_method());
        setString(insertStatement, 4, record.getReservoir_model());
        setString(insertStatement, 5, record.getUpstream_seg());
        setString(insertStatement, 6, record.getHydraul_method());
        setString(insertStatement, 7, record.getFlowtype());
        setString(insertStatement, 8, record.getFcsttype());
        setString(insertStatement, 9, record.getFrequpd_normal());
        setString(insertStatement, 10, record.getFrequpd_flood());
        setString(insertStatement, 11, record.getFrequpd_drought());
        setString(insertStatement, 12, record.getFcst_horizon());
        setShort(insertStatement, 13, record.getNummonclim());
        setShort(insertStatement, 14, record.getNumdayhyd());
        setShort(insertStatement, 15, record.getNum_elev_zones());
        setString(insertStatement, 16, record.getConsumptive_use());
        setString(insertStatement, 17, record.getChannel_loss());
        setString(insertStatement, 18, record.getPost_processor());
        setDate(insertStatement, 19, record.getImpl_date());
        setDate(insertStatement, 20, record.getExternal_date());
        setDate(insertStatement, 21, record.getWeb_date());
        setString(insertStatement, 22, record.getVar_usage());
        
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
        String deleteStatement = "DELETE FROM fcstptesp " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a FcstPtESPRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(FcstPtESPRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE fcstptesp SET lid = ?, snow_method = ?, hydrol_method = ?, reservoir_model = ?, upstream_seg = ?, hydraul_method = ?, flowtype = ?, fcsttype = ?, frequpd_normal = ?, frequpd_flood = ?, frequpd_drought = ?, fcst_horizon = ?, nummonclim = ?, numdayhyd = ?, num_elev_zones = ?, consumptive_use = ?, channel_loss = ?, post_processor = ?, impl_date = ?, external_date = ?, web_date = ?, var_usage = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setString(updateStatement, 2, record.getSnow_method());
        setString(updateStatement, 3, record.getHydrol_method());
        setString(updateStatement, 4, record.getReservoir_model());
        setString(updateStatement, 5, record.getUpstream_seg());
        setString(updateStatement, 6, record.getHydraul_method());
        setString(updateStatement, 7, record.getFlowtype());
        setString(updateStatement, 8, record.getFcsttype());
        setString(updateStatement, 9, record.getFrequpd_normal());
        setString(updateStatement, 10, record.getFrequpd_flood());
        setString(updateStatement, 11, record.getFrequpd_drought());
        setString(updateStatement, 12, record.getFcst_horizon());
        setShort(updateStatement, 13, record.getNummonclim());
        setShort(updateStatement, 14, record.getNumdayhyd());
        setShort(updateStatement, 15, record.getNum_elev_zones());
        setString(updateStatement, 16, record.getConsumptive_use());
        setString(updateStatement, 17, record.getChannel_loss());
        setString(updateStatement, 18, record.getPost_processor());
        setDate(updateStatement, 19, record.getImpl_date());
        setDate(updateStatement, 20, record.getExternal_date());
        setDate(updateStatement, 21, record.getWeb_date());
        setString(updateStatement, 22, record.getVar_usage());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(FcstPtESPRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM fcstptesp " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a FcstPtESPRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(FcstPtESPRecord oldRecord, FcstPtESPRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE fcstptesp SET lid = ?, snow_method = ?, hydrol_method = ?, reservoir_model = ?, upstream_seg = ?, hydraul_method = ?, flowtype = ?, fcsttype = ?, frequpd_normal = ?, frequpd_flood = ?, frequpd_drought = ?, fcst_horizon = ?, nummonclim = ?, numdayhyd = ?, num_elev_zones = ?, consumptive_use = ?, channel_loss = ?, post_processor = ?, impl_date = ?, external_date = ?, web_date = ?, var_usage = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getSnow_method());
        setString(updateStatement, 3, newRecord.getHydrol_method());
        setString(updateStatement, 4, newRecord.getReservoir_model());
        setString(updateStatement, 5, newRecord.getUpstream_seg());
        setString(updateStatement, 6, newRecord.getHydraul_method());
        setString(updateStatement, 7, newRecord.getFlowtype());
        setString(updateStatement, 8, newRecord.getFcsttype());
        setString(updateStatement, 9, newRecord.getFrequpd_normal());
        setString(updateStatement, 10, newRecord.getFrequpd_flood());
        setString(updateStatement, 11, newRecord.getFrequpd_drought());
        setString(updateStatement, 12, newRecord.getFcst_horizon());
        setShort(updateStatement, 13, newRecord.getNummonclim());
        setShort(updateStatement, 14, newRecord.getNumdayhyd());
        setShort(updateStatement, 15, newRecord.getNum_elev_zones());
        setString(updateStatement, 16, newRecord.getConsumptive_use());
        setString(updateStatement, 17, newRecord.getChannel_loss());
        setString(updateStatement, 18, newRecord.getPost_processor());
        setDate(updateStatement, 19, newRecord.getImpl_date());
        setDate(updateStatement, 20, newRecord.getExternal_date());
        setDate(updateStatement, 21, newRecord.getWeb_date());
        setString(updateStatement, 22, newRecord.getVar_usage());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a FcstPtESPRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(FcstPtESPRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            FcstPtESPRecord oldRecord = (FcstPtESPRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of FcstPtESPTable class
