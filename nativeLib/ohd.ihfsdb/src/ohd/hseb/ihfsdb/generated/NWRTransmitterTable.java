// filename: NWRTransmitterTable.java
// author  : DBGEN
// created : Tue May 31 17:52:25 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              nwrtransmitter table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class NWRTransmitterTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  NWRTransmitterTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public NWRTransmitterTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("nwrtransmitter");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of NWRTransmitterRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        NWRTransmitterRecord record = null;

        // create a List to hold NWRTransmitter Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM nwrtransmitter " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a NWRTransmitterRecord
            // and store its address in oneRecord
            record = new NWRTransmitterRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a NWRTransmitterRecord object

            record.setCall_sign(getString(rs, 1));
            record.setWfo(getString(rs, 2));
            record.setCity(getString(rs, 3));
            record.setCounty(getString(rs, 4));
            record.setState(getString(rs, 5));
            record.setCoverage_area(getString(rs, 6));
            record.setLat(getDouble(rs, 7));
            record.setLon(getDouble(rs, 8));
            record.setTransmit_freq(getDouble(rs, 9));
            record.setTransmit_power(getInt(rs, 10));
            record.setTransmit_prod_code(getString(rs, 11));
            record.setTransmit_countynum(getString(rs, 12));
            record.setUse_transmitter(getString(rs, 13));
            
            // add this NWRTransmitterRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the NWRTransmitterRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of NWRTransmitterRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        NWRTransmitterRecord record = null;

        // create a List to hold NWRTransmitter Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM nwrtransmitter " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a NWRTransmitterRecord
            // and store its address in oneRecord
            record = new NWRTransmitterRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a NWRTransmitterRecord object

            record.setCall_sign(getString(rs, 1));
            record.setWfo(getString(rs, 2));
            record.setCity(getString(rs, 3));
            record.setCounty(getString(rs, 4));
            record.setState(getString(rs, 5));
            record.setCoverage_area(getString(rs, 6));
            record.setLat(getDouble(rs, 7));
            record.setLon(getDouble(rs, 8));
            record.setTransmit_freq(getDouble(rs, 9));
            record.setTransmit_power(getInt(rs, 10));
            record.setTransmit_prod_code(getString(rs, 11));
            record.setTransmit_countynum(getString(rs, 12));
            record.setUse_transmitter(getString(rs, 13));
            
            // add this NWRTransmitterRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the NWRTransmitterRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a NWRTransmitterRecord object and..
//-----------------------------------------------------------------
    public int insert(NWRTransmitterRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO nwrtransmitter VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getCall_sign());
        setString(insertStatement, 2, record.getWfo());
        setString(insertStatement, 3, record.getCity());
        setString(insertStatement, 4, record.getCounty());
        setString(insertStatement, 5, record.getState());
        setString(insertStatement, 6, record.getCoverage_area());
        setDouble(insertStatement, 7, record.getLat());
        setDouble(insertStatement, 8, record.getLon());
        setDouble(insertStatement, 9, record.getTransmit_freq());
        setInt(insertStatement, 10, record.getTransmit_power());
        setString(insertStatement, 11, record.getTransmit_prod_code());
        setString(insertStatement, 12, record.getTransmit_countynum());
        setString(insertStatement, 13, record.getUse_transmitter());
        
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
        String deleteStatement = "DELETE FROM nwrtransmitter " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a NWRTransmitterRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(NWRTransmitterRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE nwrtransmitter SET call_sign = ?, wfo = ?, city = ?, county = ?, state = ?, coverage_area = ?, lat = ?, lon = ?, transmit_freq = ?, transmit_power = ?, transmit_prod_code = ?, transmit_countynum = ?, use_transmitter = ?        " + where );

        setString(updateStatement, 1, record.getCall_sign());
        setString(updateStatement, 2, record.getWfo());
        setString(updateStatement, 3, record.getCity());
        setString(updateStatement, 4, record.getCounty());
        setString(updateStatement, 5, record.getState());
        setString(updateStatement, 6, record.getCoverage_area());
        setDouble(updateStatement, 7, record.getLat());
        setDouble(updateStatement, 8, record.getLon());
        setDouble(updateStatement, 9, record.getTransmit_freq());
        setInt(updateStatement, 10, record.getTransmit_power());
        setString(updateStatement, 11, record.getTransmit_prod_code());
        setString(updateStatement, 12, record.getTransmit_countynum());
        setString(updateStatement, 13, record.getUse_transmitter());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(NWRTransmitterRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM nwrtransmitter " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a NWRTransmitterRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(NWRTransmitterRecord oldRecord, NWRTransmitterRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE nwrtransmitter SET call_sign = ?, wfo = ?, city = ?, county = ?, state = ?, coverage_area = ?, lat = ?, lon = ?, transmit_freq = ?, transmit_power = ?, transmit_prod_code = ?, transmit_countynum = ?, use_transmitter = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getCall_sign());
        setString(updateStatement, 2, newRecord.getWfo());
        setString(updateStatement, 3, newRecord.getCity());
        setString(updateStatement, 4, newRecord.getCounty());
        setString(updateStatement, 5, newRecord.getState());
        setString(updateStatement, 6, newRecord.getCoverage_area());
        setDouble(updateStatement, 7, newRecord.getLat());
        setDouble(updateStatement, 8, newRecord.getLon());
        setDouble(updateStatement, 9, newRecord.getTransmit_freq());
        setInt(updateStatement, 10, newRecord.getTransmit_power());
        setString(updateStatement, 11, newRecord.getTransmit_prod_code());
        setString(updateStatement, 12, newRecord.getTransmit_countynum());
        setString(updateStatement, 13, newRecord.getUse_transmitter());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a NWRTransmitterRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(NWRTransmitterRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            NWRTransmitterRecord oldRecord = (NWRTransmitterRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of NWRTransmitterTable class
