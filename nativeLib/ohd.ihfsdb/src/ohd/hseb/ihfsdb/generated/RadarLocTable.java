// filename: RadarLocTable.java
// author  : DBGEN
// created : Tue May 31 17:52:26 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              radarloc table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class RadarLocTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  RadarLocTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public RadarLocTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("radarloc");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of RadarLocRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        RadarLocRecord record = null;

        // create a List to hold RadarLoc Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM radarloc " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RadarLocRecord
            // and store its address in oneRecord
            record = new RadarLocRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RadarLocRecord object

            record.setRadid(getString(rs, 1));
            record.setName(getString(rs, 2));
            record.setRadid_prefix(getString(rs, 3));
            record.setRadar_num(getShort(rs, 4));
            record.setState(getString(rs, 5));
            record.setLat(getDouble(rs, 6));
            record.setLon(getDouble(rs, 7));
            record.setElev(getDouble(rs, 8));
            record.setTower_ht(getDouble(rs, 9));
            record.setUse_radar(getString(rs, 10));
            record.setOffice_id(getString(rs, 11));
            
            // add this RadarLocRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the RadarLocRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of RadarLocRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        RadarLocRecord record = null;

        // create a List to hold RadarLoc Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM radarloc " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RadarLocRecord
            // and store its address in oneRecord
            record = new RadarLocRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RadarLocRecord object

            record.setRadid(getString(rs, 1));
            record.setName(getString(rs, 2));
            record.setRadid_prefix(getString(rs, 3));
            record.setRadar_num(getShort(rs, 4));
            record.setState(getString(rs, 5));
            record.setLat(getDouble(rs, 6));
            record.setLon(getDouble(rs, 7));
            record.setElev(getDouble(rs, 8));
            record.setTower_ht(getDouble(rs, 9));
            record.setUse_radar(getString(rs, 10));
            record.setOffice_id(getString(rs, 11));
            
            // add this RadarLocRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the RadarLocRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a RadarLocRecord object and..
//-----------------------------------------------------------------
    public int insert(RadarLocRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO radarloc VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getRadid());
        setString(insertStatement, 2, record.getName());
        setString(insertStatement, 3, record.getRadid_prefix());
        setShort(insertStatement, 4, record.getRadar_num());
        setString(insertStatement, 5, record.getState());
        setDouble(insertStatement, 6, record.getLat());
        setDouble(insertStatement, 7, record.getLon());
        setDouble(insertStatement, 8, record.getElev());
        setDouble(insertStatement, 9, record.getTower_ht());
        setString(insertStatement, 10, record.getUse_radar());
        setString(insertStatement, 11, record.getOffice_id());
        
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
        String deleteStatement = "DELETE FROM radarloc " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RadarLocRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RadarLocRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE radarloc SET radid = ?, name = ?, radid_prefix = ?, radar_num = ?, state = ?, lat = ?, lon = ?, elev = ?, tower_ht = ?, use_radar = ?, office_id = ?        " + where );

        setString(updateStatement, 1, record.getRadid());
        setString(updateStatement, 2, record.getName());
        setString(updateStatement, 3, record.getRadid_prefix());
        setShort(updateStatement, 4, record.getRadar_num());
        setString(updateStatement, 5, record.getState());
        setDouble(updateStatement, 6, record.getLat());
        setDouble(updateStatement, 7, record.getLon());
        setDouble(updateStatement, 8, record.getElev());
        setDouble(updateStatement, 9, record.getTower_ht());
        setString(updateStatement, 10, record.getUse_radar());
        setString(updateStatement, 11, record.getOffice_id());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(RadarLocRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM radarloc " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RadarLocRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RadarLocRecord oldRecord, RadarLocRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE radarloc SET radid = ?, name = ?, radid_prefix = ?, radar_num = ?, state = ?, lat = ?, lon = ?, elev = ?, tower_ht = ?, use_radar = ?, office_id = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getRadid());
        setString(updateStatement, 2, newRecord.getName());
        setString(updateStatement, 3, newRecord.getRadid_prefix());
        setShort(updateStatement, 4, newRecord.getRadar_num());
        setString(updateStatement, 5, newRecord.getState());
        setDouble(updateStatement, 6, newRecord.getLat());
        setDouble(updateStatement, 7, newRecord.getLon());
        setDouble(updateStatement, 8, newRecord.getElev());
        setDouble(updateStatement, 9, newRecord.getTower_ht());
        setString(updateStatement, 10, newRecord.getUse_radar());
        setString(updateStatement, 11, newRecord.getOffice_id());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a RadarLocRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(RadarLocRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            RadarLocRecord oldRecord = (RadarLocRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of RadarLocTable class
