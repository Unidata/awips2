// filename: RWPrefsTable.java
// author  : DBGEN
// created : Tue May 31 17:52:28 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              rwprefs table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class RWPrefsTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  RWPrefsTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public RWPrefsTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("rwprefs");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of RWPrefsRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        RWPrefsRecord record = null;

        // create a List to hold RWPrefs Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM rwprefs " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RWPrefsRecord
            // and store its address in oneRecord
            record = new RWPrefsRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RWPrefsRecord object

            record.setUserid(getString(rs, 1));
            record.setState_overlay(getString(rs, 2));
            record.setCity_overlay(getString(rs, 3));
            record.setCounty_overlay(getString(rs, 4));
            record.setRiver_overlay(getString(rs, 5));
            record.setBasin_overlay(getString(rs, 6));
            record.setRadar_overlay(getString(rs, 7));
            record.setNum_hours_wind(getShort(rs, 8));
            record.setDef_display_type(getString(rs, 9));
            
            // add this RWPrefsRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the RWPrefsRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of RWPrefsRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        RWPrefsRecord record = null;

        // create a List to hold RWPrefs Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM rwprefs " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RWPrefsRecord
            // and store its address in oneRecord
            record = new RWPrefsRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RWPrefsRecord object

            record.setUserid(getString(rs, 1));
            record.setState_overlay(getString(rs, 2));
            record.setCity_overlay(getString(rs, 3));
            record.setCounty_overlay(getString(rs, 4));
            record.setRiver_overlay(getString(rs, 5));
            record.setBasin_overlay(getString(rs, 6));
            record.setRadar_overlay(getString(rs, 7));
            record.setNum_hours_wind(getShort(rs, 8));
            record.setDef_display_type(getString(rs, 9));
            
            // add this RWPrefsRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the RWPrefsRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a RWPrefsRecord object and..
//-----------------------------------------------------------------
    public int insert(RWPrefsRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO rwprefs VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getUserid());
        setString(insertStatement, 2, record.getState_overlay());
        setString(insertStatement, 3, record.getCity_overlay());
        setString(insertStatement, 4, record.getCounty_overlay());
        setString(insertStatement, 5, record.getRiver_overlay());
        setString(insertStatement, 6, record.getBasin_overlay());
        setString(insertStatement, 7, record.getRadar_overlay());
        setShort(insertStatement, 8, record.getNum_hours_wind());
        setString(insertStatement, 9, record.getDef_display_type());
        
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
        String deleteStatement = "DELETE FROM rwprefs " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RWPrefsRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RWPrefsRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE rwprefs SET userid = ?, state_overlay = ?, city_overlay = ?, county_overlay = ?, river_overlay = ?, basin_overlay = ?, radar_overlay = ?, num_hours_wind = ?, def_display_type = ?        " + where );

        setString(updateStatement, 1, record.getUserid());
        setString(updateStatement, 2, record.getState_overlay());
        setString(updateStatement, 3, record.getCity_overlay());
        setString(updateStatement, 4, record.getCounty_overlay());
        setString(updateStatement, 5, record.getRiver_overlay());
        setString(updateStatement, 6, record.getBasin_overlay());
        setString(updateStatement, 7, record.getRadar_overlay());
        setShort(updateStatement, 8, record.getNum_hours_wind());
        setString(updateStatement, 9, record.getDef_display_type());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(RWPrefsRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM rwprefs " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RWPrefsRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RWPrefsRecord oldRecord, RWPrefsRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE rwprefs SET userid = ?, state_overlay = ?, city_overlay = ?, county_overlay = ?, river_overlay = ?, basin_overlay = ?, radar_overlay = ?, num_hours_wind = ?, def_display_type = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getUserid());
        setString(updateStatement, 2, newRecord.getState_overlay());
        setString(updateStatement, 3, newRecord.getCity_overlay());
        setString(updateStatement, 4, newRecord.getCounty_overlay());
        setString(updateStatement, 5, newRecord.getRiver_overlay());
        setString(updateStatement, 6, newRecord.getBasin_overlay());
        setString(updateStatement, 7, newRecord.getRadar_overlay());
        setShort(updateStatement, 8, newRecord.getNum_hours_wind());
        setString(updateStatement, 9, newRecord.getDef_display_type());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a RWPrefsRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(RWPrefsRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            RWPrefsRecord oldRecord = (RWPrefsRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of RWPrefsTable class
