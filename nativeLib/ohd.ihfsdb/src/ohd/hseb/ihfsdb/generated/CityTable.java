// filename: CityTable.java
// author  : DBGEN
// created : Tue May 31 17:52:19 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              city table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class CityTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  CityTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public CityTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("city");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of CityRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        CityRecord record = null;

        // create a List to hold City Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM city " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a CityRecord
            // and store its address in oneRecord
            record = new CityRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a CityRecord object

            record.setName(getString(rs, 1));
            record.setState(getString(rs, 2));
            record.setLat(getDouble(rs, 3));
            record.setLon(getDouble(rs, 4));
            record.setDisp_precedence(getInt(rs, 5));
            record.setPopulation(getInt(rs, 6));
            
            // add this CityRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the CityRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of CityRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        CityRecord record = null;

        // create a List to hold City Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM city " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a CityRecord
            // and store its address in oneRecord
            record = new CityRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a CityRecord object

            record.setName(getString(rs, 1));
            record.setState(getString(rs, 2));
            record.setLat(getDouble(rs, 3));
            record.setLon(getDouble(rs, 4));
            record.setDisp_precedence(getInt(rs, 5));
            record.setPopulation(getInt(rs, 6));
            
            // add this CityRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the CityRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a CityRecord object and..
//-----------------------------------------------------------------
    public int insert(CityRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO city VALUES (?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getName());
        setString(insertStatement, 2, record.getState());
        setDouble(insertStatement, 3, record.getLat());
        setDouble(insertStatement, 4, record.getLon());
        setInt(insertStatement, 5, record.getDisp_precedence());
        setInt(insertStatement, 6, record.getPopulation());
        
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
        String deleteStatement = "DELETE FROM city " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a CityRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(CityRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE city SET name = ?, state = ?, lat = ?, lon = ?, disp_precedence = ?, population = ?        " + where );

        setString(updateStatement, 1, record.getName());
        setString(updateStatement, 2, record.getState());
        setDouble(updateStatement, 3, record.getLat());
        setDouble(updateStatement, 4, record.getLon());
        setInt(updateStatement, 5, record.getDisp_precedence());
        setInt(updateStatement, 6, record.getPopulation());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

} // end of CityTable class
