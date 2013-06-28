// filename: ColorValueTable.java
// author  : DBGEN
// created : Tue May 31 17:52:19 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              colorvalue table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class ColorValueTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  ColorValueTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public ColorValueTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("colorvalue");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of ColorValueRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        ColorValueRecord record = null;

        // create a List to hold ColorValue Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM colorvalue " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a ColorValueRecord
            // and store its address in oneRecord
            record = new ColorValueRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a ColorValueRecord object

            record.setUserid(getString(rs, 1));
            record.setApplication_name(getString(rs, 2));
            record.setColor_use_name(getString(rs, 3));
            record.setDuration(getInt(rs, 4));
            record.setThreshold_value(getDouble(rs, 5));
            record.setThreshold_unit(getString(rs, 6));
            record.setColor_name(getString(rs, 7));
            
            // add this ColorValueRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the ColorValueRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of ColorValueRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        ColorValueRecord record = null;

        // create a List to hold ColorValue Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM colorvalue " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a ColorValueRecord
            // and store its address in oneRecord
            record = new ColorValueRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a ColorValueRecord object

            record.setUserid(getString(rs, 1));
            record.setApplication_name(getString(rs, 2));
            record.setColor_use_name(getString(rs, 3));
            record.setDuration(getInt(rs, 4));
            record.setThreshold_value(getDouble(rs, 5));
            record.setThreshold_unit(getString(rs, 6));
            record.setColor_name(getString(rs, 7));
            
            // add this ColorValueRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the ColorValueRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a ColorValueRecord object and..
//-----------------------------------------------------------------
    public int insert(ColorValueRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO colorvalue VALUES (?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getUserid());
        setString(insertStatement, 2, record.getApplication_name());
        setString(insertStatement, 3, record.getColor_use_name());
        setInt(insertStatement, 4, record.getDuration());
        setDouble(insertStatement, 5, record.getThreshold_value());
        setString(insertStatement, 6, record.getThreshold_unit());
        setString(insertStatement, 7, record.getColor_name());
        
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
        String deleteStatement = "DELETE FROM colorvalue " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a ColorValueRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(ColorValueRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE colorvalue SET userid = ?, application_name = ?, color_use_name = ?, duration = ?, threshold_value = ?, threshold_unit = ?, color_name = ?        " + where );

        setString(updateStatement, 1, record.getUserid());
        setString(updateStatement, 2, record.getApplication_name());
        setString(updateStatement, 3, record.getColor_use_name());
        setInt(updateStatement, 4, record.getDuration());
        setDouble(updateStatement, 5, record.getThreshold_value());
        setString(updateStatement, 6, record.getThreshold_unit());
        setString(updateStatement, 7, record.getColor_name());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(ColorValueRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM colorvalue " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a ColorValueRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(ColorValueRecord oldRecord, ColorValueRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE colorvalue SET userid = ?, application_name = ?, color_use_name = ?, duration = ?, threshold_value = ?, threshold_unit = ?, color_name = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getUserid());
        setString(updateStatement, 2, newRecord.getApplication_name());
        setString(updateStatement, 3, newRecord.getColor_use_name());
        setInt(updateStatement, 4, newRecord.getDuration());
        setDouble(updateStatement, 5, newRecord.getThreshold_value());
        setString(updateStatement, 6, newRecord.getThreshold_unit());
        setString(updateStatement, 7, newRecord.getColor_name());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a ColorValueRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(ColorValueRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            ColorValueRecord oldRecord = (ColorValueRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of ColorValueTable class
