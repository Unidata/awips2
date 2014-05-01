// filename: PointDataPresetsTable.java
// author  : DBGEN
// created : Tue May 31 17:52:25 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              pointdatapresets table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class PointDataPresetsTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  PointDataPresetsTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public PointDataPresetsTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("pointdatapresets");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of PointDataPresetsRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        PointDataPresetsRecord record = null;

        // create a List to hold PointDataPresets Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM pointdatapresets " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a PointDataPresetsRecord
            // and store its address in oneRecord
            record = new PointDataPresetsRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a PointDataPresetsRecord object

            record.setPreset_id(getString(rs, 1));
            record.setDescr(getString(rs, 2));
            record.setPreset_rank(getShort(rs, 3));
            record.setPreset_string(getString(rs, 4));
            
            // add this PointDataPresetsRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the PointDataPresetsRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of PointDataPresetsRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        PointDataPresetsRecord record = null;

        // create a List to hold PointDataPresets Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM pointdatapresets " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a PointDataPresetsRecord
            // and store its address in oneRecord
            record = new PointDataPresetsRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a PointDataPresetsRecord object

            record.setPreset_id(getString(rs, 1));
            record.setDescr(getString(rs, 2));
            record.setPreset_rank(getShort(rs, 3));
            record.setPreset_string(getString(rs, 4));
            
            // add this PointDataPresetsRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the PointDataPresetsRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a PointDataPresetsRecord object and..
//-----------------------------------------------------------------
    public int insert(PointDataPresetsRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO pointdatapresets VALUES (?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getPreset_id());
        setString(insertStatement, 2, record.getDescr());
        setShort(insertStatement, 3, record.getPreset_rank());
        setString(insertStatement, 4, record.getPreset_string());
        
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
        String deleteStatement = "DELETE FROM pointdatapresets " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a PointDataPresetsRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(PointDataPresetsRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE pointdatapresets SET preset_id = ?, descr = ?, preset_rank = ?, preset_string = ?        " + where );

        setString(updateStatement, 1, record.getPreset_id());
        setString(updateStatement, 2, record.getDescr());
        setShort(updateStatement, 3, record.getPreset_rank());
        setString(updateStatement, 4, record.getPreset_string());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(PointDataPresetsRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM pointdatapresets " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a PointDataPresetsRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(PointDataPresetsRecord oldRecord, PointDataPresetsRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE pointdatapresets SET preset_id = ?, descr = ?, preset_rank = ?, preset_string = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getPreset_id());
        setString(updateStatement, 2, newRecord.getDescr());
        setShort(updateStatement, 3, newRecord.getPreset_rank());
        setString(updateStatement, 4, newRecord.getPreset_string());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a PointDataPresetsRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(PointDataPresetsRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            PointDataPresetsRecord oldRecord = (PointDataPresetsRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of PointDataPresetsTable class
