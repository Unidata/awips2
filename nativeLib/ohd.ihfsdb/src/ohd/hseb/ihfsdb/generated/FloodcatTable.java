// filename: FloodcatTable.java
// author  : DBGEN
// created : Tue May 31 17:52:22 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              floodcat table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class FloodcatTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  FloodcatTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public FloodcatTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("floodcat");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of FloodcatRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        FloodcatRecord record = null;

        // create a List to hold Floodcat Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM floodcat " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a FloodcatRecord
            // and store its address in oneRecord
            record = new FloodcatRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a FloodcatRecord object

            record.setLid(getString(rs, 1));
            record.setMinor_stage(getDouble(rs, 2));
            record.setModerate_stage(getDouble(rs, 3));
            record.setMajor_stage(getDouble(rs, 4));
            record.setMinor_flow(getDouble(rs, 5));
            record.setModerate_flow(getDouble(rs, 6));
            record.setMajor_flow(getDouble(rs, 7));
            
            // add this FloodcatRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the FloodcatRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of FloodcatRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        FloodcatRecord record = null;

        // create a List to hold Floodcat Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM floodcat " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a FloodcatRecord
            // and store its address in oneRecord
            record = new FloodcatRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a FloodcatRecord object

            record.setLid(getString(rs, 1));
            record.setMinor_stage(getDouble(rs, 2));
            record.setModerate_stage(getDouble(rs, 3));
            record.setMajor_stage(getDouble(rs, 4));
            record.setMinor_flow(getDouble(rs, 5));
            record.setModerate_flow(getDouble(rs, 6));
            record.setMajor_flow(getDouble(rs, 7));
            
            // add this FloodcatRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the FloodcatRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a FloodcatRecord object and..
//-----------------------------------------------------------------
    public int insert(FloodcatRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO floodcat VALUES (?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setDouble(insertStatement, 2, record.getMinor_stage());
        setDouble(insertStatement, 3, record.getModerate_stage());
        setDouble(insertStatement, 4, record.getMajor_stage());
        setDouble(insertStatement, 5, record.getMinor_flow());
        setDouble(insertStatement, 6, record.getModerate_flow());
        setDouble(insertStatement, 7, record.getMajor_flow());
        
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
        String deleteStatement = "DELETE FROM floodcat " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a FloodcatRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(FloodcatRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE floodcat SET lid = ?, minor_stage = ?, moderate_stage = ?, major_stage = ?, minor_flow = ?, moderate_flow = ?, major_flow = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setDouble(updateStatement, 2, record.getMinor_stage());
        setDouble(updateStatement, 3, record.getModerate_stage());
        setDouble(updateStatement, 4, record.getMajor_stage());
        setDouble(updateStatement, 5, record.getMinor_flow());
        setDouble(updateStatement, 6, record.getModerate_flow());
        setDouble(updateStatement, 7, record.getMajor_flow());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(FloodcatRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM floodcat " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a FloodcatRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(FloodcatRecord oldRecord, FloodcatRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE floodcat SET lid = ?, minor_stage = ?, moderate_stage = ?, major_stage = ?, minor_flow = ?, moderate_flow = ?, major_flow = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setDouble(updateStatement, 2, newRecord.getMinor_stage());
        setDouble(updateStatement, 3, newRecord.getModerate_stage());
        setDouble(updateStatement, 4, newRecord.getMajor_stage());
        setDouble(updateStatement, 5, newRecord.getMinor_flow());
        setDouble(updateStatement, 6, newRecord.getModerate_flow());
        setDouble(updateStatement, 7, newRecord.getMajor_flow());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a FloodcatRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(FloodcatRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            FloodcatRecord oldRecord = (FloodcatRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of FloodcatTable class
