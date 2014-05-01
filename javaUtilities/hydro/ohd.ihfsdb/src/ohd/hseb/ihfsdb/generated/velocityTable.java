// filename: velocityTable.java
// author  : DBGEN
// created : Tue May 31 17:52:29 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              velocity table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class velocityTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  velocityTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public velocityTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("velocity");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of velocityRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        velocityRecord record = null;

        // create a List to hold velocity Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM velocity " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a velocityRecord
            // and store its address in oneRecord
            record = new velocityRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a velocityRecord object

            record.setMosaicid(getString(rs, 1));
            record.setCreatetime(getTimeStamp(rs, 2));
            record.setUmean(getDouble(rs, 3));
            record.setVmean(getDouble(rs, 4));
            record.setCount(getShort(rs, 5));
            
            // add this velocityRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the velocityRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of velocityRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        velocityRecord record = null;

        // create a List to hold velocity Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM velocity " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a velocityRecord
            // and store its address in oneRecord
            record = new velocityRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a velocityRecord object

            record.setMosaicid(getString(rs, 1));
            record.setCreatetime(getTimeStamp(rs, 2));
            record.setUmean(getDouble(rs, 3));
            record.setVmean(getDouble(rs, 4));
            record.setCount(getShort(rs, 5));
            
            // add this velocityRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the velocityRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a velocityRecord object and..
//-----------------------------------------------------------------
    public int insert(velocityRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO velocity VALUES (?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getMosaicid());
        setTimeStamp(insertStatement, 2, record.getCreatetime());
        setDouble(insertStatement, 3, record.getUmean());
        setDouble(insertStatement, 4, record.getVmean());
        setShort(insertStatement, 5, record.getCount());
        
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
        String deleteStatement = "DELETE FROM velocity " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a velocityRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(velocityRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE velocity SET mosaicid = ?, createtime = ?, umean = ?, vmean = ?, count = ?        " + where );

        setString(updateStatement, 1, record.getMosaicid());
        setTimeStamp(updateStatement, 2, record.getCreatetime());
        setDouble(updateStatement, 3, record.getUmean());
        setDouble(updateStatement, 4, record.getVmean());
        setShort(updateStatement, 5, record.getCount());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(velocityRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM velocity " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a velocityRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(velocityRecord oldRecord, velocityRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE velocity SET mosaicid = ?, createtime = ?, umean = ?, vmean = ?, count = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getMosaicid());
        setTimeStamp(updateStatement, 2, newRecord.getCreatetime());
        setDouble(updateStatement, 3, newRecord.getUmean());
        setDouble(updateStatement, 4, newRecord.getVmean());
        setShort(updateStatement, 5, newRecord.getCount());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a velocityRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(velocityRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            velocityRecord oldRecord = (velocityRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of velocityTable class
