// filename: LightningTable.java
// author  : DBGEN
// created : Tue May 31 17:52:24 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              lightning table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class LightningTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  LightningTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public LightningTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("lightning");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of LightningRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        LightningRecord record = null;

        // create a List to hold Lightning Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM lightning " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a LightningRecord
            // and store its address in oneRecord
            record = new LightningRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a LightningRecord object

            record.setX_hgrid(getShort(rs, 1));
            record.setY_hgrid(getShort(rs, 2));
            record.setObstime(getTimeStamp(rs, 3));
            record.setNo_of_strike(getShort(rs, 4));
            
            // add this LightningRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the LightningRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of LightningRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        LightningRecord record = null;

        // create a List to hold Lightning Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM lightning " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a LightningRecord
            // and store its address in oneRecord
            record = new LightningRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a LightningRecord object

            record.setX_hgrid(getShort(rs, 1));
            record.setY_hgrid(getShort(rs, 2));
            record.setObstime(getTimeStamp(rs, 3));
            record.setNo_of_strike(getShort(rs, 4));
            
            // add this LightningRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the LightningRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a LightningRecord object and..
//-----------------------------------------------------------------
    public int insert(LightningRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO lightning VALUES (?, ?, ?, ?        )");

        setShort(insertStatement, 1, record.getX_hgrid());
        setShort(insertStatement, 2, record.getY_hgrid());
        setTimeStamp(insertStatement, 3, record.getObstime());
        setShort(insertStatement, 4, record.getNo_of_strike());
        
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
        String deleteStatement = "DELETE FROM lightning " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a LightningRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(LightningRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE lightning SET x_hgrid = ?, y_hgrid = ?, obstime = ?, no_of_strike = ?        " + where );

        setShort(updateStatement, 1, record.getX_hgrid());
        setShort(updateStatement, 2, record.getY_hgrid());
        setTimeStamp(updateStatement, 3, record.getObstime());
        setShort(updateStatement, 4, record.getNo_of_strike());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(LightningRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM lightning " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a LightningRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(LightningRecord oldRecord, LightningRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE lightning SET x_hgrid = ?, y_hgrid = ?, obstime = ?, no_of_strike = ?        " + oldRecord.getWhereString() );

        setShort(updateStatement, 1, newRecord.getX_hgrid());
        setShort(updateStatement, 2, newRecord.getY_hgrid());
        setTimeStamp(updateStatement, 3, newRecord.getObstime());
        setShort(updateStatement, 4, newRecord.getNo_of_strike());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a LightningRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(LightningRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            LightningRecord oldRecord = (LightningRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of LightningTable class
