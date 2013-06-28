// filename: SacSmaStateTable.java
// author  : DBGEN
// created : Tue May 31 17:52:28 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              sacsmastate table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class SacSmaStateTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  SacSmaStateTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public SacSmaStateTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("sacsmastate");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of SacSmaStateRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        SacSmaStateRecord record = null;

        // create a List to hold SacSmaState Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM sacsmastate " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a SacSmaStateRecord
            // and store its address in oneRecord
            record = new SacSmaStateRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a SacSmaStateRecord object

            record.setBasin_id(getString(rs, 1));
            record.setSource(getString(rs, 2));
            record.setValidtime(getTimeStamp(rs, 3));
            record.setBasistime(getTimeStamp(rs, 4));
            record.setPostingtime(getTimeStamp(rs, 5));
            record.setUztwc(getDouble(rs, 6));
            record.setUzfwc(getDouble(rs, 7));
            record.setLztwc(getDouble(rs, 8));
            record.setLzfsc(getDouble(rs, 9));
            record.setLzfpc(getDouble(rs, 10));
            record.setAdimc(getDouble(rs, 11));
            
            // add this SacSmaStateRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the SacSmaStateRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of SacSmaStateRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        SacSmaStateRecord record = null;

        // create a List to hold SacSmaState Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM sacsmastate " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a SacSmaStateRecord
            // and store its address in oneRecord
            record = new SacSmaStateRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a SacSmaStateRecord object

            record.setBasin_id(getString(rs, 1));
            record.setSource(getString(rs, 2));
            record.setValidtime(getTimeStamp(rs, 3));
            record.setBasistime(getTimeStamp(rs, 4));
            record.setPostingtime(getTimeStamp(rs, 5));
            record.setUztwc(getDouble(rs, 6));
            record.setUzfwc(getDouble(rs, 7));
            record.setLztwc(getDouble(rs, 8));
            record.setLzfsc(getDouble(rs, 9));
            record.setLzfpc(getDouble(rs, 10));
            record.setAdimc(getDouble(rs, 11));
            
            // add this SacSmaStateRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the SacSmaStateRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a SacSmaStateRecord object and..
//-----------------------------------------------------------------
    public int insert(SacSmaStateRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO sacsmastate VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getBasin_id());
        setString(insertStatement, 2, record.getSource());
        setTimeStamp(insertStatement, 3, record.getValidtime());
        setTimeStamp(insertStatement, 4, record.getBasistime());
        setTimeStamp(insertStatement, 5, record.getPostingtime());
        setDouble(insertStatement, 6, record.getUztwc());
        setDouble(insertStatement, 7, record.getUzfwc());
        setDouble(insertStatement, 8, record.getLztwc());
        setDouble(insertStatement, 9, record.getLzfsc());
        setDouble(insertStatement, 10, record.getLzfpc());
        setDouble(insertStatement, 11, record.getAdimc());
        
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
        String deleteStatement = "DELETE FROM sacsmastate " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a SacSmaStateRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(SacSmaStateRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE sacsmastate SET basin_id = ?, source = ?, validtime = ?, basistime = ?, postingtime = ?, uztwc = ?, uzfwc = ?, lztwc = ?, lzfsc = ?, lzfpc = ?, adimc = ?        " + where );

        setString(updateStatement, 1, record.getBasin_id());
        setString(updateStatement, 2, record.getSource());
        setTimeStamp(updateStatement, 3, record.getValidtime());
        setTimeStamp(updateStatement, 4, record.getBasistime());
        setTimeStamp(updateStatement, 5, record.getPostingtime());
        setDouble(updateStatement, 6, record.getUztwc());
        setDouble(updateStatement, 7, record.getUzfwc());
        setDouble(updateStatement, 8, record.getLztwc());
        setDouble(updateStatement, 9, record.getLzfsc());
        setDouble(updateStatement, 10, record.getLzfpc());
        setDouble(updateStatement, 11, record.getAdimc());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(SacSmaStateRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM sacsmastate " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a SacSmaStateRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(SacSmaStateRecord oldRecord, SacSmaStateRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE sacsmastate SET basin_id = ?, source = ?, validtime = ?, basistime = ?, postingtime = ?, uztwc = ?, uzfwc = ?, lztwc = ?, lzfsc = ?, lzfpc = ?, adimc = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getBasin_id());
        setString(updateStatement, 2, newRecord.getSource());
        setTimeStamp(updateStatement, 3, newRecord.getValidtime());
        setTimeStamp(updateStatement, 4, newRecord.getBasistime());
        setTimeStamp(updateStatement, 5, newRecord.getPostingtime());
        setDouble(updateStatement, 6, newRecord.getUztwc());
        setDouble(updateStatement, 7, newRecord.getUzfwc());
        setDouble(updateStatement, 8, newRecord.getLztwc());
        setDouble(updateStatement, 9, newRecord.getLzfsc());
        setDouble(updateStatement, 10, newRecord.getLzfpc());
        setDouble(updateStatement, 11, newRecord.getAdimc());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a SacSmaStateRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(SacSmaStateRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            SacSmaStateRecord oldRecord = (SacSmaStateRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of SacSmaStateTable class
