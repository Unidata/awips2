// filename: AdjustFactorTable.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              adjustfactor table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class AdjustFactorTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  AdjustFactorTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public AdjustFactorTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("adjustfactor");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of AdjustFactorRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        AdjustFactorRecord record = null;

        // create a List to hold AdjustFactor Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM adjustfactor " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a AdjustFactorRecord
            // and store its address in oneRecord
            record = new AdjustFactorRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a AdjustFactorRecord object

            record.setLid(getString(rs, 1));
            record.setPe(getString(rs, 2));
            record.setDur(getShort(rs, 3));
            record.setTs(getString(rs, 4));
            record.setExtremum(getString(rs, 5));
            record.setDivisor(getDouble(rs, 6));
            record.setBase(getDouble(rs, 7));
            record.setMultiplier(getDouble(rs, 8));
            record.setAdder(getDouble(rs, 9));
            
            // add this AdjustFactorRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the AdjustFactorRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of AdjustFactorRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        AdjustFactorRecord record = null;

        // create a List to hold AdjustFactor Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM adjustfactor " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a AdjustFactorRecord
            // and store its address in oneRecord
            record = new AdjustFactorRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a AdjustFactorRecord object

            record.setLid(getString(rs, 1));
            record.setPe(getString(rs, 2));
            record.setDur(getShort(rs, 3));
            record.setTs(getString(rs, 4));
            record.setExtremum(getString(rs, 5));
            record.setDivisor(getDouble(rs, 6));
            record.setBase(getDouble(rs, 7));
            record.setMultiplier(getDouble(rs, 8));
            record.setAdder(getDouble(rs, 9));
            
            // add this AdjustFactorRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the AdjustFactorRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a AdjustFactorRecord object and..
//-----------------------------------------------------------------
    public int insert(AdjustFactorRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO adjustfactor VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getPe());
        setShort(insertStatement, 3, record.getDur());
        setString(insertStatement, 4, record.getTs());
        setString(insertStatement, 5, record.getExtremum());
        setDouble(insertStatement, 6, record.getDivisor());
        setDouble(insertStatement, 7, record.getBase());
        setDouble(insertStatement, 8, record.getMultiplier());
        setDouble(insertStatement, 9, record.getAdder());
        
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
        String deleteStatement = "DELETE FROM adjustfactor " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a AdjustFactorRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(AdjustFactorRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE adjustfactor SET lid = ?, pe = ?, dur = ?, ts = ?, extremum = ?, divisor = ?, base = ?, multiplier = ?, adder = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setString(updateStatement, 2, record.getPe());
        setShort(updateStatement, 3, record.getDur());
        setString(updateStatement, 4, record.getTs());
        setString(updateStatement, 5, record.getExtremum());
        setDouble(updateStatement, 6, record.getDivisor());
        setDouble(updateStatement, 7, record.getBase());
        setDouble(updateStatement, 8, record.getMultiplier());
        setDouble(updateStatement, 9, record.getAdder());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(AdjustFactorRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM adjustfactor " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a AdjustFactorRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(AdjustFactorRecord oldRecord, AdjustFactorRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE adjustfactor SET lid = ?, pe = ?, dur = ?, ts = ?, extremum = ?, divisor = ?, base = ?, multiplier = ?, adder = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getPe());
        setShort(updateStatement, 3, newRecord.getDur());
        setString(updateStatement, 4, newRecord.getTs());
        setString(updateStatement, 5, newRecord.getExtremum());
        setDouble(updateStatement, 6, newRecord.getDivisor());
        setDouble(updateStatement, 7, newRecord.getBase());
        setDouble(updateStatement, 8, newRecord.getMultiplier());
        setDouble(updateStatement, 9, newRecord.getAdder());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a AdjustFactorRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(AdjustFactorRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            AdjustFactorRecord oldRecord = (AdjustFactorRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of AdjustFactorTable class
