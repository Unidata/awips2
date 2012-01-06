// filename: CrestTable.java
// author  : DBGEN
// created : Tue May 31 17:52:20 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              crest table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class CrestTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  CrestTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public CrestTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("crest");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of CrestRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        CrestRecord record = null;

        // create a List to hold Crest Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM crest " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a CrestRecord
            // and store its address in oneRecord
            record = new CrestRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a CrestRecord object

            record.setLid(getString(rs, 1));
            record.setDatcrst(getDate(rs, 2));
            record.setCremark(getString(rs, 3));
            record.setHw(getString(rs, 4));
            record.setJam(getString(rs, 5));
            record.setOlddatum(getString(rs, 6));
            record.setQ(getInt(rs, 7));
            record.setStage(getDouble(rs, 8));
            record.setSuppress(getString(rs, 9));
            record.setTimcrst(getString(rs, 10));
            record.setPrelim(getString(rs, 11));
            
            // add this CrestRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the CrestRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of CrestRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        CrestRecord record = null;

        // create a List to hold Crest Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM crest " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a CrestRecord
            // and store its address in oneRecord
            record = new CrestRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a CrestRecord object

            record.setLid(getString(rs, 1));
            record.setDatcrst(getDate(rs, 2));
            record.setCremark(getString(rs, 3));
            record.setHw(getString(rs, 4));
            record.setJam(getString(rs, 5));
            record.setOlddatum(getString(rs, 6));
            record.setQ(getInt(rs, 7));
            record.setStage(getDouble(rs, 8));
            record.setSuppress(getString(rs, 9));
            record.setTimcrst(getString(rs, 10));
            record.setPrelim(getString(rs, 11));
            
            // add this CrestRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the CrestRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a CrestRecord object and..
//-----------------------------------------------------------------
    public int insert(CrestRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO crest VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setDate(insertStatement, 2, record.getDatcrst());
        setString(insertStatement, 3, record.getCremark());
        setString(insertStatement, 4, record.getHw());
        setString(insertStatement, 5, record.getJam());
        setString(insertStatement, 6, record.getOlddatum());
        setInt(insertStatement, 7, record.getQ());
        setDouble(insertStatement, 8, record.getStage());
        setString(insertStatement, 9, record.getSuppress());
        setString(insertStatement, 10, record.getTimcrst());
        setString(insertStatement, 11, record.getPrelim());
        
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
        String deleteStatement = "DELETE FROM crest " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a CrestRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(CrestRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE crest SET lid = ?, datcrst = ?, cremark = ?, hw = ?, jam = ?, olddatum = ?, q = ?, stage = ?, suppress = ?, timcrst = ?, prelim = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setDate(updateStatement, 2, record.getDatcrst());
        setString(updateStatement, 3, record.getCremark());
        setString(updateStatement, 4, record.getHw());
        setString(updateStatement, 5, record.getJam());
        setString(updateStatement, 6, record.getOlddatum());
        setInt(updateStatement, 7, record.getQ());
        setDouble(updateStatement, 8, record.getStage());
        setString(updateStatement, 9, record.getSuppress());
        setString(updateStatement, 10, record.getTimcrst());
        setString(updateStatement, 11, record.getPrelim());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(CrestRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM crest " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a CrestRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(CrestRecord oldRecord, CrestRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE crest SET lid = ?, datcrst = ?, cremark = ?, hw = ?, jam = ?, olddatum = ?, q = ?, stage = ?, suppress = ?, timcrst = ?, prelim = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setDate(updateStatement, 2, newRecord.getDatcrst());
        setString(updateStatement, 3, newRecord.getCremark());
        setString(updateStatement, 4, newRecord.getHw());
        setString(updateStatement, 5, newRecord.getJam());
        setString(updateStatement, 6, newRecord.getOlddatum());
        setInt(updateStatement, 7, newRecord.getQ());
        setDouble(updateStatement, 8, newRecord.getStage());
        setString(updateStatement, 9, newRecord.getSuppress());
        setString(updateStatement, 10, newRecord.getTimcrst());
        setString(updateStatement, 11, newRecord.getPrelim());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a CrestRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(CrestRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            CrestRecord oldRecord = (CrestRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of CrestTable class
