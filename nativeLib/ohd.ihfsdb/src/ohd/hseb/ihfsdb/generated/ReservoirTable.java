// filename: ReservoirTable.java
// author  : DBGEN
// created : Tue May 31 17:52:27 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              reservoir table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class ReservoirTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  ReservoirTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public ReservoirTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("reservoir");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of ReservoirRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        ReservoirRecord record = null;

        // create a List to hold Reservoir Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM reservoir " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a ReservoirRecord
            // and store its address in oneRecord
            record = new ReservoirRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a ReservoirRecord object

            record.setLid(getString(rs, 1));
            record.setName(getString(rs, 2));
            record.setType(getString(rs, 3));
            record.setOwner(getString(rs, 4));
            record.setDeadpool(getDouble(rs, 5));
            record.setConserpool(getDouble(rs, 6));
            record.setFloodpool(getDouble(rs, 7));
            record.setSpillway(getDouble(rs, 8));
            record.setSill(getDouble(rs, 9));
            record.setTop(getDouble(rs, 10));
            record.setSurchg(getDouble(rs, 11));
            record.setElev(getDouble(rs, 12));
            record.setGates(getInt(rs, 13));
            record.setImpounded(getDate(rs, 14));
            record.setUses(getString(rs, 15));
            record.setDamids(getString(rs, 16));
            record.setDamidn(getString(rs, 17));
            
            // add this ReservoirRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the ReservoirRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of ReservoirRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        ReservoirRecord record = null;

        // create a List to hold Reservoir Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM reservoir " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a ReservoirRecord
            // and store its address in oneRecord
            record = new ReservoirRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a ReservoirRecord object

            record.setLid(getString(rs, 1));
            record.setName(getString(rs, 2));
            record.setType(getString(rs, 3));
            record.setOwner(getString(rs, 4));
            record.setDeadpool(getDouble(rs, 5));
            record.setConserpool(getDouble(rs, 6));
            record.setFloodpool(getDouble(rs, 7));
            record.setSpillway(getDouble(rs, 8));
            record.setSill(getDouble(rs, 9));
            record.setTop(getDouble(rs, 10));
            record.setSurchg(getDouble(rs, 11));
            record.setElev(getDouble(rs, 12));
            record.setGates(getInt(rs, 13));
            record.setImpounded(getDate(rs, 14));
            record.setUses(getString(rs, 15));
            record.setDamids(getString(rs, 16));
            record.setDamidn(getString(rs, 17));
            
            // add this ReservoirRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the ReservoirRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a ReservoirRecord object and..
//-----------------------------------------------------------------
    public int insert(ReservoirRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO reservoir VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getName());
        setString(insertStatement, 3, record.getType());
        setString(insertStatement, 4, record.getOwner());
        setDouble(insertStatement, 5, record.getDeadpool());
        setDouble(insertStatement, 6, record.getConserpool());
        setDouble(insertStatement, 7, record.getFloodpool());
        setDouble(insertStatement, 8, record.getSpillway());
        setDouble(insertStatement, 9, record.getSill());
        setDouble(insertStatement, 10, record.getTop());
        setDouble(insertStatement, 11, record.getSurchg());
        setDouble(insertStatement, 12, record.getElev());
        setInt(insertStatement, 13, record.getGates());
        setDate(insertStatement, 14, record.getImpounded());
        setString(insertStatement, 15, record.getUses());
        setString(insertStatement, 16, record.getDamids());
        setString(insertStatement, 17, record.getDamidn());
        
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
        String deleteStatement = "DELETE FROM reservoir " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a ReservoirRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(ReservoirRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE reservoir SET lid = ?, name = ?, type = ?, owner = ?, deadpool = ?, conserpool = ?, floodpool = ?, spillway = ?, sill = ?, top = ?, surchg = ?, elev = ?, gates = ?, impounded = ?, uses = ?, damids = ?, damidn = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setString(updateStatement, 2, record.getName());
        setString(updateStatement, 3, record.getType());
        setString(updateStatement, 4, record.getOwner());
        setDouble(updateStatement, 5, record.getDeadpool());
        setDouble(updateStatement, 6, record.getConserpool());
        setDouble(updateStatement, 7, record.getFloodpool());
        setDouble(updateStatement, 8, record.getSpillway());
        setDouble(updateStatement, 9, record.getSill());
        setDouble(updateStatement, 10, record.getTop());
        setDouble(updateStatement, 11, record.getSurchg());
        setDouble(updateStatement, 12, record.getElev());
        setInt(updateStatement, 13, record.getGates());
        setDate(updateStatement, 14, record.getImpounded());
        setString(updateStatement, 15, record.getUses());
        setString(updateStatement, 16, record.getDamids());
        setString(updateStatement, 17, record.getDamidn());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(ReservoirRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM reservoir " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a ReservoirRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(ReservoirRecord oldRecord, ReservoirRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE reservoir SET lid = ?, name = ?, type = ?, owner = ?, deadpool = ?, conserpool = ?, floodpool = ?, spillway = ?, sill = ?, top = ?, surchg = ?, elev = ?, gates = ?, impounded = ?, uses = ?, damids = ?, damidn = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getName());
        setString(updateStatement, 3, newRecord.getType());
        setString(updateStatement, 4, newRecord.getOwner());
        setDouble(updateStatement, 5, newRecord.getDeadpool());
        setDouble(updateStatement, 6, newRecord.getConserpool());
        setDouble(updateStatement, 7, newRecord.getFloodpool());
        setDouble(updateStatement, 8, newRecord.getSpillway());
        setDouble(updateStatement, 9, newRecord.getSill());
        setDouble(updateStatement, 10, newRecord.getTop());
        setDouble(updateStatement, 11, newRecord.getSurchg());
        setDouble(updateStatement, 12, newRecord.getElev());
        setInt(updateStatement, 13, newRecord.getGates());
        setDate(updateStatement, 14, newRecord.getImpounded());
        setString(updateStatement, 15, newRecord.getUses());
        setString(updateStatement, 16, newRecord.getDamids());
        setString(updateStatement, 17, newRecord.getDamidn());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a ReservoirRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(ReservoirRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            ReservoirRecord oldRecord = (ReservoirRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of ReservoirTable class
