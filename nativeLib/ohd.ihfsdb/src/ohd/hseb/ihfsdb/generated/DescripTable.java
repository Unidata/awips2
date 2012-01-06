// filename: DescripTable.java
// author  : DBGEN
// created : Tue May 31 17:52:20 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              descrip table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class DescripTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  DescripTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public DescripTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("descrip");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of DescripRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        DescripRecord record = null;

        // create a List to hold Descrip Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM descrip " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a DescripRecord
            // and store its address in oneRecord
            record = new DescripRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a DescripRecord object

            record.setLid(getString(rs, 1));
            record.setBed(getString(rs, 2));
            record.setDivert(getString(rs, 3));
            record.setRemark(getString(rs, 4));
            record.setIce(getString(rs, 5));
            record.setProximity(getString(rs, 6));
            record.setReach(getString(rs, 7));
            record.setRes(getString(rs, 8));
            record.setTopo(getString(rs, 9));
            
            // add this DescripRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the DescripRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of DescripRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        DescripRecord record = null;

        // create a List to hold Descrip Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM descrip " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a DescripRecord
            // and store its address in oneRecord
            record = new DescripRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a DescripRecord object

            record.setLid(getString(rs, 1));
            record.setBed(getString(rs, 2));
            record.setDivert(getString(rs, 3));
            record.setRemark(getString(rs, 4));
            record.setIce(getString(rs, 5));
            record.setProximity(getString(rs, 6));
            record.setReach(getString(rs, 7));
            record.setRes(getString(rs, 8));
            record.setTopo(getString(rs, 9));
            
            // add this DescripRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the DescripRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a DescripRecord object and..
//-----------------------------------------------------------------
    public int insert(DescripRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO descrip VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getBed());
        setString(insertStatement, 3, record.getDivert());
        setString(insertStatement, 4, record.getRemark());
        setString(insertStatement, 5, record.getIce());
        setString(insertStatement, 6, record.getProximity());
        setString(insertStatement, 7, record.getReach());
        setString(insertStatement, 8, record.getRes());
        setString(insertStatement, 9, record.getTopo());
        
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
        String deleteStatement = "DELETE FROM descrip " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a DescripRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(DescripRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE descrip SET lid = ?, bed = ?, divert = ?, remark = ?, ice = ?, proximity = ?, reach = ?, res = ?, topo = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setString(updateStatement, 2, record.getBed());
        setString(updateStatement, 3, record.getDivert());
        setString(updateStatement, 4, record.getRemark());
        setString(updateStatement, 5, record.getIce());
        setString(updateStatement, 6, record.getProximity());
        setString(updateStatement, 7, record.getReach());
        setString(updateStatement, 8, record.getRes());
        setString(updateStatement, 9, record.getTopo());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(DescripRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM descrip " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a DescripRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(DescripRecord oldRecord, DescripRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE descrip SET lid = ?, bed = ?, divert = ?, remark = ?, ice = ?, proximity = ?, reach = ?, res = ?, topo = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getBed());
        setString(updateStatement, 3, newRecord.getDivert());
        setString(updateStatement, 4, newRecord.getRemark());
        setString(updateStatement, 5, newRecord.getIce());
        setString(updateStatement, 6, newRecord.getProximity());
        setString(updateStatement, 7, newRecord.getReach());
        setString(updateStatement, 8, newRecord.getRes());
        setString(updateStatement, 9, newRecord.getTopo());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a DescripRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(DescripRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            DescripRecord oldRecord = (DescripRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of DescripTable class
