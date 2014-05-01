// filename: TelemTable.java
// author  : DBGEN
// created : Tue May 31 17:52:29 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              telem table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class TelemTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  TelemTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public TelemTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("telem");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of TelemRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        TelemRecord record = null;

        // create a List to hold Telem Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM telem " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a TelemRecord
            // and store its address in oneRecord
            record = new TelemRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a TelemRecord object

            record.setLid(getString(rs, 1));
            record.setType(getString(rs, 2));
            record.setPayor(getString(rs, 3));
            record.setCost(getDouble(rs, 4));
            record.setCriteria(getString(rs, 5));
            record.setOwner(getString(rs, 6));
            record.setPhone(getString(rs, 7));
            record.setSensorid(getString(rs, 8));
            record.setRptfreq(getString(rs, 9));
            record.setNotify(getString(rs, 10));
            record.setObsvfreq(getString(rs, 11));
            
            // add this TelemRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the TelemRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of TelemRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        TelemRecord record = null;

        // create a List to hold Telem Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM telem " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a TelemRecord
            // and store its address in oneRecord
            record = new TelemRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a TelemRecord object

            record.setLid(getString(rs, 1));
            record.setType(getString(rs, 2));
            record.setPayor(getString(rs, 3));
            record.setCost(getDouble(rs, 4));
            record.setCriteria(getString(rs, 5));
            record.setOwner(getString(rs, 6));
            record.setPhone(getString(rs, 7));
            record.setSensorid(getString(rs, 8));
            record.setRptfreq(getString(rs, 9));
            record.setNotify(getString(rs, 10));
            record.setObsvfreq(getString(rs, 11));
            
            // add this TelemRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the TelemRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a TelemRecord object and..
//-----------------------------------------------------------------
    public int insert(TelemRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO telem VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getType());
        setString(insertStatement, 3, record.getPayor());
        setDouble(insertStatement, 4, record.getCost());
        setString(insertStatement, 5, record.getCriteria());
        setString(insertStatement, 6, record.getOwner());
        setString(insertStatement, 7, record.getPhone());
        setString(insertStatement, 8, record.getSensorid());
        setString(insertStatement, 9, record.getRptfreq());
        setString(insertStatement, 10, record.getNotify());
        setString(insertStatement, 11, record.getObsvfreq());
        
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
        String deleteStatement = "DELETE FROM telem " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a TelemRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(TelemRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE telem SET lid = ?, type = ?, payor = ?, cost = ?, criteria = ?, owner = ?, phone = ?, sensorid = ?, rptfreq = ?, notify = ?, obsvfreq = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setString(updateStatement, 2, record.getType());
        setString(updateStatement, 3, record.getPayor());
        setDouble(updateStatement, 4, record.getCost());
        setString(updateStatement, 5, record.getCriteria());
        setString(updateStatement, 6, record.getOwner());
        setString(updateStatement, 7, record.getPhone());
        setString(updateStatement, 8, record.getSensorid());
        setString(updateStatement, 9, record.getRptfreq());
        setString(updateStatement, 10, record.getNotify());
        setString(updateStatement, 11, record.getObsvfreq());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(TelemRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM telem " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a TelemRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(TelemRecord oldRecord, TelemRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE telem SET lid = ?, type = ?, payor = ?, cost = ?, criteria = ?, owner = ?, phone = ?, sensorid = ?, rptfreq = ?, notify = ?, obsvfreq = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getType());
        setString(updateStatement, 3, newRecord.getPayor());
        setDouble(updateStatement, 4, newRecord.getCost());
        setString(updateStatement, 5, newRecord.getCriteria());
        setString(updateStatement, 6, newRecord.getOwner());
        setString(updateStatement, 7, newRecord.getPhone());
        setString(updateStatement, 8, newRecord.getSensorid());
        setString(updateStatement, 9, newRecord.getRptfreq());
        setString(updateStatement, 10, newRecord.getNotify());
        setString(updateStatement, 11, newRecord.getObsvfreq());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a TelemRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(TelemRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            TelemRecord oldRecord = (TelemRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of TelemTable class
