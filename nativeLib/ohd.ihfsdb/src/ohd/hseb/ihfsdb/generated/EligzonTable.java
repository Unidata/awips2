// filename: EligzonTable.java
// author  : DBGEN
// created : Tue May 31 17:52:21 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              eligzon table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class EligzonTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  EligzonTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public EligzonTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("eligzon");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of EligzonRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        EligzonRecord record = null;

        // create a List to hold Eligzon Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM eligzon " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a EligzonRecord
            // and store its address in oneRecord
            record = new EligzonRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a EligzonRecord object

            record.setState(getString(rs, 1));
            record.setZonenum(getString(rs, 2));
            record.setDescr(getString(rs, 3));
            
            // add this EligzonRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the EligzonRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of EligzonRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        EligzonRecord record = null;

        // create a List to hold Eligzon Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM eligzon " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a EligzonRecord
            // and store its address in oneRecord
            record = new EligzonRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a EligzonRecord object

            record.setState(getString(rs, 1));
            record.setZonenum(getString(rs, 2));
            record.setDescr(getString(rs, 3));
            
            // add this EligzonRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the EligzonRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a EligzonRecord object and..
//-----------------------------------------------------------------
    public int insert(EligzonRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO eligzon VALUES (?, ?, ?        )");

        setString(insertStatement, 1, record.getState());
        setString(insertStatement, 2, record.getZonenum());
        setString(insertStatement, 3, record.getDescr());
        
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
        String deleteStatement = "DELETE FROM eligzon " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a EligzonRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(EligzonRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE eligzon SET state = ?, zonenum = ?, descr = ?        " + where );

        setString(updateStatement, 1, record.getState());
        setString(updateStatement, 2, record.getZonenum());
        setString(updateStatement, 3, record.getDescr());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(EligzonRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM eligzon " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a EligzonRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(EligzonRecord oldRecord, EligzonRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE eligzon SET state = ?, zonenum = ?, descr = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getState());
        setString(updateStatement, 2, newRecord.getZonenum());
        setString(updateStatement, 3, newRecord.getDescr());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a EligzonRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(EligzonRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            EligzonRecord oldRecord = (EligzonRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of EligzonTable class
