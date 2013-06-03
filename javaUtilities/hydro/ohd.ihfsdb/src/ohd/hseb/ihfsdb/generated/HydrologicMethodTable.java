// filename: HydrologicMethodTable.java
// author  : DBGEN
// created : Tue May 31 17:52:24 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              hydrologicmethod table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class HydrologicMethodTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  HydrologicMethodTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public HydrologicMethodTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("hydrologicmethod");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of HydrologicMethodRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        HydrologicMethodRecord record = null;

        // create a List to hold HydrologicMethod Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM hydrologicmethod " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a HydrologicMethodRecord
            // and store its address in oneRecord
            record = new HydrologicMethodRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a HydrologicMethodRecord object

            record.setHydrol_method(getString(rs, 1));
            
            // add this HydrologicMethodRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the HydrologicMethodRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of HydrologicMethodRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        HydrologicMethodRecord record = null;

        // create a List to hold HydrologicMethod Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM hydrologicmethod " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a HydrologicMethodRecord
            // and store its address in oneRecord
            record = new HydrologicMethodRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a HydrologicMethodRecord object

            record.setHydrol_method(getString(rs, 1));
            
            // add this HydrologicMethodRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the HydrologicMethodRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a HydrologicMethodRecord object and..
//-----------------------------------------------------------------
    public int insert(HydrologicMethodRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO hydrologicmethod VALUES (?        )");

        setString(insertStatement, 1, record.getHydrol_method());
        
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
        String deleteStatement = "DELETE FROM hydrologicmethod " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a HydrologicMethodRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(HydrologicMethodRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE hydrologicmethod SET hydrol_method = ?        " + where );

        setString(updateStatement, 1, record.getHydrol_method());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(HydrologicMethodRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM hydrologicmethod " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a HydrologicMethodRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(HydrologicMethodRecord oldRecord, HydrologicMethodRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE hydrologicmethod SET hydrol_method = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getHydrol_method());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a HydrologicMethodRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(HydrologicMethodRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            HydrologicMethodRecord oldRecord = (HydrologicMethodRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of HydrologicMethodTable class
