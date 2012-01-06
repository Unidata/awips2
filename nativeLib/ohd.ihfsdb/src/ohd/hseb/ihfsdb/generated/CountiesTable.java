// filename: CountiesTable.java
// author  : DBGEN
// created : Tue May 31 17:52:20 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              counties table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class CountiesTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  CountiesTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public CountiesTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("counties");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of CountiesRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        CountiesRecord record = null;

        // create a List to hold Counties Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM counties " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a CountiesRecord
            // and store its address in oneRecord
            record = new CountiesRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a CountiesRecord object

            record.setCounty(getString(rs, 1));
            record.setState(getString(rs, 2));
            record.setCountynum(getString(rs, 3));
            record.setWfo(getString(rs, 4));
            record.setPrimary_back(getString(rs, 5));
            record.setSecondary_back(getString(rs, 6));
            
            // add this CountiesRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the CountiesRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of CountiesRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        CountiesRecord record = null;

        // create a List to hold Counties Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM counties " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a CountiesRecord
            // and store its address in oneRecord
            record = new CountiesRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a CountiesRecord object

            record.setCounty(getString(rs, 1));
            record.setState(getString(rs, 2));
            record.setCountynum(getString(rs, 3));
            record.setWfo(getString(rs, 4));
            record.setPrimary_back(getString(rs, 5));
            record.setSecondary_back(getString(rs, 6));
            
            // add this CountiesRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the CountiesRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a CountiesRecord object and..
//-----------------------------------------------------------------
    public int insert(CountiesRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO counties VALUES (?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getCounty());
        setString(insertStatement, 2, record.getState());
        setString(insertStatement, 3, record.getCountynum());
        setString(insertStatement, 4, record.getWfo());
        setString(insertStatement, 5, record.getPrimary_back());
        setString(insertStatement, 6, record.getSecondary_back());
        
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
        String deleteStatement = "DELETE FROM counties " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a CountiesRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(CountiesRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE counties SET county = ?, state = ?, countynum = ?, wfo = ?, primary_back = ?, secondary_back = ?        " + where );

        setString(updateStatement, 1, record.getCounty());
        setString(updateStatement, 2, record.getState());
        setString(updateStatement, 3, record.getCountynum());
        setString(updateStatement, 4, record.getWfo());
        setString(updateStatement, 5, record.getPrimary_back());
        setString(updateStatement, 6, record.getSecondary_back());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(CountiesRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM counties " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a CountiesRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(CountiesRecord oldRecord, CountiesRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE counties SET county = ?, state = ?, countynum = ?, wfo = ?, primary_back = ?, secondary_back = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getCounty());
        setString(updateStatement, 2, newRecord.getState());
        setString(updateStatement, 3, newRecord.getCountynum());
        setString(updateStatement, 4, newRecord.getWfo());
        setString(updateStatement, 5, newRecord.getPrimary_back());
        setString(updateStatement, 6, newRecord.getSecondary_back());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a CountiesRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(CountiesRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            CountiesRecord oldRecord = (CountiesRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of CountiesTable class
