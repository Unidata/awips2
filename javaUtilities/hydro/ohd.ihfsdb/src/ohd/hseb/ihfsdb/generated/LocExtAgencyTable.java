// filename: LocExtAgencyTable.java
// author  : DBGEN
// created : Tue May 31 17:52:24 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              locextagency table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class LocExtAgencyTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  LocExtAgencyTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public LocExtAgencyTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("locextagency");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of LocExtAgencyRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        LocExtAgencyRecord record = null;

        // create a List to hold LocExtAgency Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM locextagency " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a LocExtAgencyRecord
            // and store its address in oneRecord
            record = new LocExtAgencyRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a LocExtAgencyRecord object

            record.setLid(getString(rs, 1));
            record.setAgency_code(getString(rs, 2));
            record.setOffice(getString(rs, 3));
            
            // add this LocExtAgencyRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the LocExtAgencyRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of LocExtAgencyRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        LocExtAgencyRecord record = null;

        // create a List to hold LocExtAgency Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM locextagency " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a LocExtAgencyRecord
            // and store its address in oneRecord
            record = new LocExtAgencyRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a LocExtAgencyRecord object

            record.setLid(getString(rs, 1));
            record.setAgency_code(getString(rs, 2));
            record.setOffice(getString(rs, 3));
            
            // add this LocExtAgencyRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the LocExtAgencyRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a LocExtAgencyRecord object and..
//-----------------------------------------------------------------
    public int insert(LocExtAgencyRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO locextagency VALUES (?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getAgency_code());
        setString(insertStatement, 3, record.getOffice());
        
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
        String deleteStatement = "DELETE FROM locextagency " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a LocExtAgencyRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(LocExtAgencyRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE locextagency SET lid = ?, agency_code = ?, office = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setString(updateStatement, 2, record.getAgency_code());
        setString(updateStatement, 3, record.getOffice());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(LocExtAgencyRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM locextagency " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a LocExtAgencyRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(LocExtAgencyRecord oldRecord, LocExtAgencyRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE locextagency SET lid = ?, agency_code = ?, office = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getAgency_code());
        setString(updateStatement, 3, newRecord.getOffice());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a LocExtAgencyRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(LocExtAgencyRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            LocExtAgencyRecord oldRecord = (LocExtAgencyRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of LocExtAgencyTable class
