// filename: StnClassTable.java
// author  : DBGEN
// created : Tue May 31 17:52:29 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              stnclass table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class StnClassTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  StnClassTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public StnClassTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("stnclass");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of StnClassRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        StnClassRecord record = null;

        // create a List to hold StnClass Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM stnclass " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a StnClassRecord
            // and store its address in oneRecord
            record = new StnClassRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a StnClassRecord object

            record.setLid(getString(rs, 1));
            record.setDisp_class(getString(rs, 2));
            record.setDcp(getString(rs, 3));
            record.setObserver(getString(rs, 4));
            record.setTelem_type(getString(rs, 5));
            
            // add this StnClassRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the StnClassRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of StnClassRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        StnClassRecord record = null;

        // create a List to hold StnClass Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM stnclass " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a StnClassRecord
            // and store its address in oneRecord
            record = new StnClassRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a StnClassRecord object

            record.setLid(getString(rs, 1));
            record.setDisp_class(getString(rs, 2));
            record.setDcp(getString(rs, 3));
            record.setObserver(getString(rs, 4));
            record.setTelem_type(getString(rs, 5));
            
            // add this StnClassRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the StnClassRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a StnClassRecord object and..
//-----------------------------------------------------------------
    public int insert(StnClassRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO stnclass VALUES (?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getDisp_class());
        setString(insertStatement, 3, record.getDcp());
        setString(insertStatement, 4, record.getObserver());
        setString(insertStatement, 5, record.getTelem_type());
        
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
        String deleteStatement = "DELETE FROM stnclass " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a StnClassRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(StnClassRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE stnclass SET lid = ?, disp_class = ?, dcp = ?, observer = ?, telem_type = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setString(updateStatement, 2, record.getDisp_class());
        setString(updateStatement, 3, record.getDcp());
        setString(updateStatement, 4, record.getObserver());
        setString(updateStatement, 5, record.getTelem_type());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(StnClassRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM stnclass " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a StnClassRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(StnClassRecord oldRecord, StnClassRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE stnclass SET lid = ?, disp_class = ?, dcp = ?, observer = ?, telem_type = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getDisp_class());
        setString(updateStatement, 3, newRecord.getDcp());
        setString(updateStatement, 4, newRecord.getObserver());
        setString(updateStatement, 5, newRecord.getTelem_type());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a StnClassRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(StnClassRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            StnClassRecord oldRecord = (StnClassRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of StnClassTable class
