// filename: DcpTable.java
// author  : DBGEN
// created : Tue May 31 17:52:20 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              dcp table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class DcpTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  DcpTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public DcpTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("dcp");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of DcpRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        DcpRecord record = null;

        // create a List to hold Dcp Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM dcp " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a DcpRecord
            // and store its address in oneRecord
            record = new DcpRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a DcpRecord object

            record.setLid(getString(rs, 1));
            record.setCriteria(getString(rs, 2));
            record.setOwner(getString(rs, 3));
            record.setGoes(getString(rs, 4));
            record.setRptfreq(getString(rs, 5));
            record.setRptime(getString(rs, 6));
            record.setNotify(getString(rs, 7));
            record.setObsvfreq(getString(rs, 8));
            record.setRandrept(getString(rs, 9));
            
            // add this DcpRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the DcpRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of DcpRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        DcpRecord record = null;

        // create a List to hold Dcp Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM dcp " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a DcpRecord
            // and store its address in oneRecord
            record = new DcpRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a DcpRecord object

            record.setLid(getString(rs, 1));
            record.setCriteria(getString(rs, 2));
            record.setOwner(getString(rs, 3));
            record.setGoes(getString(rs, 4));
            record.setRptfreq(getString(rs, 5));
            record.setRptime(getString(rs, 6));
            record.setNotify(getString(rs, 7));
            record.setObsvfreq(getString(rs, 8));
            record.setRandrept(getString(rs, 9));
            
            // add this DcpRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the DcpRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a DcpRecord object and..
//-----------------------------------------------------------------
    public int insert(DcpRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO dcp VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getCriteria());
        setString(insertStatement, 3, record.getOwner());
        setString(insertStatement, 4, record.getGoes());
        setString(insertStatement, 5, record.getRptfreq());
        setString(insertStatement, 6, record.getRptime());
        setString(insertStatement, 7, record.getNotify());
        setString(insertStatement, 8, record.getObsvfreq());
        setString(insertStatement, 9, record.getRandrept());
        
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
        String deleteStatement = "DELETE FROM dcp " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a DcpRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(DcpRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE dcp SET lid = ?, criteria = ?, owner = ?, goes = ?, rptfreq = ?, rptime = ?, notify = ?, obsvfreq = ?, randrept = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setString(updateStatement, 2, record.getCriteria());
        setString(updateStatement, 3, record.getOwner());
        setString(updateStatement, 4, record.getGoes());
        setString(updateStatement, 5, record.getRptfreq());
        setString(updateStatement, 6, record.getRptime());
        setString(updateStatement, 7, record.getNotify());
        setString(updateStatement, 8, record.getObsvfreq());
        setString(updateStatement, 9, record.getRandrept());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(DcpRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM dcp " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a DcpRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(DcpRecord oldRecord, DcpRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE dcp SET lid = ?, criteria = ?, owner = ?, goes = ?, rptfreq = ?, rptime = ?, notify = ?, obsvfreq = ?, randrept = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getCriteria());
        setString(updateStatement, 3, newRecord.getOwner());
        setString(updateStatement, 4, newRecord.getGoes());
        setString(updateStatement, 5, newRecord.getRptfreq());
        setString(updateStatement, 6, newRecord.getRptime());
        setString(updateStatement, 7, newRecord.getNotify());
        setString(updateStatement, 8, newRecord.getObsvfreq());
        setString(updateStatement, 9, newRecord.getRandrept());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a DcpRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(DcpRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            DcpRecord oldRecord = (DcpRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of DcpTable class
