// filename: ContingencyValueTable.java
// author  : DBGEN
// created : Tue May 31 17:52:19 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              contingencyvalue table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class ContingencyValueTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  ContingencyValueTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public ContingencyValueTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("contingencyvalue");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of ContingencyValueRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        ContingencyValueRecord record = null;

        // create a List to hold ContingencyValue Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM contingencyvalue " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a ContingencyValueRecord
            // and store its address in oneRecord
            record = new ContingencyValueRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a ContingencyValueRecord object

            record.setLid(getString(rs, 1));
            record.setPe(getString(rs, 2));
            record.setDur(getShort(rs, 3));
            record.setTs(getString(rs, 4));
            record.setExtremum(getString(rs, 5));
            record.setProbability(getReal(rs, 6));
            record.setValidtime(getTimeStamp(rs, 7));
            record.setBasistime(getTimeStamp(rs, 8));
            record.setValue(getDouble(rs, 9));
            record.setShef_qual_code(getString(rs, 10));
            record.setQuality_code(getInt(rs, 11));
            record.setRevision(getShort(rs, 12));
            record.setProduct_id(getString(rs, 13));
            record.setProducttime(getTimeStamp(rs, 14));
            record.setPostingtime(getTimeStamp(rs, 15));
            
            // add this ContingencyValueRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the ContingencyValueRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of ContingencyValueRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        ContingencyValueRecord record = null;

        // create a List to hold ContingencyValue Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM contingencyvalue " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a ContingencyValueRecord
            // and store its address in oneRecord
            record = new ContingencyValueRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a ContingencyValueRecord object

            record.setLid(getString(rs, 1));
            record.setPe(getString(rs, 2));
            record.setDur(getShort(rs, 3));
            record.setTs(getString(rs, 4));
            record.setExtremum(getString(rs, 5));
            record.setProbability(getReal(rs, 6));
            record.setValidtime(getTimeStamp(rs, 7));
            record.setBasistime(getTimeStamp(rs, 8));
            record.setValue(getDouble(rs, 9));
            record.setShef_qual_code(getString(rs, 10));
            record.setQuality_code(getInt(rs, 11));
            record.setRevision(getShort(rs, 12));
            record.setProduct_id(getString(rs, 13));
            record.setProducttime(getTimeStamp(rs, 14));
            record.setPostingtime(getTimeStamp(rs, 15));
            
            // add this ContingencyValueRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the ContingencyValueRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a ContingencyValueRecord object and..
//-----------------------------------------------------------------
    public int insert(ContingencyValueRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO contingencyvalue VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getPe());
        setShort(insertStatement, 3, record.getDur());
        setString(insertStatement, 4, record.getTs());
        setString(insertStatement, 5, record.getExtremum());
        setReal(insertStatement, 6, record.getProbability());
        setTimeStamp(insertStatement, 7, record.getValidtime());
        setTimeStamp(insertStatement, 8, record.getBasistime());
        setDouble(insertStatement, 9, record.getValue());
        setString(insertStatement, 10, record.getShef_qual_code());
        setInt(insertStatement, 11, record.getQuality_code());
        setShort(insertStatement, 12, record.getRevision());
        setString(insertStatement, 13, record.getProduct_id());
        setTimeStamp(insertStatement, 14, record.getProducttime());
        setTimeStamp(insertStatement, 15, record.getPostingtime());
        
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
        String deleteStatement = "DELETE FROM contingencyvalue " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a ContingencyValueRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(ContingencyValueRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE contingencyvalue SET lid = ?, pe = ?, dur = ?, ts = ?, extremum = ?, probability = ?, validtime = ?, basistime = ?, value = ?, shef_qual_code = ?, quality_code = ?, revision = ?, product_id = ?, producttime = ?, postingtime = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setString(updateStatement, 2, record.getPe());
        setShort(updateStatement, 3, record.getDur());
        setString(updateStatement, 4, record.getTs());
        setString(updateStatement, 5, record.getExtremum());
        setReal(updateStatement, 6, record.getProbability());
        setTimeStamp(updateStatement, 7, record.getValidtime());
        setTimeStamp(updateStatement, 8, record.getBasistime());
        setDouble(updateStatement, 9, record.getValue());
        setString(updateStatement, 10, record.getShef_qual_code());
        setInt(updateStatement, 11, record.getQuality_code());
        setShort(updateStatement, 12, record.getRevision());
        setString(updateStatement, 13, record.getProduct_id());
        setTimeStamp(updateStatement, 14, record.getProducttime());
        setTimeStamp(updateStatement, 15, record.getPostingtime());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(ContingencyValueRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM contingencyvalue " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a ContingencyValueRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(ContingencyValueRecord oldRecord, ContingencyValueRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE contingencyvalue SET lid = ?, pe = ?, dur = ?, ts = ?, extremum = ?, probability = ?, validtime = ?, basistime = ?, value = ?, shef_qual_code = ?, quality_code = ?, revision = ?, product_id = ?, producttime = ?, postingtime = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getPe());
        setShort(updateStatement, 3, newRecord.getDur());
        setString(updateStatement, 4, newRecord.getTs());
        setString(updateStatement, 5, newRecord.getExtremum());
        setReal(updateStatement, 6, newRecord.getProbability());
        setTimeStamp(updateStatement, 7, newRecord.getValidtime());
        setTimeStamp(updateStatement, 8, newRecord.getBasistime());
        setDouble(updateStatement, 9, newRecord.getValue());
        setString(updateStatement, 10, newRecord.getShef_qual_code());
        setInt(updateStatement, 11, newRecord.getQuality_code());
        setShort(updateStatement, 12, newRecord.getRevision());
        setString(updateStatement, 13, newRecord.getProduct_id());
        setTimeStamp(updateStatement, 14, newRecord.getProducttime());
        setTimeStamp(updateStatement, 15, newRecord.getPostingtime());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a ContingencyValueRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(ContingencyValueRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            ContingencyValueRecord oldRecord = (ContingencyValueRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of ContingencyValueTable class
