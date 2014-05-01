// filename: TextProductTable.java
// author  : DBGEN
// created : Tue May 31 17:52:29 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              textproduct table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class TextProductTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  TextProductTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public TextProductTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("textproduct");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of TextProductRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        TextProductRecord record = null;

        // create a List to hold TextProduct Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM textproduct " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a TextProductRecord
            // and store its address in oneRecord
            record = new TextProductRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a TextProductRecord object

            record.setProduct_id(getString(rs, 1));
            record.setProducttime(getTimeStamp(rs, 2));
            record.setPostingtime(getTimeStamp(rs, 3));
            record.setProdtype(getString(rs, 4));
            record.setIssnum(getInt(rs, 5));
            record.setProduct(getString(rs, 6));
            
            // add this TextProductRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the TextProductRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of TextProductRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        TextProductRecord record = null;

        // create a List to hold TextProduct Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM textproduct " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a TextProductRecord
            // and store its address in oneRecord
            record = new TextProductRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a TextProductRecord object

            record.setProduct_id(getString(rs, 1));
            record.setProducttime(getTimeStamp(rs, 2));
            record.setPostingtime(getTimeStamp(rs, 3));
            record.setProdtype(getString(rs, 4));
            record.setIssnum(getInt(rs, 5));
            record.setProduct(getString(rs, 6));
            
            // add this TextProductRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the TextProductRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a TextProductRecord object and..
//-----------------------------------------------------------------
    public int insert(TextProductRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO textproduct VALUES (?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getProduct_id());
        setTimeStamp(insertStatement, 2, record.getProducttime());
        setTimeStamp(insertStatement, 3, record.getPostingtime());
        setString(insertStatement, 4, record.getProdtype());
        setInt(insertStatement, 5, record.getIssnum());
        setString(insertStatement, 6, record.getProduct());
        
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
        String deleteStatement = "DELETE FROM textproduct " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a TextProductRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(TextProductRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE textproduct SET product_id = ?, producttime = ?, postingtime = ?, prodtype = ?, issnum = ?, product = ?        " + where );

        setString(updateStatement, 1, record.getProduct_id());
        setTimeStamp(updateStatement, 2, record.getProducttime());
        setTimeStamp(updateStatement, 3, record.getPostingtime());
        setString(updateStatement, 4, record.getProdtype());
        setInt(updateStatement, 5, record.getIssnum());
        setString(updateStatement, 6, record.getProduct());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(TextProductRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM textproduct " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a TextProductRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(TextProductRecord oldRecord, TextProductRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE textproduct SET product_id = ?, producttime = ?, postingtime = ?, prodtype = ?, issnum = ?, product = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getProduct_id());
        setTimeStamp(updateStatement, 2, newRecord.getProducttime());
        setTimeStamp(updateStatement, 3, newRecord.getPostingtime());
        setString(updateStatement, 4, newRecord.getProdtype());
        setInt(updateStatement, 5, newRecord.getIssnum());
        setString(updateStatement, 6, newRecord.getProduct());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a TextProductRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(TextProductRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            TextProductRecord oldRecord = (TextProductRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of TextProductTable class
