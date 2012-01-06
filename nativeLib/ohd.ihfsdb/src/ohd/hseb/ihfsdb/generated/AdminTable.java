// filename: AdminTable.java
// author  : DBGEN
// created : Tue May 31 17:52:19 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              admin table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class AdminTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  AdminTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public AdminTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("admin");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of AdminRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        AdminRecord record = null;

        // create a List to hold Admin Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM admin " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a AdminRecord
            // and store its address in oneRecord
            record = new AdminRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a AdminRecord object

            record.setFocalpoint(getString(rs, 1));
            record.setOfc(getString(rs, 2));
            record.setPhone(getString(rs, 3));
            record.setRegion(getString(rs, 4));
            record.setRegno(getString(rs, 5));
            record.setCd404(getString(rs, 6));
            record.setTenyr(getDate(rs, 7));
            record.setOneyr(getDate(rs, 8));
            record.setHsa(getString(rs, 9));
            record.setHsa_num(getShort(rs, 10));
            record.setHb_password(getString(rs, 11));
            
            // add this AdminRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the AdminRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of AdminRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        AdminRecord record = null;

        // create a List to hold Admin Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM admin " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a AdminRecord
            // and store its address in oneRecord
            record = new AdminRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a AdminRecord object

            record.setFocalpoint(getString(rs, 1));
            record.setOfc(getString(rs, 2));
            record.setPhone(getString(rs, 3));
            record.setRegion(getString(rs, 4));
            record.setRegno(getString(rs, 5));
            record.setCd404(getString(rs, 6));
            record.setTenyr(getDate(rs, 7));
            record.setOneyr(getDate(rs, 8));
            record.setHsa(getString(rs, 9));
            record.setHsa_num(getShort(rs, 10));
            record.setHb_password(getString(rs, 11));
            
            // add this AdminRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the AdminRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a AdminRecord object and..
//-----------------------------------------------------------------
    public int insert(AdminRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO admin VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getFocalpoint());
        setString(insertStatement, 2, record.getOfc());
        setString(insertStatement, 3, record.getPhone());
        setString(insertStatement, 4, record.getRegion());
        setString(insertStatement, 5, record.getRegno());
        setString(insertStatement, 6, record.getCd404());
        setDate(insertStatement, 7, record.getTenyr());
        setDate(insertStatement, 8, record.getOneyr());
        setString(insertStatement, 9, record.getHsa());
        setShort(insertStatement, 10, record.getHsa_num());
        setString(insertStatement, 11, record.getHb_password());
        
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
        String deleteStatement = "DELETE FROM admin " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a AdminRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(AdminRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE admin SET focalpoint = ?, ofc = ?, phone = ?, region = ?, regno = ?, cd404 = ?, tenyr = ?, oneyr = ?, hsa = ?, hsa_num = ?, hb_password = ?        " + where );

        setString(updateStatement, 1, record.getFocalpoint());
        setString(updateStatement, 2, record.getOfc());
        setString(updateStatement, 3, record.getPhone());
        setString(updateStatement, 4, record.getRegion());
        setString(updateStatement, 5, record.getRegno());
        setString(updateStatement, 6, record.getCd404());
        setDate(updateStatement, 7, record.getTenyr());
        setDate(updateStatement, 8, record.getOneyr());
        setString(updateStatement, 9, record.getHsa());
        setShort(updateStatement, 10, record.getHsa_num());
        setString(updateStatement, 11, record.getHb_password());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(AdminRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM admin " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a AdminRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(AdminRecord oldRecord, AdminRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE admin SET focalpoint = ?, ofc = ?, phone = ?, region = ?, regno = ?, cd404 = ?, tenyr = ?, oneyr = ?, hsa = ?, hsa_num = ?, hb_password = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getFocalpoint());
        setString(updateStatement, 2, newRecord.getOfc());
        setString(updateStatement, 3, newRecord.getPhone());
        setString(updateStatement, 4, newRecord.getRegion());
        setString(updateStatement, 5, newRecord.getRegno());
        setString(updateStatement, 6, newRecord.getCd404());
        setDate(updateStatement, 7, newRecord.getTenyr());
        setDate(updateStatement, 8, newRecord.getOneyr());
        setString(updateStatement, 9, newRecord.getHsa());
        setShort(updateStatement, 10, newRecord.getHsa_num());
        setString(updateStatement, 11, newRecord.getHb_password());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a AdminRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(AdminRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            AdminRecord oldRecord = (AdminRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of AdminTable class
