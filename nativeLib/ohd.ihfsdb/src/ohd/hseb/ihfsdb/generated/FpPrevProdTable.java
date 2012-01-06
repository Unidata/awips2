// filename: FpPrevProdTable.java
// author  : DBGEN
// created : Tue May 31 17:52:23 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              fpprevprod table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class FpPrevProdTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  FpPrevProdTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public FpPrevProdTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("fpprevprod");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of FpPrevProdRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        FpPrevProdRecord record = null;

        // create a List to hold FpPrevProd Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM fpprevprod " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a FpPrevProdRecord
            // and store its address in oneRecord
            record = new FpPrevProdRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a FpPrevProdRecord object

            record.setLid(getString(rs, 1));
            record.setProduct_id(getString(rs, 2));
            record.setProd_categ(getString(rs, 3));
            record.setProducttime(getTimeStamp(rs, 4));
            record.setOffice_id(getString(rs, 5));
            record.setObsvalue(getDouble(rs, 6));
            record.setObstime(getTimeStamp(rs, 7));
            record.setMax_fcstvalue(getDouble(rs, 8));
            record.setValidtime(getTimeStamp(rs, 9));
            record.setBasistime(getTimeStamp(rs, 10));
            
            // add this FpPrevProdRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the FpPrevProdRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of FpPrevProdRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        FpPrevProdRecord record = null;

        // create a List to hold FpPrevProd Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM fpprevprod " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a FpPrevProdRecord
            // and store its address in oneRecord
            record = new FpPrevProdRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a FpPrevProdRecord object

            record.setLid(getString(rs, 1));
            record.setProduct_id(getString(rs, 2));
            record.setProd_categ(getString(rs, 3));
            record.setProducttime(getTimeStamp(rs, 4));
            record.setOffice_id(getString(rs, 5));
            record.setObsvalue(getDouble(rs, 6));
            record.setObstime(getTimeStamp(rs, 7));
            record.setMax_fcstvalue(getDouble(rs, 8));
            record.setValidtime(getTimeStamp(rs, 9));
            record.setBasistime(getTimeStamp(rs, 10));
            
            // add this FpPrevProdRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the FpPrevProdRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a FpPrevProdRecord object and..
//-----------------------------------------------------------------
    public int insert(FpPrevProdRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO fpprevprod VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getProduct_id());
        setString(insertStatement, 3, record.getProd_categ());
        setTimeStamp(insertStatement, 4, record.getProducttime());
        setString(insertStatement, 5, record.getOffice_id());
        setDouble(insertStatement, 6, record.getObsvalue());
        setTimeStamp(insertStatement, 7, record.getObstime());
        setDouble(insertStatement, 8, record.getMax_fcstvalue());
        setTimeStamp(insertStatement, 9, record.getValidtime());
        setTimeStamp(insertStatement, 10, record.getBasistime());
        
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
        String deleteStatement = "DELETE FROM fpprevprod " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a FpPrevProdRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(FpPrevProdRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE fpprevprod SET lid = ?, product_id = ?, prod_categ = ?, producttime = ?, office_id = ?, obsvalue = ?, obstime = ?, max_fcstvalue = ?, validtime = ?, basistime = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setString(updateStatement, 2, record.getProduct_id());
        setString(updateStatement, 3, record.getProd_categ());
        setTimeStamp(updateStatement, 4, record.getProducttime());
        setString(updateStatement, 5, record.getOffice_id());
        setDouble(updateStatement, 6, record.getObsvalue());
        setTimeStamp(updateStatement, 7, record.getObstime());
        setDouble(updateStatement, 8, record.getMax_fcstvalue());
        setTimeStamp(updateStatement, 9, record.getValidtime());
        setTimeStamp(updateStatement, 10, record.getBasistime());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(FpPrevProdRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM fpprevprod " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a FpPrevProdRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(FpPrevProdRecord oldRecord, FpPrevProdRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE fpprevprod SET lid = ?, product_id = ?, prod_categ = ?, producttime = ?, office_id = ?, obsvalue = ?, obstime = ?, max_fcstvalue = ?, validtime = ?, basistime = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getProduct_id());
        setString(updateStatement, 3, newRecord.getProd_categ());
        setTimeStamp(updateStatement, 4, newRecord.getProducttime());
        setString(updateStatement, 5, newRecord.getOffice_id());
        setDouble(updateStatement, 6, newRecord.getObsvalue());
        setTimeStamp(updateStatement, 7, newRecord.getObstime());
        setDouble(updateStatement, 8, newRecord.getMax_fcstvalue());
        setTimeStamp(updateStatement, 9, newRecord.getValidtime());
        setTimeStamp(updateStatement, 10, newRecord.getBasistime());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a FpPrevProdRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(FpPrevProdRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            FpPrevProdRecord oldRecord = (FpPrevProdRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of FpPrevProdTable class
