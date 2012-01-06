// filename: S3PostAnalParamsTable.java
// author  : DBGEN
// created : Tue May 31 17:52:28 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              s3postanalparams table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class S3PostAnalParamsTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  S3PostAnalParamsTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public S3PostAnalParamsTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("s3postanalparams");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of S3PostAnalParamsRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        S3PostAnalParamsRecord record = null;

        // create a List to hold S3PostAnalParams Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM s3postanalparams " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a S3PostAnalParamsRecord
            // and store its address in oneRecord
            record = new S3PostAnalParamsRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a S3PostAnalParamsRecord object

            record.setGg_weighting(getShort(rs, 1));
            record.setGg_min_gage_val(getReal(rs, 2));
            record.setGg_min_dist(getShort(rs, 3));
            record.setKernel_est_scale(getReal(rs, 4));
            record.setRhat(getReal(rs, 5));
            
            // add this S3PostAnalParamsRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the S3PostAnalParamsRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of S3PostAnalParamsRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        S3PostAnalParamsRecord record = null;

        // create a List to hold S3PostAnalParams Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM s3postanalparams " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a S3PostAnalParamsRecord
            // and store its address in oneRecord
            record = new S3PostAnalParamsRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a S3PostAnalParamsRecord object

            record.setGg_weighting(getShort(rs, 1));
            record.setGg_min_gage_val(getReal(rs, 2));
            record.setGg_min_dist(getShort(rs, 3));
            record.setKernel_est_scale(getReal(rs, 4));
            record.setRhat(getReal(rs, 5));
            
            // add this S3PostAnalParamsRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the S3PostAnalParamsRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a S3PostAnalParamsRecord object and..
//-----------------------------------------------------------------
    public int insert(S3PostAnalParamsRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO s3postanalparams VALUES (?, ?, ?, ?, ?        )");

        setShort(insertStatement, 1, record.getGg_weighting());
        setReal(insertStatement, 2, record.getGg_min_gage_val());
        setShort(insertStatement, 3, record.getGg_min_dist());
        setReal(insertStatement, 4, record.getKernel_est_scale());
        setReal(insertStatement, 5, record.getRhat());
        
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
        String deleteStatement = "DELETE FROM s3postanalparams " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a S3PostAnalParamsRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(S3PostAnalParamsRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE s3postanalparams SET gg_weighting = ?, gg_min_gage_val = ?, gg_min_dist = ?, kernel_est_scale = ?, rhat = ?        " + where );

        setShort(updateStatement, 1, record.getGg_weighting());
        setReal(updateStatement, 2, record.getGg_min_gage_val());
        setShort(updateStatement, 3, record.getGg_min_dist());
        setReal(updateStatement, 4, record.getKernel_est_scale());
        setReal(updateStatement, 5, record.getRhat());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

} // end of S3PostAnalParamsTable class
