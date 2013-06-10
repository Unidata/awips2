// filename: RWParamsTable.java
// author  : DBGEN
// created : Tue May 31 17:52:28 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              rwparams table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class RWParamsTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  RWParamsTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public RWParamsTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("rwparams");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of RWParamsRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        RWParamsRecord record = null;

        // create a List to hold RWParams Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM rwparams " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RWParamsRecord
            // and store its address in oneRecord
            record = new RWParamsRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RWParamsRecord object

            record.setRw_min_rain(getReal(rs, 1));
            record.setRw_sep_dist(getReal(rs, 2));
            record.setRw_lag0_ind_corr(getReal(rs, 3));
            record.setRw_lag0_cond_corr(getReal(rs, 4));
            record.setNum_near_gages(getShort(rs, 5));
            record.setNum_near_rad_bins(getShort(rs, 6));
            record.setDef_cond_var_rad(getReal(rs, 7));
            record.setDef_ind_corr_scl(getReal(rs, 8));
            record.setDef_cond_corr_scl(getReal(rs, 9));
            record.setMin_ind_corr_scl(getReal(rs, 10));
            record.setMin_cond_corr_scl(getReal(rs, 11));
            record.setMax_ind_corr_scl(getReal(rs, 12));
            record.setMax_cond_corr_scl(getReal(rs, 13));
            record.setNn_srch_method(getShort(rs, 14));
            
            // add this RWParamsRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the RWParamsRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of RWParamsRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        RWParamsRecord record = null;

        // create a List to hold RWParams Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM rwparams " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RWParamsRecord
            // and store its address in oneRecord
            record = new RWParamsRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RWParamsRecord object

            record.setRw_min_rain(getReal(rs, 1));
            record.setRw_sep_dist(getReal(rs, 2));
            record.setRw_lag0_ind_corr(getReal(rs, 3));
            record.setRw_lag0_cond_corr(getReal(rs, 4));
            record.setNum_near_gages(getShort(rs, 5));
            record.setNum_near_rad_bins(getShort(rs, 6));
            record.setDef_cond_var_rad(getReal(rs, 7));
            record.setDef_ind_corr_scl(getReal(rs, 8));
            record.setDef_cond_corr_scl(getReal(rs, 9));
            record.setMin_ind_corr_scl(getReal(rs, 10));
            record.setMin_cond_corr_scl(getReal(rs, 11));
            record.setMax_ind_corr_scl(getReal(rs, 12));
            record.setMax_cond_corr_scl(getReal(rs, 13));
            record.setNn_srch_method(getShort(rs, 14));
            
            // add this RWParamsRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the RWParamsRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a RWParamsRecord object and..
//-----------------------------------------------------------------
    public int insert(RWParamsRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO rwparams VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setReal(insertStatement, 1, record.getRw_min_rain());
        setReal(insertStatement, 2, record.getRw_sep_dist());
        setReal(insertStatement, 3, record.getRw_lag0_ind_corr());
        setReal(insertStatement, 4, record.getRw_lag0_cond_corr());
        setShort(insertStatement, 5, record.getNum_near_gages());
        setShort(insertStatement, 6, record.getNum_near_rad_bins());
        setReal(insertStatement, 7, record.getDef_cond_var_rad());
        setReal(insertStatement, 8, record.getDef_ind_corr_scl());
        setReal(insertStatement, 9, record.getDef_cond_corr_scl());
        setReal(insertStatement, 10, record.getMin_ind_corr_scl());
        setReal(insertStatement, 11, record.getMin_cond_corr_scl());
        setReal(insertStatement, 12, record.getMax_ind_corr_scl());
        setReal(insertStatement, 13, record.getMax_cond_corr_scl());
        setShort(insertStatement, 14, record.getNn_srch_method());
        
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
        String deleteStatement = "DELETE FROM rwparams " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RWParamsRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RWParamsRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE rwparams SET rw_min_rain = ?, rw_sep_dist = ?, rw_lag0_ind_corr = ?, rw_lag0_cond_corr = ?, num_near_gages = ?, num_near_rad_bins = ?, def_cond_var_rad = ?, def_ind_corr_scl = ?, def_cond_corr_scl = ?, min_ind_corr_scl = ?, min_cond_corr_scl = ?, max_ind_corr_scl = ?, max_cond_corr_scl = ?, nn_srch_method = ?        " + where );

        setReal(updateStatement, 1, record.getRw_min_rain());
        setReal(updateStatement, 2, record.getRw_sep_dist());
        setReal(updateStatement, 3, record.getRw_lag0_ind_corr());
        setReal(updateStatement, 4, record.getRw_lag0_cond_corr());
        setShort(updateStatement, 5, record.getNum_near_gages());
        setShort(updateStatement, 6, record.getNum_near_rad_bins());
        setReal(updateStatement, 7, record.getDef_cond_var_rad());
        setReal(updateStatement, 8, record.getDef_ind_corr_scl());
        setReal(updateStatement, 9, record.getDef_cond_corr_scl());
        setReal(updateStatement, 10, record.getMin_ind_corr_scl());
        setReal(updateStatement, 11, record.getMin_cond_corr_scl());
        setReal(updateStatement, 12, record.getMax_ind_corr_scl());
        setReal(updateStatement, 13, record.getMax_cond_corr_scl());
        setShort(updateStatement, 14, record.getNn_srch_method());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

} // end of RWParamsTable class
