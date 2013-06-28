// filename: MonthlyValuesTable.java
// author  : DBGEN
// created : Tue May 31 17:52:25 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              monthlyvalues table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class MonthlyValuesTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  MonthlyValuesTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public MonthlyValuesTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("monthlyvalues");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of MonthlyValuesRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        MonthlyValuesRecord record = null;

        // create a List to hold MonthlyValues Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM monthlyvalues " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a MonthlyValuesRecord
            // and store its address in oneRecord
            record = new MonthlyValuesRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a MonthlyValuesRecord object

            record.setLid(getString(rs, 1));
            record.setPe(getString(rs, 2));
            record.setDur(getShort(rs, 3));
            record.setTs(getString(rs, 4));
            record.setExtremum(getString(rs, 5));
            record.setAdjustment(getString(rs, 6));
            record.setPostingtime(getTimeStamp(rs, 7));
            record.setJan_value(getDouble(rs, 8));
            record.setFeb_value(getDouble(rs, 9));
            record.setMar_value(getDouble(rs, 10));
            record.setApr_value(getDouble(rs, 11));
            record.setMay_value(getDouble(rs, 12));
            record.setJun_value(getDouble(rs, 13));
            record.setJul_value(getDouble(rs, 14));
            record.setAug_value(getDouble(rs, 15));
            record.setSep_value(getDouble(rs, 16));
            record.setOct_value(getDouble(rs, 17));
            record.setNov_value(getDouble(rs, 18));
            record.setDec_value(getDouble(rs, 19));
            
            // add this MonthlyValuesRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the MonthlyValuesRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of MonthlyValuesRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        MonthlyValuesRecord record = null;

        // create a List to hold MonthlyValues Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM monthlyvalues " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a MonthlyValuesRecord
            // and store its address in oneRecord
            record = new MonthlyValuesRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a MonthlyValuesRecord object

            record.setLid(getString(rs, 1));
            record.setPe(getString(rs, 2));
            record.setDur(getShort(rs, 3));
            record.setTs(getString(rs, 4));
            record.setExtremum(getString(rs, 5));
            record.setAdjustment(getString(rs, 6));
            record.setPostingtime(getTimeStamp(rs, 7));
            record.setJan_value(getDouble(rs, 8));
            record.setFeb_value(getDouble(rs, 9));
            record.setMar_value(getDouble(rs, 10));
            record.setApr_value(getDouble(rs, 11));
            record.setMay_value(getDouble(rs, 12));
            record.setJun_value(getDouble(rs, 13));
            record.setJul_value(getDouble(rs, 14));
            record.setAug_value(getDouble(rs, 15));
            record.setSep_value(getDouble(rs, 16));
            record.setOct_value(getDouble(rs, 17));
            record.setNov_value(getDouble(rs, 18));
            record.setDec_value(getDouble(rs, 19));
            
            // add this MonthlyValuesRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the MonthlyValuesRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a MonthlyValuesRecord object and..
//-----------------------------------------------------------------
    public int insert(MonthlyValuesRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO monthlyvalues VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getPe());
        setShort(insertStatement, 3, record.getDur());
        setString(insertStatement, 4, record.getTs());
        setString(insertStatement, 5, record.getExtremum());
        setString(insertStatement, 6, record.getAdjustment());
        setTimeStamp(insertStatement, 7, record.getPostingtime());
        setDouble(insertStatement, 8, record.getJan_value());
        setDouble(insertStatement, 9, record.getFeb_value());
        setDouble(insertStatement, 10, record.getMar_value());
        setDouble(insertStatement, 11, record.getApr_value());
        setDouble(insertStatement, 12, record.getMay_value());
        setDouble(insertStatement, 13, record.getJun_value());
        setDouble(insertStatement, 14, record.getJul_value());
        setDouble(insertStatement, 15, record.getAug_value());
        setDouble(insertStatement, 16, record.getSep_value());
        setDouble(insertStatement, 17, record.getOct_value());
        setDouble(insertStatement, 18, record.getNov_value());
        setDouble(insertStatement, 19, record.getDec_value());
        
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
        String deleteStatement = "DELETE FROM monthlyvalues " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a MonthlyValuesRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(MonthlyValuesRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE monthlyvalues SET lid = ?, pe = ?, dur = ?, ts = ?, extremum = ?, adjustment = ?, postingtime = ?, jan_value = ?, feb_value = ?, mar_value = ?, apr_value = ?, may_value = ?, jun_value = ?, jul_value = ?, aug_value = ?, sep_value = ?, oct_value = ?, nov_value = ?, dec_value = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setString(updateStatement, 2, record.getPe());
        setShort(updateStatement, 3, record.getDur());
        setString(updateStatement, 4, record.getTs());
        setString(updateStatement, 5, record.getExtremum());
        setString(updateStatement, 6, record.getAdjustment());
        setTimeStamp(updateStatement, 7, record.getPostingtime());
        setDouble(updateStatement, 8, record.getJan_value());
        setDouble(updateStatement, 9, record.getFeb_value());
        setDouble(updateStatement, 10, record.getMar_value());
        setDouble(updateStatement, 11, record.getApr_value());
        setDouble(updateStatement, 12, record.getMay_value());
        setDouble(updateStatement, 13, record.getJun_value());
        setDouble(updateStatement, 14, record.getJul_value());
        setDouble(updateStatement, 15, record.getAug_value());
        setDouble(updateStatement, 16, record.getSep_value());
        setDouble(updateStatement, 17, record.getOct_value());
        setDouble(updateStatement, 18, record.getNov_value());
        setDouble(updateStatement, 19, record.getDec_value());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(MonthlyValuesRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM monthlyvalues " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a MonthlyValuesRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(MonthlyValuesRecord oldRecord, MonthlyValuesRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE monthlyvalues SET lid = ?, pe = ?, dur = ?, ts = ?, extremum = ?, adjustment = ?, postingtime = ?, jan_value = ?, feb_value = ?, mar_value = ?, apr_value = ?, may_value = ?, jun_value = ?, jul_value = ?, aug_value = ?, sep_value = ?, oct_value = ?, nov_value = ?, dec_value = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getPe());
        setShort(updateStatement, 3, newRecord.getDur());
        setString(updateStatement, 4, newRecord.getTs());
        setString(updateStatement, 5, newRecord.getExtremum());
        setString(updateStatement, 6, newRecord.getAdjustment());
        setTimeStamp(updateStatement, 7, newRecord.getPostingtime());
        setDouble(updateStatement, 8, newRecord.getJan_value());
        setDouble(updateStatement, 9, newRecord.getFeb_value());
        setDouble(updateStatement, 10, newRecord.getMar_value());
        setDouble(updateStatement, 11, newRecord.getApr_value());
        setDouble(updateStatement, 12, newRecord.getMay_value());
        setDouble(updateStatement, 13, newRecord.getJun_value());
        setDouble(updateStatement, 14, newRecord.getJul_value());
        setDouble(updateStatement, 15, newRecord.getAug_value());
        setDouble(updateStatement, 16, newRecord.getSep_value());
        setDouble(updateStatement, 17, newRecord.getOct_value());
        setDouble(updateStatement, 18, newRecord.getNov_value());
        setDouble(updateStatement, 19, newRecord.getDec_value());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a MonthlyValuesRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(MonthlyValuesRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            MonthlyValuesRecord oldRecord = (MonthlyValuesRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of MonthlyValuesTable class
