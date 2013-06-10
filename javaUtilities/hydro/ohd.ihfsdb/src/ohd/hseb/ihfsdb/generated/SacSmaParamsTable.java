// filename: SacSmaParamsTable.java
// author  : DBGEN
// created : Tue May 31 17:52:28 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              sacsmaparams table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class SacSmaParamsTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  SacSmaParamsTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public SacSmaParamsTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("sacsmaparams");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of SacSmaParamsRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        SacSmaParamsRecord record = null;

        // create a List to hold SacSmaParams Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM sacsmaparams " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a SacSmaParamsRecord
            // and store its address in oneRecord
            record = new SacSmaParamsRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a SacSmaParamsRecord object

            record.setBasin_id(getString(rs, 1));
            record.setSource(getString(rs, 2));
            record.setValidtime(getTimeStamp(rs, 3));
            record.setPostingtime(getTimeStamp(rs, 4));
            record.setUztwm(getDouble(rs, 5));
            record.setUzfwm(getDouble(rs, 6));
            record.setUzk(getDouble(rs, 7));
            record.setPctim(getDouble(rs, 8));
            record.setAdimp(getDouble(rs, 9));
            record.setRiva(getDouble(rs, 10));
            record.setZperc(getDouble(rs, 11));
            record.setRexp(getDouble(rs, 12));
            record.setLztwm(getDouble(rs, 13));
            record.setLzfsm(getDouble(rs, 14));
            record.setLzfpm(getDouble(rs, 15));
            record.setLzsk(getDouble(rs, 16));
            record.setLzpk(getDouble(rs, 17));
            record.setPfree(getDouble(rs, 18));
            record.setRserv(getDouble(rs, 19));
            record.setSide(getDouble(rs, 20));
            record.setPeadj(getDouble(rs, 21));
            record.setPxadj(getDouble(rs, 22));
            record.setEfc(getDouble(rs, 23));
            
            // add this SacSmaParamsRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the SacSmaParamsRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of SacSmaParamsRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        SacSmaParamsRecord record = null;

        // create a List to hold SacSmaParams Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM sacsmaparams " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a SacSmaParamsRecord
            // and store its address in oneRecord
            record = new SacSmaParamsRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a SacSmaParamsRecord object

            record.setBasin_id(getString(rs, 1));
            record.setSource(getString(rs, 2));
            record.setValidtime(getTimeStamp(rs, 3));
            record.setPostingtime(getTimeStamp(rs, 4));
            record.setUztwm(getDouble(rs, 5));
            record.setUzfwm(getDouble(rs, 6));
            record.setUzk(getDouble(rs, 7));
            record.setPctim(getDouble(rs, 8));
            record.setAdimp(getDouble(rs, 9));
            record.setRiva(getDouble(rs, 10));
            record.setZperc(getDouble(rs, 11));
            record.setRexp(getDouble(rs, 12));
            record.setLztwm(getDouble(rs, 13));
            record.setLzfsm(getDouble(rs, 14));
            record.setLzfpm(getDouble(rs, 15));
            record.setLzsk(getDouble(rs, 16));
            record.setLzpk(getDouble(rs, 17));
            record.setPfree(getDouble(rs, 18));
            record.setRserv(getDouble(rs, 19));
            record.setSide(getDouble(rs, 20));
            record.setPeadj(getDouble(rs, 21));
            record.setPxadj(getDouble(rs, 22));
            record.setEfc(getDouble(rs, 23));
            
            // add this SacSmaParamsRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the SacSmaParamsRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a SacSmaParamsRecord object and..
//-----------------------------------------------------------------
    public int insert(SacSmaParamsRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO sacsmaparams VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getBasin_id());
        setString(insertStatement, 2, record.getSource());
        setTimeStamp(insertStatement, 3, record.getValidtime());
        setTimeStamp(insertStatement, 4, record.getPostingtime());
        setDouble(insertStatement, 5, record.getUztwm());
        setDouble(insertStatement, 6, record.getUzfwm());
        setDouble(insertStatement, 7, record.getUzk());
        setDouble(insertStatement, 8, record.getPctim());
        setDouble(insertStatement, 9, record.getAdimp());
        setDouble(insertStatement, 10, record.getRiva());
        setDouble(insertStatement, 11, record.getZperc());
        setDouble(insertStatement, 12, record.getRexp());
        setDouble(insertStatement, 13, record.getLztwm());
        setDouble(insertStatement, 14, record.getLzfsm());
        setDouble(insertStatement, 15, record.getLzfpm());
        setDouble(insertStatement, 16, record.getLzsk());
        setDouble(insertStatement, 17, record.getLzpk());
        setDouble(insertStatement, 18, record.getPfree());
        setDouble(insertStatement, 19, record.getRserv());
        setDouble(insertStatement, 20, record.getSide());
        setDouble(insertStatement, 21, record.getPeadj());
        setDouble(insertStatement, 22, record.getPxadj());
        setDouble(insertStatement, 23, record.getEfc());
        
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
        String deleteStatement = "DELETE FROM sacsmaparams " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a SacSmaParamsRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(SacSmaParamsRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE sacsmaparams SET basin_id = ?, source = ?, validtime = ?, postingtime = ?, uztwm = ?, uzfwm = ?, uzk = ?, pctim = ?, adimp = ?, riva = ?, zperc = ?, rexp = ?, lztwm = ?, lzfsm = ?, lzfpm = ?, lzsk = ?, lzpk = ?, pfree = ?, rserv = ?, side = ?, peadj = ?, pxadj = ?, efc = ?        " + where );

        setString(updateStatement, 1, record.getBasin_id());
        setString(updateStatement, 2, record.getSource());
        setTimeStamp(updateStatement, 3, record.getValidtime());
        setTimeStamp(updateStatement, 4, record.getPostingtime());
        setDouble(updateStatement, 5, record.getUztwm());
        setDouble(updateStatement, 6, record.getUzfwm());
        setDouble(updateStatement, 7, record.getUzk());
        setDouble(updateStatement, 8, record.getPctim());
        setDouble(updateStatement, 9, record.getAdimp());
        setDouble(updateStatement, 10, record.getRiva());
        setDouble(updateStatement, 11, record.getZperc());
        setDouble(updateStatement, 12, record.getRexp());
        setDouble(updateStatement, 13, record.getLztwm());
        setDouble(updateStatement, 14, record.getLzfsm());
        setDouble(updateStatement, 15, record.getLzfpm());
        setDouble(updateStatement, 16, record.getLzsk());
        setDouble(updateStatement, 17, record.getLzpk());
        setDouble(updateStatement, 18, record.getPfree());
        setDouble(updateStatement, 19, record.getRserv());
        setDouble(updateStatement, 20, record.getSide());
        setDouble(updateStatement, 21, record.getPeadj());
        setDouble(updateStatement, 22, record.getPxadj());
        setDouble(updateStatement, 23, record.getEfc());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(SacSmaParamsRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM sacsmaparams " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a SacSmaParamsRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(SacSmaParamsRecord oldRecord, SacSmaParamsRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE sacsmaparams SET basin_id = ?, source = ?, validtime = ?, postingtime = ?, uztwm = ?, uzfwm = ?, uzk = ?, pctim = ?, adimp = ?, riva = ?, zperc = ?, rexp = ?, lztwm = ?, lzfsm = ?, lzfpm = ?, lzsk = ?, lzpk = ?, pfree = ?, rserv = ?, side = ?, peadj = ?, pxadj = ?, efc = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getBasin_id());
        setString(updateStatement, 2, newRecord.getSource());
        setTimeStamp(updateStatement, 3, newRecord.getValidtime());
        setTimeStamp(updateStatement, 4, newRecord.getPostingtime());
        setDouble(updateStatement, 5, newRecord.getUztwm());
        setDouble(updateStatement, 6, newRecord.getUzfwm());
        setDouble(updateStatement, 7, newRecord.getUzk());
        setDouble(updateStatement, 8, newRecord.getPctim());
        setDouble(updateStatement, 9, newRecord.getAdimp());
        setDouble(updateStatement, 10, newRecord.getRiva());
        setDouble(updateStatement, 11, newRecord.getZperc());
        setDouble(updateStatement, 12, newRecord.getRexp());
        setDouble(updateStatement, 13, newRecord.getLztwm());
        setDouble(updateStatement, 14, newRecord.getLzfsm());
        setDouble(updateStatement, 15, newRecord.getLzfpm());
        setDouble(updateStatement, 16, newRecord.getLzsk());
        setDouble(updateStatement, 17, newRecord.getLzpk());
        setDouble(updateStatement, 18, newRecord.getPfree());
        setDouble(updateStatement, 19, newRecord.getRserv());
        setDouble(updateStatement, 20, newRecord.getSide());
        setDouble(updateStatement, 21, newRecord.getPeadj());
        setDouble(updateStatement, 22, newRecord.getPxadj());
        setDouble(updateStatement, 23, newRecord.getEfc());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a SacSmaParamsRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(SacSmaParamsRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            SacSmaParamsRecord oldRecord = (SacSmaParamsRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of SacSmaParamsTable class
