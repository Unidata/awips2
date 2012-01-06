// filename: HourlyPPTable.java
// author  : DBGEN
// created : Tue May 31 17:52:23 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              hourlypp table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class HourlyPPTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  HourlyPPTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public HourlyPPTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("hourlypp");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of HourlyPPRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        HourlyPPRecord record = null;

        // create a List to hold HourlyPP Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM hourlypp " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a HourlyPPRecord
            // and store its address in oneRecord
            record = new HourlyPPRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a HourlyPPRecord object

            record.setLid(getString(rs, 1));
            record.setTs(getString(rs, 2));
            record.setObsdate(getDate(rs, 3));
            record.setMinute_offset(getString(rs, 4));
            record.setHourly_qc(getString(rs, 5));
            record.setHour1(getShort(rs, 6));
            record.setHour2(getShort(rs, 7));
            record.setHour3(getShort(rs, 8));
            record.setHour4(getShort(rs, 9));
            record.setHour5(getShort(rs, 10));
            record.setHour6(getShort(rs, 11));
            record.setHour7(getShort(rs, 12));
            record.setHour8(getShort(rs, 13));
            record.setHour9(getShort(rs, 14));
            record.setHour10(getShort(rs, 15));
            record.setHour11(getShort(rs, 16));
            record.setHour12(getShort(rs, 17));
            record.setHour13(getShort(rs, 18));
            record.setHour14(getShort(rs, 19));
            record.setHour15(getShort(rs, 20));
            record.setHour16(getShort(rs, 21));
            record.setHour17(getShort(rs, 22));
            record.setHour18(getShort(rs, 23));
            record.setHour19(getShort(rs, 24));
            record.setHour20(getShort(rs, 25));
            record.setHour21(getShort(rs, 26));
            record.setHour22(getShort(rs, 27));
            record.setHour23(getShort(rs, 28));
            record.setHour24(getShort(rs, 29));
            record.setSixhr06(getShort(rs, 30));
            record.setSixhr12(getShort(rs, 31));
            record.setSixhr18(getShort(rs, 32));
            record.setSixhr24(getShort(rs, 33));
            record.setSixhrqc(getString(rs, 34));
            record.setSixhroffset(getString(rs, 35));
            
            // add this HourlyPPRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the HourlyPPRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of HourlyPPRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        HourlyPPRecord record = null;

        // create a List to hold HourlyPP Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM hourlypp " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a HourlyPPRecord
            // and store its address in oneRecord
            record = new HourlyPPRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a HourlyPPRecord object

            record.setLid(getString(rs, 1));
            record.setTs(getString(rs, 2));
            record.setObsdate(getDate(rs, 3));
            record.setMinute_offset(getString(rs, 4));
            record.setHourly_qc(getString(rs, 5));
            record.setHour1(getShort(rs, 6));
            record.setHour2(getShort(rs, 7));
            record.setHour3(getShort(rs, 8));
            record.setHour4(getShort(rs, 9));
            record.setHour5(getShort(rs, 10));
            record.setHour6(getShort(rs, 11));
            record.setHour7(getShort(rs, 12));
            record.setHour8(getShort(rs, 13));
            record.setHour9(getShort(rs, 14));
            record.setHour10(getShort(rs, 15));
            record.setHour11(getShort(rs, 16));
            record.setHour12(getShort(rs, 17));
            record.setHour13(getShort(rs, 18));
            record.setHour14(getShort(rs, 19));
            record.setHour15(getShort(rs, 20));
            record.setHour16(getShort(rs, 21));
            record.setHour17(getShort(rs, 22));
            record.setHour18(getShort(rs, 23));
            record.setHour19(getShort(rs, 24));
            record.setHour20(getShort(rs, 25));
            record.setHour21(getShort(rs, 26));
            record.setHour22(getShort(rs, 27));
            record.setHour23(getShort(rs, 28));
            record.setHour24(getShort(rs, 29));
            record.setSixhr06(getShort(rs, 30));
            record.setSixhr12(getShort(rs, 31));
            record.setSixhr18(getShort(rs, 32));
            record.setSixhr24(getShort(rs, 33));
            record.setSixhrqc(getString(rs, 34));
            record.setSixhroffset(getString(rs, 35));
            
            // add this HourlyPPRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the HourlyPPRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a HourlyPPRecord object and..
//-----------------------------------------------------------------
    public int insert(HourlyPPRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO hourlypp VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getTs());
        setDate(insertStatement, 3, record.getObsdate());
        setString(insertStatement, 4, record.getMinute_offset());
        setString(insertStatement, 5, record.getHourly_qc());
        setShort(insertStatement, 6, record.getHour1());
        setShort(insertStatement, 7, record.getHour2());
        setShort(insertStatement, 8, record.getHour3());
        setShort(insertStatement, 9, record.getHour4());
        setShort(insertStatement, 10, record.getHour5());
        setShort(insertStatement, 11, record.getHour6());
        setShort(insertStatement, 12, record.getHour7());
        setShort(insertStatement, 13, record.getHour8());
        setShort(insertStatement, 14, record.getHour9());
        setShort(insertStatement, 15, record.getHour10());
        setShort(insertStatement, 16, record.getHour11());
        setShort(insertStatement, 17, record.getHour12());
        setShort(insertStatement, 18, record.getHour13());
        setShort(insertStatement, 19, record.getHour14());
        setShort(insertStatement, 20, record.getHour15());
        setShort(insertStatement, 21, record.getHour16());
        setShort(insertStatement, 22, record.getHour17());
        setShort(insertStatement, 23, record.getHour18());
        setShort(insertStatement, 24, record.getHour19());
        setShort(insertStatement, 25, record.getHour20());
        setShort(insertStatement, 26, record.getHour21());
        setShort(insertStatement, 27, record.getHour22());
        setShort(insertStatement, 28, record.getHour23());
        setShort(insertStatement, 29, record.getHour24());
        setShort(insertStatement, 30, record.getSixhr06());
        setShort(insertStatement, 31, record.getSixhr12());
        setShort(insertStatement, 32, record.getSixhr18());
        setShort(insertStatement, 33, record.getSixhr24());
        setString(insertStatement, 34, record.getSixhrqc());
        setString(insertStatement, 35, record.getSixhroffset());
        
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
        String deleteStatement = "DELETE FROM hourlypp " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a HourlyPPRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(HourlyPPRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE hourlypp SET lid = ?, ts = ?, obsdate = ?, minute_offset = ?, hourly_qc = ?, hour1 = ?, hour2 = ?, hour3 = ?, hour4 = ?, hour5 = ?, hour6 = ?, hour7 = ?, hour8 = ?, hour9 = ?, hour10 = ?, hour11 = ?, hour12 = ?, hour13 = ?, hour14 = ?, hour15 = ?, hour16 = ?, hour17 = ?, hour18 = ?, hour19 = ?, hour20 = ?, hour21 = ?, hour22 = ?, hour23 = ?, hour24 = ?, sixhr06 = ?, sixhr12 = ?, sixhr18 = ?, sixhr24 = ?, sixhrqc = ?, sixhroffset = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setString(updateStatement, 2, record.getTs());
        setDate(updateStatement, 3, record.getObsdate());
        setString(updateStatement, 4, record.getMinute_offset());
        setString(updateStatement, 5, record.getHourly_qc());
        setShort(updateStatement, 6, record.getHour1());
        setShort(updateStatement, 7, record.getHour2());
        setShort(updateStatement, 8, record.getHour3());
        setShort(updateStatement, 9, record.getHour4());
        setShort(updateStatement, 10, record.getHour5());
        setShort(updateStatement, 11, record.getHour6());
        setShort(updateStatement, 12, record.getHour7());
        setShort(updateStatement, 13, record.getHour8());
        setShort(updateStatement, 14, record.getHour9());
        setShort(updateStatement, 15, record.getHour10());
        setShort(updateStatement, 16, record.getHour11());
        setShort(updateStatement, 17, record.getHour12());
        setShort(updateStatement, 18, record.getHour13());
        setShort(updateStatement, 19, record.getHour14());
        setShort(updateStatement, 20, record.getHour15());
        setShort(updateStatement, 21, record.getHour16());
        setShort(updateStatement, 22, record.getHour17());
        setShort(updateStatement, 23, record.getHour18());
        setShort(updateStatement, 24, record.getHour19());
        setShort(updateStatement, 25, record.getHour20());
        setShort(updateStatement, 26, record.getHour21());
        setShort(updateStatement, 27, record.getHour22());
        setShort(updateStatement, 28, record.getHour23());
        setShort(updateStatement, 29, record.getHour24());
        setShort(updateStatement, 30, record.getSixhr06());
        setShort(updateStatement, 31, record.getSixhr12());
        setShort(updateStatement, 32, record.getSixhr18());
        setShort(updateStatement, 33, record.getSixhr24());
        setString(updateStatement, 34, record.getSixhrqc());
        setString(updateStatement, 35, record.getSixhroffset());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(HourlyPPRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM hourlypp " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a HourlyPPRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(HourlyPPRecord oldRecord, HourlyPPRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE hourlypp SET lid = ?, ts = ?, obsdate = ?, minute_offset = ?, hourly_qc = ?, hour1 = ?, hour2 = ?, hour3 = ?, hour4 = ?, hour5 = ?, hour6 = ?, hour7 = ?, hour8 = ?, hour9 = ?, hour10 = ?, hour11 = ?, hour12 = ?, hour13 = ?, hour14 = ?, hour15 = ?, hour16 = ?, hour17 = ?, hour18 = ?, hour19 = ?, hour20 = ?, hour21 = ?, hour22 = ?, hour23 = ?, hour24 = ?, sixhr06 = ?, sixhr12 = ?, sixhr18 = ?, sixhr24 = ?, sixhrqc = ?, sixhroffset = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getTs());
        setDate(updateStatement, 3, newRecord.getObsdate());
        setString(updateStatement, 4, newRecord.getMinute_offset());
        setString(updateStatement, 5, newRecord.getHourly_qc());
        setShort(updateStatement, 6, newRecord.getHour1());
        setShort(updateStatement, 7, newRecord.getHour2());
        setShort(updateStatement, 8, newRecord.getHour3());
        setShort(updateStatement, 9, newRecord.getHour4());
        setShort(updateStatement, 10, newRecord.getHour5());
        setShort(updateStatement, 11, newRecord.getHour6());
        setShort(updateStatement, 12, newRecord.getHour7());
        setShort(updateStatement, 13, newRecord.getHour8());
        setShort(updateStatement, 14, newRecord.getHour9());
        setShort(updateStatement, 15, newRecord.getHour10());
        setShort(updateStatement, 16, newRecord.getHour11());
        setShort(updateStatement, 17, newRecord.getHour12());
        setShort(updateStatement, 18, newRecord.getHour13());
        setShort(updateStatement, 19, newRecord.getHour14());
        setShort(updateStatement, 20, newRecord.getHour15());
        setShort(updateStatement, 21, newRecord.getHour16());
        setShort(updateStatement, 22, newRecord.getHour17());
        setShort(updateStatement, 23, newRecord.getHour18());
        setShort(updateStatement, 24, newRecord.getHour19());
        setShort(updateStatement, 25, newRecord.getHour20());
        setShort(updateStatement, 26, newRecord.getHour21());
        setShort(updateStatement, 27, newRecord.getHour22());
        setShort(updateStatement, 28, newRecord.getHour23());
        setShort(updateStatement, 29, newRecord.getHour24());
        setShort(updateStatement, 30, newRecord.getSixhr06());
        setShort(updateStatement, 31, newRecord.getSixhr12());
        setShort(updateStatement, 32, newRecord.getSixhr18());
        setShort(updateStatement, 33, newRecord.getSixhr24());
        setString(updateStatement, 34, newRecord.getSixhrqc());
        setString(updateStatement, 35, newRecord.getSixhroffset());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a HourlyPPRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(HourlyPPRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            HourlyPPRecord oldRecord = (HourlyPPRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of HourlyPPTable class
