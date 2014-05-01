// filename: LocationTable.java
// author  : DBGEN
// created : Tue May 31 17:52:24 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              location table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class LocationTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  LocationTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public LocationTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("location");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of LocationRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        LocationRecord record = null;

        // create a List to hold Location Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM location " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a LocationRecord
            // and store its address in oneRecord
            record = new LocationRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a LocationRecord object

            record.setLid(getString(rs, 1));
            record.setCounty(getString(rs, 2));
            record.setCoe(getString(rs, 3));
            record.setCpm(getString(rs, 4));
            record.setDetail(getString(rs, 5));
            record.setElev(getDouble(rs, 6));
            record.setHdatum(getString(rs, 7));
            record.setHsa(getString(rs, 8));
            record.setHu(getString(rs, 9));
            record.setLat(getDouble(rs, 10));
            record.setLon(getDouble(rs, 11));
            record.setLremark(getString(rs, 12));
            record.setLrevise(getDate(rs, 13));
            record.setName(getString(rs, 14));
            record.setNetwork(getString(rs, 15));
            record.setRb(getString(rs, 16));
            record.setRfc(getString(rs, 17));
            record.setSbd(getDate(rs, 18));
            record.setSn(getString(rs, 19));
            record.setState(getString(rs, 20));
            record.setWaro(getString(rs, 21));
            record.setWfo(getString(rs, 22));
            record.setWsfo(getString(rs, 23));
            record.setType(getString(rs, 24));
            record.setDes(getString(rs, 25));
            record.setDet(getString(rs, 26));
            record.setPost(getInt(rs, 27));
            record.setStntype(getString(rs, 28));
            record.setTzone(getString(rs, 29));
            
            // add this LocationRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the LocationRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of LocationRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        LocationRecord record = null;

        // create a List to hold Location Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM location " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a LocationRecord
            // and store its address in oneRecord
            record = new LocationRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a LocationRecord object

            record.setLid(getString(rs, 1));
            record.setCounty(getString(rs, 2));
            record.setCoe(getString(rs, 3));
            record.setCpm(getString(rs, 4));
            record.setDetail(getString(rs, 5));
            record.setElev(getDouble(rs, 6));
            record.setHdatum(getString(rs, 7));
            record.setHsa(getString(rs, 8));
            record.setHu(getString(rs, 9));
            record.setLat(getDouble(rs, 10));
            record.setLon(getDouble(rs, 11));
            record.setLremark(getString(rs, 12));
            record.setLrevise(getDate(rs, 13));
            record.setName(getString(rs, 14));
            record.setNetwork(getString(rs, 15));
            record.setRb(getString(rs, 16));
            record.setRfc(getString(rs, 17));
            record.setSbd(getDate(rs, 18));
            record.setSn(getString(rs, 19));
            record.setState(getString(rs, 20));
            record.setWaro(getString(rs, 21));
            record.setWfo(getString(rs, 22));
            record.setWsfo(getString(rs, 23));
            record.setType(getString(rs, 24));
            record.setDes(getString(rs, 25));
            record.setDet(getString(rs, 26));
            record.setPost(getInt(rs, 27));
            record.setStntype(getString(rs, 28));
            record.setTzone(getString(rs, 29));
            
            // add this LocationRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the LocationRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a LocationRecord object and..
//-----------------------------------------------------------------
    public int insert(LocationRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO location VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getCounty());
        setString(insertStatement, 3, record.getCoe());
        setString(insertStatement, 4, record.getCpm());
        setString(insertStatement, 5, record.getDetail());
        setDouble(insertStatement, 6, record.getElev());
        setString(insertStatement, 7, record.getHdatum());
        setString(insertStatement, 8, record.getHsa());
        setString(insertStatement, 9, record.getHu());
        setDouble(insertStatement, 10, record.getLat());
        setDouble(insertStatement, 11, record.getLon());
        setString(insertStatement, 12, record.getLremark());
        setDate(insertStatement, 13, record.getLrevise());
        setString(insertStatement, 14, record.getName());
        setString(insertStatement, 15, record.getNetwork());
        setString(insertStatement, 16, record.getRb());
        setString(insertStatement, 17, record.getRfc());
        setDate(insertStatement, 18, record.getSbd());
        setString(insertStatement, 19, record.getSn());
        setString(insertStatement, 20, record.getState());
        setString(insertStatement, 21, record.getWaro());
        setString(insertStatement, 22, record.getWfo());
        setString(insertStatement, 23, record.getWsfo());
        setString(insertStatement, 24, record.getType());
        setString(insertStatement, 25, record.getDes());
        setString(insertStatement, 26, record.getDet());
        setInt(insertStatement, 27, record.getPost());
        setString(insertStatement, 28, record.getStntype());
        setString(insertStatement, 29, record.getTzone());
        
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
        String deleteStatement = "DELETE FROM location " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a LocationRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(LocationRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE location SET lid = ?, county = ?, coe = ?, cpm = ?, detail = ?, elev = ?, hdatum = ?, hsa = ?, hu = ?, lat = ?, lon = ?, lremark = ?, lrevise = ?, name = ?, network = ?, rb = ?, rfc = ?, sbd = ?, sn = ?, state = ?, waro = ?, wfo = ?, wsfo = ?, type = ?, des = ?, det = ?, post = ?, stntype = ?, tzone = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setString(updateStatement, 2, record.getCounty());
        setString(updateStatement, 3, record.getCoe());
        setString(updateStatement, 4, record.getCpm());
        setString(updateStatement, 5, record.getDetail());
        setDouble(updateStatement, 6, record.getElev());
        setString(updateStatement, 7, record.getHdatum());
        setString(updateStatement, 8, record.getHsa());
        setString(updateStatement, 9, record.getHu());
        setDouble(updateStatement, 10, record.getLat());
        setDouble(updateStatement, 11, record.getLon());
        setString(updateStatement, 12, record.getLremark());
        setDate(updateStatement, 13, record.getLrevise());
        setString(updateStatement, 14, record.getName());
        setString(updateStatement, 15, record.getNetwork());
        setString(updateStatement, 16, record.getRb());
        setString(updateStatement, 17, record.getRfc());
        setDate(updateStatement, 18, record.getSbd());
        setString(updateStatement, 19, record.getSn());
        setString(updateStatement, 20, record.getState());
        setString(updateStatement, 21, record.getWaro());
        setString(updateStatement, 22, record.getWfo());
        setString(updateStatement, 23, record.getWsfo());
        setString(updateStatement, 24, record.getType());
        setString(updateStatement, 25, record.getDes());
        setString(updateStatement, 26, record.getDet());
        setInt(updateStatement, 27, record.getPost());
        setString(updateStatement, 28, record.getStntype());
        setString(updateStatement, 29, record.getTzone());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(LocationRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM location " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a LocationRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(LocationRecord oldRecord, LocationRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE location SET lid = ?, county = ?, coe = ?, cpm = ?, detail = ?, elev = ?, hdatum = ?, hsa = ?, hu = ?, lat = ?, lon = ?, lremark = ?, lrevise = ?, name = ?, network = ?, rb = ?, rfc = ?, sbd = ?, sn = ?, state = ?, waro = ?, wfo = ?, wsfo = ?, type = ?, des = ?, det = ?, post = ?, stntype = ?, tzone = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getCounty());
        setString(updateStatement, 3, newRecord.getCoe());
        setString(updateStatement, 4, newRecord.getCpm());
        setString(updateStatement, 5, newRecord.getDetail());
        setDouble(updateStatement, 6, newRecord.getElev());
        setString(updateStatement, 7, newRecord.getHdatum());
        setString(updateStatement, 8, newRecord.getHsa());
        setString(updateStatement, 9, newRecord.getHu());
        setDouble(updateStatement, 10, newRecord.getLat());
        setDouble(updateStatement, 11, newRecord.getLon());
        setString(updateStatement, 12, newRecord.getLremark());
        setDate(updateStatement, 13, newRecord.getLrevise());
        setString(updateStatement, 14, newRecord.getName());
        setString(updateStatement, 15, newRecord.getNetwork());
        setString(updateStatement, 16, newRecord.getRb());
        setString(updateStatement, 17, newRecord.getRfc());
        setDate(updateStatement, 18, newRecord.getSbd());
        setString(updateStatement, 19, newRecord.getSn());
        setString(updateStatement, 20, newRecord.getState());
        setString(updateStatement, 21, newRecord.getWaro());
        setString(updateStatement, 22, newRecord.getWfo());
        setString(updateStatement, 23, newRecord.getWsfo());
        setString(updateStatement, 24, newRecord.getType());
        setString(updateStatement, 25, newRecord.getDes());
        setString(updateStatement, 26, newRecord.getDet());
        setInt(updateStatement, 27, newRecord.getPost());
        setString(updateStatement, 28, newRecord.getStntype());
        setString(updateStatement, 29, newRecord.getTzone());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a LocationRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(LocationRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            LocationRecord oldRecord = (LocationRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of LocationTable class
