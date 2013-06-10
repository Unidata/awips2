// filename: VTECeventTable.java
// author  : DBGEN
// created : Tue May 31 17:52:30 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              vtecevent table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class VTECeventTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  VTECeventTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public VTECeventTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("vtecevent");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of VTECeventRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        VTECeventRecord record = null;

        // create a List to hold VTECevent Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM vtecevent " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a VTECeventRecord
            // and store its address in oneRecord
            record = new VTECeventRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a VTECeventRecord object

            record.setGeoid(getString(rs, 1));
            record.setProduct_id(getString(rs, 2));
            record.setProducttime(getTimeStamp(rs, 3));
            record.setProductmode(getString(rs, 4));
            record.setAction(getString(rs, 5));
            record.setOffice_id(getString(rs, 6));
            record.setPhenom(getString(rs, 7));
            record.setSignif(getString(rs, 8));
            record.setEtn(getShort(rs, 9));
            record.setBegintime(getTimeStamp(rs, 10));
            record.setEndtime(getTimeStamp(rs, 11));
            record.setSeverity(getString(rs, 12));
            record.setImmed_cause(getString(rs, 13));
            record.setRisetime(getTimeStamp(rs, 14));
            record.setCresttime(getTimeStamp(rs, 15));
            record.setFalltime(getTimeStamp(rs, 16));
            record.setRecord(getString(rs, 17));
            record.setRisets(getString(rs, 18));
            record.setCrests(getString(rs, 19));
            record.setFallts(getString(rs, 20));
            record.setCrest_value(getDouble(rs, 21));
            record.setExpiretime(getTimeStamp(rs, 22));
            
            // add this VTECeventRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the VTECeventRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of VTECeventRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        VTECeventRecord record = null;

        // create a List to hold VTECevent Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM vtecevent " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a VTECeventRecord
            // and store its address in oneRecord
            record = new VTECeventRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a VTECeventRecord object

            record.setGeoid(getString(rs, 1));
            record.setProduct_id(getString(rs, 2));
            record.setProducttime(getTimeStamp(rs, 3));
            record.setProductmode(getString(rs, 4));
            record.setAction(getString(rs, 5));
            record.setOffice_id(getString(rs, 6));
            record.setPhenom(getString(rs, 7));
            record.setSignif(getString(rs, 8));
            record.setEtn(getShort(rs, 9));
            record.setBegintime(getTimeStamp(rs, 10));
            record.setEndtime(getTimeStamp(rs, 11));
            record.setSeverity(getString(rs, 12));
            record.setImmed_cause(getString(rs, 13));
            record.setRisetime(getTimeStamp(rs, 14));
            record.setCresttime(getTimeStamp(rs, 15));
            record.setFalltime(getTimeStamp(rs, 16));
            record.setRecord(getString(rs, 17));
            record.setRisets(getString(rs, 18));
            record.setCrests(getString(rs, 19));
            record.setFallts(getString(rs, 20));
            record.setCrest_value(getDouble(rs, 21));
            record.setExpiretime(getTimeStamp(rs, 22));
            
            // add this VTECeventRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the VTECeventRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a VTECeventRecord object and..
//-----------------------------------------------------------------
    public int insert(VTECeventRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO vtecevent VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getGeoid());
        setString(insertStatement, 2, record.getProduct_id());
        setTimeStamp(insertStatement, 3, record.getProducttime());
        setString(insertStatement, 4, record.getProductmode());
        setString(insertStatement, 5, record.getAction());
        setString(insertStatement, 6, record.getOffice_id());
        setString(insertStatement, 7, record.getPhenom());
        setString(insertStatement, 8, record.getSignif());
        setShort(insertStatement, 9, record.getEtn());
        setTimeStamp(insertStatement, 10, record.getBegintime());
        setTimeStamp(insertStatement, 11, record.getEndtime());
        setString(insertStatement, 12, record.getSeverity());
        setString(insertStatement, 13, record.getImmed_cause());
        setTimeStamp(insertStatement, 14, record.getRisetime());
        setTimeStamp(insertStatement, 15, record.getCresttime());
        setTimeStamp(insertStatement, 16, record.getFalltime());
        setString(insertStatement, 17, record.getRecord());
        setString(insertStatement, 18, record.getRisets());
        setString(insertStatement, 19, record.getCrests());
        setString(insertStatement, 20, record.getFallts());
        setDouble(insertStatement, 21, record.getCrest_value());
        setTimeStamp(insertStatement, 22, record.getExpiretime());
        
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
        String deleteStatement = "DELETE FROM vtecevent " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a VTECeventRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(VTECeventRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE vtecevent SET geoid = ?, product_id = ?, producttime = ?, productmode = ?, action = ?, office_id = ?, phenom = ?, signif = ?, etn = ?, begintime = ?, endtime = ?, severity = ?, immed_cause = ?, risetime = ?, cresttime = ?, falltime = ?, record = ?, risets = ?, crests = ?, fallts = ?, crest_value = ?, expiretime = ?        " + where );

        setString(updateStatement, 1, record.getGeoid());
        setString(updateStatement, 2, record.getProduct_id());
        setTimeStamp(updateStatement, 3, record.getProducttime());
        setString(updateStatement, 4, record.getProductmode());
        setString(updateStatement, 5, record.getAction());
        setString(updateStatement, 6, record.getOffice_id());
        setString(updateStatement, 7, record.getPhenom());
        setString(updateStatement, 8, record.getSignif());
        setShort(updateStatement, 9, record.getEtn());
        setTimeStamp(updateStatement, 10, record.getBegintime());
        setTimeStamp(updateStatement, 11, record.getEndtime());
        setString(updateStatement, 12, record.getSeverity());
        setString(updateStatement, 13, record.getImmed_cause());
        setTimeStamp(updateStatement, 14, record.getRisetime());
        setTimeStamp(updateStatement, 15, record.getCresttime());
        setTimeStamp(updateStatement, 16, record.getFalltime());
        setString(updateStatement, 17, record.getRecord());
        setString(updateStatement, 18, record.getRisets());
        setString(updateStatement, 19, record.getCrests());
        setString(updateStatement, 20, record.getFallts());
        setDouble(updateStatement, 21, record.getCrest_value());
        setTimeStamp(updateStatement, 22, record.getExpiretime());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(VTECeventRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM vtecevent " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a VTECeventRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(VTECeventRecord oldRecord, VTECeventRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE vtecevent SET geoid = ?, product_id = ?, producttime = ?, productmode = ?, action = ?, office_id = ?, phenom = ?, signif = ?, etn = ?, begintime = ?, endtime = ?, severity = ?, immed_cause = ?, risetime = ?, cresttime = ?, falltime = ?, record = ?, risets = ?, crests = ?, fallts = ?, crest_value = ?, expiretime = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getGeoid());
        setString(updateStatement, 2, newRecord.getProduct_id());
        setTimeStamp(updateStatement, 3, newRecord.getProducttime());
        setString(updateStatement, 4, newRecord.getProductmode());
        setString(updateStatement, 5, newRecord.getAction());
        setString(updateStatement, 6, newRecord.getOffice_id());
        setString(updateStatement, 7, newRecord.getPhenom());
        setString(updateStatement, 8, newRecord.getSignif());
        setShort(updateStatement, 9, newRecord.getEtn());
        setTimeStamp(updateStatement, 10, newRecord.getBegintime());
        setTimeStamp(updateStatement, 11, newRecord.getEndtime());
        setString(updateStatement, 12, newRecord.getSeverity());
        setString(updateStatement, 13, newRecord.getImmed_cause());
        setTimeStamp(updateStatement, 14, newRecord.getRisetime());
        setTimeStamp(updateStatement, 15, newRecord.getCresttime());
        setTimeStamp(updateStatement, 16, newRecord.getFalltime());
        setString(updateStatement, 17, newRecord.getRecord());
        setString(updateStatement, 18, newRecord.getRisets());
        setString(updateStatement, 19, newRecord.getCrests());
        setString(updateStatement, 20, newRecord.getFallts());
        setDouble(updateStatement, 21, newRecord.getCrest_value());
        setTimeStamp(updateStatement, 22, newRecord.getExpiretime());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a VTECeventRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(VTECeventRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            VTECeventRecord oldRecord = (VTECeventRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of VTECeventTable class
