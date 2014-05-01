// filename: PseudoGageValTable.java
// author  : DBGEN
// created : Tue May 31 17:52:26 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              pseudogageval table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class PseudoGageValTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  PseudoGageValTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public PseudoGageValTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("pseudogageval");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of PseudoGageValRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        PseudoGageValRecord record = null;

        // create a List to hold PseudoGageVal Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM pseudogageval " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a PseudoGageValRecord
            // and store its address in oneRecord
            record = new PseudoGageValRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a PseudoGageValRecord object

            record.setPseudo_gage_id(getString(rs, 1));
            record.setObstime(getTimeStamp(rs, 2));
            record.setLat(getDouble(rs, 3));
            record.setLon(getDouble(rs, 4));
            record.setGage_value(getReal(rs, 5));
            record.setMan_edited(getString(rs, 6));
            record.setPrev_gage_value(getReal(rs, 7));
            
            // add this PseudoGageValRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the PseudoGageValRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of PseudoGageValRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        PseudoGageValRecord record = null;

        // create a List to hold PseudoGageVal Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM pseudogageval " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a PseudoGageValRecord
            // and store its address in oneRecord
            record = new PseudoGageValRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a PseudoGageValRecord object

            record.setPseudo_gage_id(getString(rs, 1));
            record.setObstime(getTimeStamp(rs, 2));
            record.setLat(getDouble(rs, 3));
            record.setLon(getDouble(rs, 4));
            record.setGage_value(getReal(rs, 5));
            record.setMan_edited(getString(rs, 6));
            record.setPrev_gage_value(getReal(rs, 7));
            
            // add this PseudoGageValRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the PseudoGageValRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a PseudoGageValRecord object and..
//-----------------------------------------------------------------
    public int insert(PseudoGageValRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO pseudogageval VALUES (?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getPseudo_gage_id());
        setTimeStamp(insertStatement, 2, record.getObstime());
        setDouble(insertStatement, 3, record.getLat());
        setDouble(insertStatement, 4, record.getLon());
        setReal(insertStatement, 5, record.getGage_value());
        setString(insertStatement, 6, record.getMan_edited());
        setReal(insertStatement, 7, record.getPrev_gage_value());
        
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
        String deleteStatement = "DELETE FROM pseudogageval " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a PseudoGageValRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(PseudoGageValRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE pseudogageval SET pseudo_gage_id = ?, obstime = ?, lat = ?, lon = ?, gage_value = ?, man_edited = ?, prev_gage_value = ?        " + where );

        setString(updateStatement, 1, record.getPseudo_gage_id());
        setTimeStamp(updateStatement, 2, record.getObstime());
        setDouble(updateStatement, 3, record.getLat());
        setDouble(updateStatement, 4, record.getLon());
        setReal(updateStatement, 5, record.getGage_value());
        setString(updateStatement, 6, record.getMan_edited());
        setReal(updateStatement, 7, record.getPrev_gage_value());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(PseudoGageValRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM pseudogageval " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a PseudoGageValRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(PseudoGageValRecord oldRecord, PseudoGageValRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE pseudogageval SET pseudo_gage_id = ?, obstime = ?, lat = ?, lon = ?, gage_value = ?, man_edited = ?, prev_gage_value = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getPseudo_gage_id());
        setTimeStamp(updateStatement, 2, newRecord.getObstime());
        setDouble(updateStatement, 3, newRecord.getLat());
        setDouble(updateStatement, 4, newRecord.getLon());
        setReal(updateStatement, 5, newRecord.getGage_value());
        setString(updateStatement, 6, newRecord.getMan_edited());
        setReal(updateStatement, 7, newRecord.getPrev_gage_value());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a PseudoGageValRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(PseudoGageValRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            PseudoGageValRecord oldRecord = (PseudoGageValRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of PseudoGageValTable class
