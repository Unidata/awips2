// filename: RWBiasDynTable.java
// author  : DBGEN
// created : Tue May 31 17:52:27 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              rwbiasdyn table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class RWBiasDynTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  RWBiasDynTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public RWBiasDynTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("rwbiasdyn");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of RWBiasDynRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        RWBiasDynRecord record = null;

        // create a List to hold RWBiasDyn Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM rwbiasdyn " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RWBiasDynRecord
            // and store its address in oneRecord
            record = new RWBiasDynRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RWBiasDynRecord object

            record.setRadid(getString(rs, 1));
            record.setOffice_id(getString(rs, 2));
            record.setObstime(getTimeStamp(rs, 3));
            record.setMemspan_ind(getShort(rs, 4));
            record.setNumpairs(getDouble(rs, 5));
            record.setSumgag(getReal(rs, 6));
            record.setSumrad(getReal(rs, 7));
            record.setBias(getReal(rs, 8));
            
            // add this RWBiasDynRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the RWBiasDynRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of RWBiasDynRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        RWBiasDynRecord record = null;

        // create a List to hold RWBiasDyn Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM rwbiasdyn " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RWBiasDynRecord
            // and store its address in oneRecord
            record = new RWBiasDynRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RWBiasDynRecord object

            record.setRadid(getString(rs, 1));
            record.setOffice_id(getString(rs, 2));
            record.setObstime(getTimeStamp(rs, 3));
            record.setMemspan_ind(getShort(rs, 4));
            record.setNumpairs(getDouble(rs, 5));
            record.setSumgag(getReal(rs, 6));
            record.setSumrad(getReal(rs, 7));
            record.setBias(getReal(rs, 8));
            
            // add this RWBiasDynRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the RWBiasDynRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a RWBiasDynRecord object and..
//-----------------------------------------------------------------
    public int insert(RWBiasDynRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO rwbiasdyn VALUES (?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getRadid());
        setString(insertStatement, 2, record.getOffice_id());
        setTimeStamp(insertStatement, 3, record.getObstime());
        setShort(insertStatement, 4, record.getMemspan_ind());
        setDouble(insertStatement, 5, record.getNumpairs());
        setReal(insertStatement, 6, record.getSumgag());
        setReal(insertStatement, 7, record.getSumrad());
        setReal(insertStatement, 8, record.getBias());
        
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
        String deleteStatement = "DELETE FROM rwbiasdyn " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RWBiasDynRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RWBiasDynRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE rwbiasdyn SET radid = ?, office_id = ?, obstime = ?, memspan_ind = ?, numpairs = ?, sumgag = ?, sumrad = ?, bias = ?        " + where );

        setString(updateStatement, 1, record.getRadid());
        setString(updateStatement, 2, record.getOffice_id());
        setTimeStamp(updateStatement, 3, record.getObstime());
        setShort(updateStatement, 4, record.getMemspan_ind());
        setDouble(updateStatement, 5, record.getNumpairs());
        setReal(updateStatement, 6, record.getSumgag());
        setReal(updateStatement, 7, record.getSumrad());
        setReal(updateStatement, 8, record.getBias());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(RWBiasDynRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM rwbiasdyn " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RWBiasDynRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RWBiasDynRecord oldRecord, RWBiasDynRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE rwbiasdyn SET radid = ?, office_id = ?, obstime = ?, memspan_ind = ?, numpairs = ?, sumgag = ?, sumrad = ?, bias = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getRadid());
        setString(updateStatement, 2, newRecord.getOffice_id());
        setTimeStamp(updateStatement, 3, newRecord.getObstime());
        setShort(updateStatement, 4, newRecord.getMemspan_ind());
        setDouble(updateStatement, 5, newRecord.getNumpairs());
        setReal(updateStatement, 6, newRecord.getSumgag());
        setReal(updateStatement, 7, newRecord.getSumrad());
        setReal(updateStatement, 8, newRecord.getBias());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a RWBiasDynRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(RWBiasDynRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            RWBiasDynRecord oldRecord = (RWBiasDynRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of RWBiasDynTable class
