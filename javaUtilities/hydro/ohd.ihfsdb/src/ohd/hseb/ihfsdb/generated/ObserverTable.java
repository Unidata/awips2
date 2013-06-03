// filename: ObserverTable.java
// author  : DBGEN
// created : Tue May 31 17:52:25 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              observer table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class ObserverTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  ObserverTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public ObserverTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("observer");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of ObserverRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        ObserverRecord record = null;

        // create a List to hold Observer Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM observer " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a ObserverRecord
            // and store its address in oneRecord
            record = new ObserverRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a ObserverRecord object

            record.setLid(getString(rs, 1));
            record.setA1(getString(rs, 2));
            record.setA2(getString(rs, 3));
            record.setA3(getString(rs, 4));
            record.setCity(getString(rs, 5));
            record.setState(getString(rs, 6));
            record.setZip(getString(rs, 7));
            record.setComm(getString(rs, 8));
            record.setDos(getDate(rs, 9));
            record.setGn(getString(rs, 10));
            record.setHphone(getString(rs, 11));
            record.setFirstname(getString(rs, 12));
            record.setLastname(getString(rs, 13));
            record.setPhone(getString(rs, 14));
            record.setEmail(getString(rs, 15));
            record.setOrnr(getString(rs, 16));
            record.setRate(getDouble(rs, 17));
            record.setRecip(getString(rs, 18));
            record.setRprt(getString(rs, 19));
            record.setSpons(getString(rs, 20));
            record.setSsn(getString(rs, 21));
            record.setTsk(getString(rs, 22));
            
            // add this ObserverRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the ObserverRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of ObserverRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        ObserverRecord record = null;

        // create a List to hold Observer Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM observer " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a ObserverRecord
            // and store its address in oneRecord
            record = new ObserverRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a ObserverRecord object

            record.setLid(getString(rs, 1));
            record.setA1(getString(rs, 2));
            record.setA2(getString(rs, 3));
            record.setA3(getString(rs, 4));
            record.setCity(getString(rs, 5));
            record.setState(getString(rs, 6));
            record.setZip(getString(rs, 7));
            record.setComm(getString(rs, 8));
            record.setDos(getDate(rs, 9));
            record.setGn(getString(rs, 10));
            record.setHphone(getString(rs, 11));
            record.setFirstname(getString(rs, 12));
            record.setLastname(getString(rs, 13));
            record.setPhone(getString(rs, 14));
            record.setEmail(getString(rs, 15));
            record.setOrnr(getString(rs, 16));
            record.setRate(getDouble(rs, 17));
            record.setRecip(getString(rs, 18));
            record.setRprt(getString(rs, 19));
            record.setSpons(getString(rs, 20));
            record.setSsn(getString(rs, 21));
            record.setTsk(getString(rs, 22));
            
            // add this ObserverRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the ObserverRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a ObserverRecord object and..
//-----------------------------------------------------------------
    public int insert(ObserverRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO observer VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getA1());
        setString(insertStatement, 3, record.getA2());
        setString(insertStatement, 4, record.getA3());
        setString(insertStatement, 5, record.getCity());
        setString(insertStatement, 6, record.getState());
        setString(insertStatement, 7, record.getZip());
        setString(insertStatement, 8, record.getComm());
        setDate(insertStatement, 9, record.getDos());
        setString(insertStatement, 10, record.getGn());
        setString(insertStatement, 11, record.getHphone());
        setString(insertStatement, 12, record.getFirstname());
        setString(insertStatement, 13, record.getLastname());
        setString(insertStatement, 14, record.getPhone());
        setString(insertStatement, 15, record.getEmail());
        setString(insertStatement, 16, record.getOrnr());
        setDouble(insertStatement, 17, record.getRate());
        setString(insertStatement, 18, record.getRecip());
        setString(insertStatement, 19, record.getRprt());
        setString(insertStatement, 20, record.getSpons());
        setString(insertStatement, 21, record.getSsn());
        setString(insertStatement, 22, record.getTsk());
        
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
        String deleteStatement = "DELETE FROM observer " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a ObserverRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(ObserverRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE observer SET lid = ?, a1 = ?, a2 = ?, a3 = ?, city = ?, state = ?, zip = ?, comm = ?, dos = ?, gn = ?, hphone = ?, firstname = ?, lastname = ?, phone = ?, email = ?, ornr = ?, rate = ?, recip = ?, rprt = ?, spons = ?, ssn = ?, tsk = ?        " + where );

        setString(updateStatement, 1, record.getLid());
        setString(updateStatement, 2, record.getA1());
        setString(updateStatement, 3, record.getA2());
        setString(updateStatement, 4, record.getA3());
        setString(updateStatement, 5, record.getCity());
        setString(updateStatement, 6, record.getState());
        setString(updateStatement, 7, record.getZip());
        setString(updateStatement, 8, record.getComm());
        setDate(updateStatement, 9, record.getDos());
        setString(updateStatement, 10, record.getGn());
        setString(updateStatement, 11, record.getHphone());
        setString(updateStatement, 12, record.getFirstname());
        setString(updateStatement, 13, record.getLastname());
        setString(updateStatement, 14, record.getPhone());
        setString(updateStatement, 15, record.getEmail());
        setString(updateStatement, 16, record.getOrnr());
        setDouble(updateStatement, 17, record.getRate());
        setString(updateStatement, 18, record.getRecip());
        setString(updateStatement, 19, record.getRprt());
        setString(updateStatement, 20, record.getSpons());
        setString(updateStatement, 21, record.getSsn());
        setString(updateStatement, 22, record.getTsk());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(ObserverRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM observer " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a ObserverRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(ObserverRecord oldRecord, ObserverRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE observer SET lid = ?, a1 = ?, a2 = ?, a3 = ?, city = ?, state = ?, zip = ?, comm = ?, dos = ?, gn = ?, hphone = ?, firstname = ?, lastname = ?, phone = ?, email = ?, ornr = ?, rate = ?, recip = ?, rprt = ?, spons = ?, ssn = ?, tsk = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getA1());
        setString(updateStatement, 3, newRecord.getA2());
        setString(updateStatement, 4, newRecord.getA3());
        setString(updateStatement, 5, newRecord.getCity());
        setString(updateStatement, 6, newRecord.getState());
        setString(updateStatement, 7, newRecord.getZip());
        setString(updateStatement, 8, newRecord.getComm());
        setDate(updateStatement, 9, newRecord.getDos());
        setString(updateStatement, 10, newRecord.getGn());
        setString(updateStatement, 11, newRecord.getHphone());
        setString(updateStatement, 12, newRecord.getFirstname());
        setString(updateStatement, 13, newRecord.getLastname());
        setString(updateStatement, 14, newRecord.getPhone());
        setString(updateStatement, 15, newRecord.getEmail());
        setString(updateStatement, 16, newRecord.getOrnr());
        setDouble(updateStatement, 17, newRecord.getRate());
        setString(updateStatement, 18, newRecord.getRecip());
        setString(updateStatement, 19, newRecord.getRprt());
        setString(updateStatement, 20, newRecord.getSpons());
        setString(updateStatement, 21, newRecord.getSsn());
        setString(updateStatement, 22, newRecord.getTsk());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a ObserverRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(ObserverRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            ObserverRecord oldRecord = (ObserverRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of ObserverTable class
