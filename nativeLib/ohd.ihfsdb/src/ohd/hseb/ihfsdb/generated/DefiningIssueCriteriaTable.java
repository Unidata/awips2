// filename: DefiningIssueCriteriaTable.java
// author  : DBGEN
// created : Tue May 31 17:52:20 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              definingissuecriteria table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class DefiningIssueCriteriaTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  DefiningIssueCriteriaTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public DefiningIssueCriteriaTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("definingissuecriteria");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of DefiningIssueCriteriaRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        DefiningIssueCriteriaRecord record = null;

        // create a List to hold DefiningIssueCriteria Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM definingissuecriteria " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a DefiningIssueCriteriaRecord
            // and store its address in oneRecord
            record = new DefiningIssueCriteriaRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a DefiningIssueCriteriaRecord object

            record.setDef_issue_crit(getString(rs, 1));
            
            // add this DefiningIssueCriteriaRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the DefiningIssueCriteriaRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of DefiningIssueCriteriaRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        DefiningIssueCriteriaRecord record = null;

        // create a List to hold DefiningIssueCriteria Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM definingissuecriteria " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a DefiningIssueCriteriaRecord
            // and store its address in oneRecord
            record = new DefiningIssueCriteriaRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a DefiningIssueCriteriaRecord object

            record.setDef_issue_crit(getString(rs, 1));
            
            // add this DefiningIssueCriteriaRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the DefiningIssueCriteriaRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a DefiningIssueCriteriaRecord object and..
//-----------------------------------------------------------------
    public int insert(DefiningIssueCriteriaRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO definingissuecriteria VALUES (?        )");

        setString(insertStatement, 1, record.getDef_issue_crit());
        
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
        String deleteStatement = "DELETE FROM definingissuecriteria " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a DefiningIssueCriteriaRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(DefiningIssueCriteriaRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE definingissuecriteria SET def_issue_crit = ?        " + where );

        setString(updateStatement, 1, record.getDef_issue_crit());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(DefiningIssueCriteriaRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM definingissuecriteria " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a DefiningIssueCriteriaRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(DefiningIssueCriteriaRecord oldRecord, DefiningIssueCriteriaRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE definingissuecriteria SET def_issue_crit = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getDef_issue_crit());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a DefiningIssueCriteriaRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(DefiningIssueCriteriaRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            DefiningIssueCriteriaRecord oldRecord = (DefiningIssueCriteriaRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of DefiningIssueCriteriaTable class
