// filename: RWBiasStatTable.java
// author  : DBGEN
// created : Tue May 31 17:52:27 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into the
//              rwbiasstat table of an IHFS database
//
package ohd.hseb.ihfsdb.generated;

import java.sql.*;

import java.util.*;

import ohd.hseb.db.*;

public class RWBiasStatTable extends DbTable
{
//-----------------------------------------------------------------
//  Private data
//-----------------------------------------------------------------
    private int _recordsFound = -1;
//-----------------------------------------------------------------
//  RWBiasStatTable() - constructor to set statement variable and initialize
//		number of records found to zero
//-----------------------------------------------------------------
    public RWBiasStatTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("rwbiasstat");
    }


    //-----------------------------------------------------------------
    //  select() - this method is called with a where clause and returns
    //		a List of RWBiasStatRecord objects
    //-----------------------------------------------------------------
    public List select(String where) throws SQLException
    {
        RWBiasStatRecord record = null;

        // create a List to hold RWBiasStat Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM rwbiasstat " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RWBiasStatRecord
            // and store its address in oneRecord
            record = new RWBiasStatRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RWBiasStatRecord object

            record.setOffice_id(getString(rs, 1));
            record.setMin_gr_value_bias(getReal(rs, 2));
            record.setNpair_bias_select(getInt(rs, 3));
            record.setNpair_svar_update(getInt(rs, 4));
            record.setStd_cut(getInt(rs, 5));
            record.setLag_cut(getInt(rs, 6));
            record.setInit_span(getInt(rs, 7));
            record.setBias_qc_opt(getInt(rs, 8));
            record.setNum_span(getInt(rs, 9));
            record.setMem_span1(getReal(rs, 10));
            record.setMem_span2(getReal(rs, 11));
            record.setMem_span3(getReal(rs, 12));
            record.setMem_span4(getReal(rs, 13));
            record.setMem_span5(getReal(rs, 14));
            record.setMem_span6(getReal(rs, 15));
            record.setMem_span7(getReal(rs, 16));
            record.setMem_span8(getReal(rs, 17));
            record.setMem_span9(getReal(rs, 18));
            record.setMem_span10(getReal(rs, 19));
            
            // add this RWBiasStatRecord object to the list
            recordList.add(record);
        }
        // Close the result set
        rs.close();

        // return a List which holds the RWBiasStatRecord objects
        return recordList;

    } // end of select method

    //-----------------------------------------------------------------
    //  selectNRecords() - this method is called with a where clause and returns
    //		a List filled with a maximum of maxRecordCount of RWBiasStatRecord objects 
    //-----------------------------------------------------------------
    public List selectNRecords(String where, int maxRecordCount) throws SQLException
    {
        RWBiasStatRecord record = null;

        // create a List to hold RWBiasStat Records
        List recordList = new ArrayList();

        // set number of records found to zero
        _recordsFound = 0;

        // Create the SQL statement and issue it
        // construct the select statment
        String selectStatement = "SELECT * FROM rwbiasstat " + where;

        // get the result set back from the query to the database
        ResultSet rs = getStatement().executeQuery(selectStatement);

        // loop through the result set
        while (rs.next())
        {
            // create an instance of a RWBiasStatRecord
            // and store its address in oneRecord
            record = new RWBiasStatRecord();

            // increment the number of records found
            _recordsFound++;

            // assign the data returned to the result set for one
            // record in the database to a RWBiasStatRecord object

            record.setOffice_id(getString(rs, 1));
            record.setMin_gr_value_bias(getReal(rs, 2));
            record.setNpair_bias_select(getInt(rs, 3));
            record.setNpair_svar_update(getInt(rs, 4));
            record.setStd_cut(getInt(rs, 5));
            record.setLag_cut(getInt(rs, 6));
            record.setInit_span(getInt(rs, 7));
            record.setBias_qc_opt(getInt(rs, 8));
            record.setNum_span(getInt(rs, 9));
            record.setMem_span1(getReal(rs, 10));
            record.setMem_span2(getReal(rs, 11));
            record.setMem_span3(getReal(rs, 12));
            record.setMem_span4(getReal(rs, 13));
            record.setMem_span5(getReal(rs, 14));
            record.setMem_span6(getReal(rs, 15));
            record.setMem_span7(getReal(rs, 16));
            record.setMem_span8(getReal(rs, 17));
            record.setMem_span9(getReal(rs, 18));
            record.setMem_span10(getReal(rs, 19));
            
            // add this RWBiasStatRecord object to the list
            recordList.add(record);
            if (_recordsFound >= maxRecordCount)
            {
                break;
            }
        }
        // Close the result set
        rs.close();

        // return a List which holds the RWBiasStatRecord objects
        return recordList;

    } // end of selectNRecords method

//-----------------------------------------------------------------
//  insert() - this method is called with a RWBiasStatRecord object and..
//-----------------------------------------------------------------
    public int insert(RWBiasStatRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
" INSERT INTO rwbiasstat VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?        )");

        setString(insertStatement, 1, record.getOffice_id());
        setReal(insertStatement, 2, record.getMin_gr_value_bias());
        setInt(insertStatement, 3, record.getNpair_bias_select());
        setInt(insertStatement, 4, record.getNpair_svar_update());
        setInt(insertStatement, 5, record.getStd_cut());
        setInt(insertStatement, 6, record.getLag_cut());
        setInt(insertStatement, 7, record.getInit_span());
        setInt(insertStatement, 8, record.getBias_qc_opt());
        setInt(insertStatement, 9, record.getNum_span());
        setReal(insertStatement, 10, record.getMem_span1());
        setReal(insertStatement, 11, record.getMem_span2());
        setReal(insertStatement, 12, record.getMem_span3());
        setReal(insertStatement, 13, record.getMem_span4());
        setReal(insertStatement, 14, record.getMem_span5());
        setReal(insertStatement, 15, record.getMem_span6());
        setReal(insertStatement, 16, record.getMem_span7());
        setReal(insertStatement, 17, record.getMem_span8());
        setReal(insertStatement, 18, record.getMem_span9());
        setReal(insertStatement, 19, record.getMem_span10());
        
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
        String deleteStatement = "DELETE FROM rwbiasstat " + where;

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RWBiasStatRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RWBiasStatRecord record, String where)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE rwbiasstat SET office_id = ?, min_gr_value_bias = ?, npair_bias_select = ?, npair_svar_update = ?, std_cut = ?, lag_cut = ?, init_span = ?, bias_qc_opt = ?, num_span = ?, mem_span1 = ?, mem_span2 = ?, mem_span3 = ?, mem_span4 = ?, mem_span5 = ?, mem_span6 = ?, mem_span7 = ?, mem_span8 = ?, mem_span9 = ?, mem_span10 = ?        " + where );

        setString(updateStatement, 1, record.getOffice_id());
        setReal(updateStatement, 2, record.getMin_gr_value_bias());
        setInt(updateStatement, 3, record.getNpair_bias_select());
        setInt(updateStatement, 4, record.getNpair_svar_update());
        setInt(updateStatement, 5, record.getStd_cut());
        setInt(updateStatement, 6, record.getLag_cut());
        setInt(updateStatement, 7, record.getInit_span());
        setInt(updateStatement, 8, record.getBias_qc_opt());
        setInt(updateStatement, 9, record.getNum_span());
        setReal(updateStatement, 10, record.getMem_span1());
        setReal(updateStatement, 11, record.getMem_span2());
        setReal(updateStatement, 12, record.getMem_span3());
        setReal(updateStatement, 13, record.getMem_span4());
        setReal(updateStatement, 14, record.getMem_span5());
        setReal(updateStatement, 15, record.getMem_span6());
        setReal(updateStatement, 16, record.getMem_span7());
        setReal(updateStatement, 17, record.getMem_span8());
        setReal(updateStatement, 18, record.getMem_span9());
        setReal(updateStatement, 19, record.getMem_span10());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  delete() - this method is called with a where clause and returns
//                   the number of records deleted
//-----------------------------------------------------------------
    public int delete(RWBiasStatRecord record) throws SQLException
    {
        int returnCode=-999;

        // Create a SQL delete statement and issue it
        // construct the delete statement
        String deleteStatement = "DELETE FROM rwbiasstat " + record.getWhereString();

        // get the number of records processed by the delete
        returnCode = getStatement().executeUpdate(deleteStatement);

        return returnCode;
    } // end of delete method 

//-----------------------------------------------------------------
//  update() - this method is called with a RWBiasStatRecord object and a where clause..
//-----------------------------------------------------------------
    public int update(RWBiasStatRecord oldRecord, RWBiasStatRecord newRecord)  throws SQLException
    {
        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        PreparedStatement updateStatement = getConnection().prepareStatement(
" UPDATE rwbiasstat SET office_id = ?, min_gr_value_bias = ?, npair_bias_select = ?, npair_svar_update = ?, std_cut = ?, lag_cut = ?, init_span = ?, bias_qc_opt = ?, num_span = ?, mem_span1 = ?, mem_span2 = ?, mem_span3 = ?, mem_span4 = ?, mem_span5 = ?, mem_span6 = ?, mem_span7 = ?, mem_span8 = ?, mem_span9 = ?, mem_span10 = ?        " + oldRecord.getWhereString() );

        setString(updateStatement, 1, newRecord.getOffice_id());
        setReal(updateStatement, 2, newRecord.getMin_gr_value_bias());
        setInt(updateStatement, 3, newRecord.getNpair_bias_select());
        setInt(updateStatement, 4, newRecord.getNpair_svar_update());
        setInt(updateStatement, 5, newRecord.getStd_cut());
        setInt(updateStatement, 6, newRecord.getLag_cut());
        setInt(updateStatement, 7, newRecord.getInit_span());
        setInt(updateStatement, 8, newRecord.getBias_qc_opt());
        setInt(updateStatement, 9, newRecord.getNum_span());
        setReal(updateStatement, 10, newRecord.getMem_span1());
        setReal(updateStatement, 11, newRecord.getMem_span2());
        setReal(updateStatement, 12, newRecord.getMem_span3());
        setReal(updateStatement, 13, newRecord.getMem_span4());
        setReal(updateStatement, 14, newRecord.getMem_span5());
        setReal(updateStatement, 15, newRecord.getMem_span6());
        setReal(updateStatement, 16, newRecord.getMem_span7());
        setReal(updateStatement, 17, newRecord.getMem_span8());
        setReal(updateStatement, 18, newRecord.getMem_span9());
        setReal(updateStatement, 19, newRecord.getMem_span10());
        // get the number of records processed by the update
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a RWBiasStatRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------
    public int insertOrUpdate(RWBiasStatRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            RWBiasStatRecord oldRecord = (RWBiasStatRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate() 
} // end of RWBiasStatTable class
