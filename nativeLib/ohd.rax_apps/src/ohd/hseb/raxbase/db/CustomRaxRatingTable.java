package ohd.hseb.raxbase.db;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.List;

import ohd.hseb.db.Database;
import ohd.hseb.raxdb.generated.RaxRatingRecord;
import ohd.hseb.raxdb.generated.RaxRatingTable;

public class CustomRaxRatingTable extends RaxRatingTable
{
//  -----------------------------------------------------------------
//  RaxRatingTable() - constructor to set statement variable and initialize
//      number of records found to zero
//-----------------------------------------------------------------
    public CustomRaxRatingTable(Database database) 
    {
        //Constructor calls DbTable's constructor
        super(database);
        setTableName("rating");
    }

    private String createArrayString(String origArrayString)
    {
        String arrayString = "null";
        
        if (origArrayString != null)
        {
            arrayString =  "'" + origArrayString + "'";
        }
        
        return arrayString;
    }
    
//  -----------------------------------------------------------------
//  insert() - this method is called with a RaxRatingRecord object and..
//-----------------------------------------------------------------
    public int insert(RaxRatingRecord record)  throws SQLException
    {
        int returnCode=-999;

        // Create a SQL insert statement and issue it
        // construct the insert statement
        PreparedStatement insertStatement = getConnection().prepareStatement(
                " INSERT INTO rating VALUES (?, ?, ?, ?, ?, ?, ?, ?, " +
                createArrayString(record.getStgflow()) + ", ?, ?, " +
                createArrayString(record.getOffsets()) + ", ? )");

    //    " UPDATE rating SET lid = ?, pe1 = ?, pe2 = ?, tbl = ?, valid_date = ?, src = ?, othagid = ?, rfs_input = ?, stgflow = " + newRecord.getStgflow() + " , units = ?, interpolate = ?, offsets = + " + newRecord.getOffsets() + "  allowstg = ? " + oldRecord.getWhereString() );

        setString(insertStatement, 1, record.getLid());
        setString(insertStatement, 2, record.getPe1());
        setString(insertStatement, 3, record.getPe2());
        setDouble(insertStatement, 4, record.getTbl());
        setTimeStamp(insertStatement, 5, record.getValid_date());
        setString(insertStatement, 6, record.getSrc());
        setString(insertStatement, 7, record.getOthagid());
        setString(insertStatement, 8, record.getRfs_input());

       // setString(insertStatement, 9, record.getStgflow());
        setString(insertStatement, 9, record.getUnits());
        setString(insertStatement, 10, record.getInterpolate());
       // setString(insertStatement, 12, record.getOffsets());
        setDouble(insertStatement, 11, record.getAllowstg());
 /*
        setString(insertStatement, 9, record.getStgflow());
        setString(insertStatement, 10, record.getUnits());
        setString(insertStatement, 11, record.getInterpolate());
        setString(insertStatement, 12, record.getOffsets());
        setDouble(insertStatement, 13, record.getAllowstg());
   */
        // get the number of records processed by the insert
        returnCode = insertStatement.executeUpdate();

        return returnCode;

    } // end of insert method


 public int update(RaxRatingRecord oldRecord, RaxRatingRecord newRecord)  throws SQLException
    {
        String header = "CustomRaxRatingTable.update(oldRecord, newRecord): ";

        int returnCode=-999;
        // Create a SQL update statement and issue it
        // construct the update statement
        

        System.out.println(header + "newRecord.getStgflow() = " + newRecord.getStgflow());
        
        String updateStatementString = " UPDATE rating SET lid = ?, pe1 = ?, pe2 = ?, tbl = ?, valid_date = ?, src = ?, othagid = ?, rfs_input = ?, stgflow = " + 
                                      createArrayString(newRecord.getStgflow()) +
                                      ", units = ?, interpolate = ?, offsets = " + 
                                       createArrayString(newRecord.getOffsets()) + 
                                       ",  allowstg = ? " + oldRecord.getWhereString(); 
        
        PreparedStatement updateStatement = getConnection().prepareStatement( updateStatementString );


        String newOffsetsString = null;

        setString(updateStatement, 1, newRecord.getLid());
        setString(updateStatement, 2, newRecord.getPe1());
        setString(updateStatement, 3, newRecord.getPe2());
        setDouble(updateStatement, 4, newRecord.getTbl());
        setTimeStamp(updateStatement, 5, newRecord.getValid_date());
        setString(updateStatement, 6, newRecord.getSrc());
        setString(updateStatement, 7, newRecord.getOthagid());
        setString(updateStatement, 8, newRecord.getRfs_input());
        setString(updateStatement, 9, newRecord.getUnits());
        setString(updateStatement, 10, newRecord.getInterpolate());
        setDouble(updateStatement, 11, newRecord.getAllowstg());

    //    setArray(updateStatement, 9,  newRecord.getStgflow());
    //    setString(updateStatement, 10, newRecord.getUnits());
    //    setString(updateStatement, 11, newRecord.getInterpolate());
    //    setArray(updateStatement, 12, newRecord.getOffsets());
    //    setDouble(updateStatement, 13, newRecord.getAllowstg());
        // get the number of records processed by the update


        System.out.println(header + " newRecord.getStgflow() = " +   newRecord.getStgflow());
        System.out.println(header + " updateStatement = " + updateStatement);
        returnCode = updateStatement.executeUpdate();

        return returnCode;

    } // end of updateRecord method

//-----------------------------------------------------------------
//  insertOrUpdate() - this method is call with a RaxRatingRecord object.
//                   the number of records inserted or updated
//-----------------------------------------------------------------


    public int insertOrUpdate(RaxRatingRecord record) throws SQLException
    {
        int returnCode=-999;
        List recordList = select(record.getWhereString());

        if (recordList.size() < 1)
        {
            returnCode = insert(record);
        }
        else
        {
            RaxRatingRecord oldRecord = (RaxRatingRecord) recordList.get(0);
            returnCode = update(oldRecord, record);
        }
        return returnCode;
    } // end of insertOrUpdate()
}