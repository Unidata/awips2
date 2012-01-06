// filename: PurgeDynDataRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              PurgeDynData table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class PurgeDynDataRecord extends DbRecord
{
    private String table_name;

    private String time_column_name;

    private int num_hours_host;

    private int num_hours_backup;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public PurgeDynDataRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public PurgeDynDataRecord(PurgeDynDataRecord origRecord)
    {
        setTable_name(origRecord.getTable_name());
        setTime_column_name(origRecord.getTime_column_name());
        setNum_hours_host(origRecord.getNum_hours_host());
        setNum_hours_backup(origRecord.getNum_hours_backup());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a PurgeDynData record

    //-----------------------------------------------------------------
    public String getTable_name()
    {
        return table_name;
    }

    public void setTable_name(String table_name)
    {
        this.table_name = table_name ;
    }

    public String getTime_column_name()
    {
        return time_column_name;
    }

    public void setTime_column_name(String time_column_name)
    {
        this.time_column_name = time_column_name ;
    }

    public int getNum_hours_host()
    {
        return num_hours_host;
    }

    public void setNum_hours_host(int num_hours_host)
    {
        this.num_hours_host = num_hours_host ;
    }

    public int getNum_hours_backup()
    {
        return num_hours_backup;
    }

    public void setNum_hours_backup(int num_hours_backup)
    {
        this.num_hours_backup = num_hours_backup ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE table_name = '" + table_name + "'" 
                ;
        return outString;
    } // end toString()
//-----------------------------------------------------------------
//  toString() - this method is called with no arguments
//  and returns a String of the internal values
//-----------------------------------------------------------------
    public String toString()
    {
        String outString = 
                getTable_name() + " " +
                getTime_column_name() + " " +
                getNum_hours_host() + " " +
                getNum_hours_backup() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of PurgeDynDataRecord class

