// filename: SnowMethodRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              SnowMethod table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class SnowMethodRecord extends DbRecord
{
    private String snow_method;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public SnowMethodRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public SnowMethodRecord(SnowMethodRecord origRecord)
    {
        setSnow_method(origRecord.getSnow_method());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a SnowMethod record

    //-----------------------------------------------------------------
    public String getSnow_method()
    {
        return snow_method;
    }

    public void setSnow_method(String snow_method)
    {
        this.snow_method = snow_method ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE snow_method = '" + snow_method + "'" 
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
                getSnow_method() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of SnowMethodRecord class

