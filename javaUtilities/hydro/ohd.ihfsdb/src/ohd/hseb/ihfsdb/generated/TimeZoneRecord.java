// filename: TimeZoneRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              TimeZone table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class TimeZoneRecord extends DbRecord
{
    private String tzone;

    private String name;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public TimeZoneRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public TimeZoneRecord(TimeZoneRecord origRecord)
    {
        setTzone(origRecord.getTzone());
        setName(origRecord.getName());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a TimeZone record

    //-----------------------------------------------------------------
    public String getTzone()
    {
        return tzone;
    }

    public void setTzone(String tzone)
    {
        this.tzone = tzone ;
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE tzone = '" + tzone + "'" 
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
                getTzone() + " " +
                getName() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of TimeZoneRecord class

