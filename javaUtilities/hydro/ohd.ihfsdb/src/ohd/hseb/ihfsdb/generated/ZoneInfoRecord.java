// This is a view record !
// filename: ZoneInfoRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              ZoneInfo table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class ZoneInfoRecord extends DbRecord
{
    private String lid;

    private String state;

    private String zonenum;

    private String descr;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public ZoneInfoRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public ZoneInfoRecord(ZoneInfoRecord origRecord)
    {
        setLid(origRecord.getLid());
        setState(origRecord.getState());
        setZonenum(origRecord.getZonenum());
        setDescr(origRecord.getDescr());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a ZoneInfo record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getState()
    {
        return state;
    }

    public void setState(String state)
    {
        this.state = state ;
    }

    public String getZonenum()
    {
        return zonenum;
    }

    public void setZonenum(String zonenum)
    {
        this.zonenum = zonenum ;
    }

    public String getDescr()
    {
        return descr;
    }

    public void setDescr(String descr)
    {
        this.descr = descr ;
    }

//-----------------------------------------------------------------
//  toString() - this method is called with no arguments
//  and returns a String of the internal values
//-----------------------------------------------------------------
    public String toString()
    {
        String outString = 
                getLid() + " " +
                getState() + " " +
                getZonenum() + " " +
                getDescr() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of ZoneInfoRecord class

