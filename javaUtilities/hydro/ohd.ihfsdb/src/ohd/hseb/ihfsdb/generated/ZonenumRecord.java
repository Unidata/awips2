// filename: ZonenumRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Zonenum table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class ZonenumRecord extends DbRecord
{
    private String lid;

    private String state;

    private String zonenum;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public ZonenumRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public ZonenumRecord(ZonenumRecord origRecord)
    {
        setLid(origRecord.getLid());
        setState(origRecord.getState());
        setZonenum(origRecord.getZonenum());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Zonenum record

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

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE lid = '" + lid + "'" 
                 + " AND state = '" + state + "'" 
                 + " AND zonenum = '" + zonenum + "'" 
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
                getLid() + " " +
                getState() + " " +
                getZonenum() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of ZonenumRecord class

