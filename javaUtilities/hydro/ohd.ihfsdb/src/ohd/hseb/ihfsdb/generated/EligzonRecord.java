// filename: EligzonRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:14 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Eligzon table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class EligzonRecord extends DbRecord
{
    private String state;

    private String zonenum;

    private String descr;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public EligzonRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public EligzonRecord(EligzonRecord origRecord)
    {
        setState(origRecord.getState());
        setZonenum(origRecord.getZonenum());
        setDescr(origRecord.getDescr());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Eligzon record

    //-----------------------------------------------------------------
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
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE state = '" + state + "'" 
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
                getState() + " " +
                getZonenum() + " " +
                getDescr() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of EligzonRecord class

