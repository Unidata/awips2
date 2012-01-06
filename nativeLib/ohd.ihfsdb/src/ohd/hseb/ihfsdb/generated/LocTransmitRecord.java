// This is a view record !
// filename: LocTransmitRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              LocTransmit table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class LocTransmitRecord extends DbRecord
{
    private String lid;

    private String call_sign;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public LocTransmitRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public LocTransmitRecord(LocTransmitRecord origRecord)
    {
        setLid(origRecord.getLid());
        setCall_sign(origRecord.getCall_sign());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a LocTransmit record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getCall_sign()
    {
        return call_sign;
    }

    public void setCall_sign(String call_sign)
    {
        this.call_sign = call_sign ;
    }

//-----------------------------------------------------------------
//  toString() - this method is called with no arguments
//  and returns a String of the internal values
//-----------------------------------------------------------------
    public String toString()
    {
        String outString = 
                getLid() + " " +
                getCall_sign() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of LocTransmitRecord class

