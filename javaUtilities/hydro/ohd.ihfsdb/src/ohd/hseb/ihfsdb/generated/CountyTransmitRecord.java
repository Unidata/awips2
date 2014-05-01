// filename: CountyTransmitRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:13 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              CountyTransmit table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class CountyTransmitRecord extends DbRecord
{
    private String call_sign;

    private String county;

    private String state;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public CountyTransmitRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public CountyTransmitRecord(CountyTransmitRecord origRecord)
    {
        setCall_sign(origRecord.getCall_sign());
        setCounty(origRecord.getCounty());
        setState(origRecord.getState());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a CountyTransmit record

    //-----------------------------------------------------------------
    public String getCall_sign()
    {
        return call_sign;
    }

    public void setCall_sign(String call_sign)
    {
        this.call_sign = call_sign ;
    }

    public String getCounty()
    {
        return county;
    }

    public void setCounty(String county)
    {
        this.county = county ;
    }

    public String getState()
    {
        return state;
    }

    public void setState(String state)
    {
        this.state = state ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE call_sign = '" + call_sign + "'" 
                 + " AND county = '" + county + "'" 
                 + " AND state = '" + state + "'" 
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
                getCall_sign() + " " +
                getCounty() + " " +
                getState() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of CountyTransmitRecord class

