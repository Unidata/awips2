// filename: CountynumRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:13 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Countynum table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class CountynumRecord extends DbRecord
{
    private String lid;

    private String state;

    private String county;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public CountynumRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public CountynumRecord(CountynumRecord origRecord)
    {
        setLid(origRecord.getLid());
        setState(origRecord.getState());
        setCounty(origRecord.getCounty());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Countynum record

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

    public String getCounty()
    {
        return county;
    }

    public void setCounty(String county)
    {
        this.county = county ;
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
                 + " AND county = '" + county + "'" 
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
                getCounty() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of CountynumRecord class

