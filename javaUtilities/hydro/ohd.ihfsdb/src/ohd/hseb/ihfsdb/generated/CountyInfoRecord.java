// This is a view record !
// filename: CountyInfoRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              CountyInfo table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class CountyInfoRecord extends DbRecord
{
    private String lid;

    private String state;

    private String county;

    private String countynum;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public CountyInfoRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public CountyInfoRecord(CountyInfoRecord origRecord)
    {
        setLid(origRecord.getLid());
        setState(origRecord.getState());
        setCounty(origRecord.getCounty());
        setCountynum(origRecord.getCountynum());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a CountyInfo record

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

    public String getCountynum()
    {
        return countynum;
    }

    public void setCountynum(String countynum)
    {
        this.countynum = countynum ;
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
                getCounty() + " " +
                getCountynum() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of CountyInfoRecord class

