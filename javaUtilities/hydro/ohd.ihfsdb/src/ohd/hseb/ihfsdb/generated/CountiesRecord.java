// filename: CountiesRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:13 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Counties table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class CountiesRecord extends DbRecord
{
    private String county;

    private String state;

    private String countynum;

    private String wfo;

    private String primary_back;

    private String secondary_back;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public CountiesRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public CountiesRecord(CountiesRecord origRecord)
    {
        setCounty(origRecord.getCounty());
        setState(origRecord.getState());
        setCountynum(origRecord.getCountynum());
        setWfo(origRecord.getWfo());
        setPrimary_back(origRecord.getPrimary_back());
        setSecondary_back(origRecord.getSecondary_back());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Counties record

    //-----------------------------------------------------------------
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

    public String getCountynum()
    {
        return countynum;
    }

    public void setCountynum(String countynum)
    {
        this.countynum = countynum ;
    }

    public String getWfo()
    {
        return wfo;
    }

    public void setWfo(String wfo)
    {
        this.wfo = wfo ;
    }

    public String getPrimary_back()
    {
        return primary_back;
    }

    public void setPrimary_back(String primary_back)
    {
        this.primary_back = primary_back ;
    }

    public String getSecondary_back()
    {
        return secondary_back;
    }

    public void setSecondary_back(String secondary_back)
    {
        this.secondary_back = secondary_back ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE county = '" + county + "'" 
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
                getCounty() + " " +
                getState() + " " +
                getCountynum() + " " +
                getWfo() + " " +
                getPrimary_back() + " " +
                getSecondary_back() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of CountiesRecord class

