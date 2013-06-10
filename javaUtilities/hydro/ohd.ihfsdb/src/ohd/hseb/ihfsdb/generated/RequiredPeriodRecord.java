// filename: RequiredPeriodRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              RequiredPeriod table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class RequiredPeriodRecord extends DbRecord
{
    private String period_req;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public RequiredPeriodRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public RequiredPeriodRecord(RequiredPeriodRecord origRecord)
    {
        setPeriod_req(origRecord.getPeriod_req());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a RequiredPeriod record

    //-----------------------------------------------------------------
    public String getPeriod_req()
    {
        return period_req;
    }

    public void setPeriod_req(String period_req)
    {
        this.period_req = period_req ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE period_req = '" + period_req + "'" 
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
                getPeriod_req() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of RequiredPeriodRecord class

