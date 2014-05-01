// filename: FcstHorizonRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:14 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              FcstHorizon table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class FcstHorizonRecord extends DbRecord
{
    private String fcst_horizon;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public FcstHorizonRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public FcstHorizonRecord(FcstHorizonRecord origRecord)
    {
        setFcst_horizon(origRecord.getFcst_horizon());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a FcstHorizon record

    //-----------------------------------------------------------------
    public String getFcst_horizon()
    {
        return fcst_horizon;
    }

    public void setFcst_horizon(String fcst_horizon)
    {
        this.fcst_horizon = fcst_horizon ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE fcst_horizon = '" + fcst_horizon + "'" 
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
                getFcst_horizon() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of FcstHorizonRecord class

