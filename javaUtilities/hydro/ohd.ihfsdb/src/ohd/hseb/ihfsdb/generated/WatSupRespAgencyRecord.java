// filename: WatSupRespAgencyRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              WatSupRespAgency table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class WatSupRespAgencyRecord extends DbRecord
{
    private String watsup_resp_agency;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public WatSupRespAgencyRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public WatSupRespAgencyRecord(WatSupRespAgencyRecord origRecord)
    {
        setWatsup_resp_agency(origRecord.getWatsup_resp_agency());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a WatSupRespAgency record

    //-----------------------------------------------------------------
    public String getWatsup_resp_agency()
    {
        return watsup_resp_agency;
    }

    public void setWatsup_resp_agency(String watsup_resp_agency)
    {
        this.watsup_resp_agency = watsup_resp_agency ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE watsup_resp_agency = '" + watsup_resp_agency + "'" 
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
                getWatsup_resp_agency() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of WatSupRespAgencyRecord class

