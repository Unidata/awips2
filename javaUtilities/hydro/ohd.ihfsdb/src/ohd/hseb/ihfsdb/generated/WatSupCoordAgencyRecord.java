// filename: WatSupCoordAgencyRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              WatSupCoordAgency table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class WatSupCoordAgencyRecord extends DbRecord
{
    private String watsup_coord_agency;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public WatSupCoordAgencyRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public WatSupCoordAgencyRecord(WatSupCoordAgencyRecord origRecord)
    {
        setWatsup_coord_agency(origRecord.getWatsup_coord_agency());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a WatSupCoordAgency record

    //-----------------------------------------------------------------
    public String getWatsup_coord_agency()
    {
        return watsup_coord_agency;
    }

    public void setWatsup_coord_agency(String watsup_coord_agency)
    {
        this.watsup_coord_agency = watsup_coord_agency ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE watsup_coord_agency = '" + watsup_coord_agency + "'" 
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
                getWatsup_coord_agency() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of WatSupCoordAgencyRecord class

