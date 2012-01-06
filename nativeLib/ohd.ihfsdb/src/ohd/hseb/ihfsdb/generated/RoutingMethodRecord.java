// filename: RoutingMethodRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              RoutingMethod table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class RoutingMethodRecord extends DbRecord
{
    private String hydraul_method;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public RoutingMethodRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public RoutingMethodRecord(RoutingMethodRecord origRecord)
    {
        setHydraul_method(origRecord.getHydraul_method());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a RoutingMethod record

    //-----------------------------------------------------------------
    public String getHydraul_method()
    {
        return hydraul_method;
    }

    public void setHydraul_method(String hydraul_method)
    {
        this.hydraul_method = hydraul_method ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE hydraul_method = '" + hydraul_method + "'" 
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
                getHydraul_method() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of RoutingMethodRecord class

