// filename: WatSupMethodRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              WatSupMethod table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class WatSupMethodRecord extends DbRecord
{
    private String watsup_method;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public WatSupMethodRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public WatSupMethodRecord(WatSupMethodRecord origRecord)
    {
        setWatsup_method(origRecord.getWatsup_method());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a WatSupMethod record

    //-----------------------------------------------------------------
    public String getWatsup_method()
    {
        return watsup_method;
    }

    public void setWatsup_method(String watsup_method)
    {
        this.watsup_method = watsup_method ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE watsup_method = '" + watsup_method + "'" 
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
                getWatsup_method() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of WatSupMethodRecord class

