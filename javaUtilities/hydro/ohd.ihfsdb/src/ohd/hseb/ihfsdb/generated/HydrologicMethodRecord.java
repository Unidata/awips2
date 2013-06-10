// filename: HydrologicMethodRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:15 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              HydrologicMethod table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class HydrologicMethodRecord extends DbRecord
{
    private String hydrol_method;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public HydrologicMethodRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public HydrologicMethodRecord(HydrologicMethodRecord origRecord)
    {
        setHydrol_method(origRecord.getHydrol_method());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a HydrologicMethod record

    //-----------------------------------------------------------------
    public String getHydrol_method()
    {
        return hydrol_method;
    }

    public void setHydrol_method(String hydrol_method)
    {
        this.hydrol_method = hydrol_method ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE hydrol_method = '" + hydrol_method + "'" 
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
                getHydrol_method() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of HydrologicMethodRecord class

