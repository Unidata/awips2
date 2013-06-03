// filename: FcstGenMethodRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:14 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              FcstGenMethod table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class FcstGenMethodRecord extends DbRecord
{
    private String fcst_gen_method;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public FcstGenMethodRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public FcstGenMethodRecord(FcstGenMethodRecord origRecord)
    {
        setFcst_gen_method(origRecord.getFcst_gen_method());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a FcstGenMethod record

    //-----------------------------------------------------------------
    public String getFcst_gen_method()
    {
        return fcst_gen_method;
    }

    public void setFcst_gen_method(String fcst_gen_method)
    {
        this.fcst_gen_method = fcst_gen_method ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE fcst_gen_method = '" + fcst_gen_method + "'" 
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
                getFcst_gen_method() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of FcstGenMethodRecord class

