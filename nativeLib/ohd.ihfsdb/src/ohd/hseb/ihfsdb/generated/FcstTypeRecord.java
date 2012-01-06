// filename: FcstTypeRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:15 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              FcstType table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class FcstTypeRecord extends DbRecord
{
    private String fcsttype;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public FcstTypeRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public FcstTypeRecord(FcstTypeRecord origRecord)
    {
        setFcsttype(origRecord.getFcsttype());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a FcstType record

    //-----------------------------------------------------------------
    public String getFcsttype()
    {
        return fcsttype;
    }

    public void setFcsttype(String fcsttype)
    {
        this.fcsttype = fcsttype ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE fcsttype = '" + fcsttype + "'" 
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
                getFcsttype() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of FcstTypeRecord class

