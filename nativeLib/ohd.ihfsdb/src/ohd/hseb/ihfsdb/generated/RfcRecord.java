// filename: RfcRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Rfc table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class RfcRecord extends DbRecord
{
    private String rfc;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public RfcRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public RfcRecord(RfcRecord origRecord)
    {
        setRfc(origRecord.getRfc());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Rfc record

    //-----------------------------------------------------------------
    public String getRfc()
    {
        return rfc;
    }

    public void setRfc(String rfc)
    {
        this.rfc = rfc ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE rfc = '" + rfc + "'" 
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
                getRfc() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of RfcRecord class

