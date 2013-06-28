// filename: HsaRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:15 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Hsa table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class HsaRecord extends DbRecord
{
    private String hsa;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public HsaRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public HsaRecord(HsaRecord origRecord)
    {
        setHsa(origRecord.getHsa());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Hsa record

    //-----------------------------------------------------------------
    public String getHsa()
    {
        return hsa;
    }

    public void setHsa(String hsa)
    {
        this.hsa = hsa ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE hsa = '" + hsa + "'" 
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
                getHsa() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of HsaRecord class

