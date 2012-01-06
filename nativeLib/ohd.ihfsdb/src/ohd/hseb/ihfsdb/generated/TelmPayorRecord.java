// filename: TelmPayorRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              TelmPayor table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class TelmPayorRecord extends DbRecord
{
    private String payor;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public TelmPayorRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public TelmPayorRecord(TelmPayorRecord origRecord)
    {
        setPayor(origRecord.getPayor());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a TelmPayor record

    //-----------------------------------------------------------------
    public String getPayor()
    {
        return payor;
    }

    public void setPayor(String payor)
    {
        this.payor = payor ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE payor = '" + payor + "'" 
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
                getPayor() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of TelmPayorRecord class

