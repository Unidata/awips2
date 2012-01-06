// filename: wfoRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              wfo table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class wfoRecord extends DbRecord
{
    private String wfo;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public wfoRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public wfoRecord(wfoRecord origRecord)
    {
        setWfo(origRecord.getWfo());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a wfo record

    //-----------------------------------------------------------------
    public String getWfo()
    {
        return wfo;
    }

    public void setWfo(String wfo)
    {
        this.wfo = wfo ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE wfo = '" + wfo + "'" 
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
                getWfo() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of wfoRecord class

