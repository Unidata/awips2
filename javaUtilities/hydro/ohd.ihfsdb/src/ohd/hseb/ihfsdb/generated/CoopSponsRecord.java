// filename: CoopSponsRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:13 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              CoopSpons table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class CoopSponsRecord extends DbRecord
{
    private String spons;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public CoopSponsRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public CoopSponsRecord(CoopSponsRecord origRecord)
    {
        setSpons(origRecord.getSpons());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a CoopSpons record

    //-----------------------------------------------------------------
    public String getSpons()
    {
        return spons;
    }

    public void setSpons(String spons)
    {
        this.spons = spons ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE spons = '" + spons + "'" 
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
                getSpons() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of CoopSponsRecord class

