// filename: ProximityRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Proximity table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class ProximityRecord extends DbRecord
{
    private String proximity;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public ProximityRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public ProximityRecord(ProximityRecord origRecord)
    {
        setProximity(origRecord.getProximity());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Proximity record

    //-----------------------------------------------------------------
    public String getProximity()
    {
        return proximity;
    }

    public void setProximity(String proximity)
    {
        this.proximity = proximity ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE proximity = '" + proximity + "'" 
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
                getProximity() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of ProximityRecord class

