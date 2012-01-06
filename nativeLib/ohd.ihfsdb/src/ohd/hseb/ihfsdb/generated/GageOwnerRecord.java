// filename: GageOwnerRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:15 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              GageOwner table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class GageOwnerRecord extends DbRecord
{
    private String owner;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public GageOwnerRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public GageOwnerRecord(GageOwnerRecord origRecord)
    {
        setOwner(origRecord.getOwner());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a GageOwner record

    //-----------------------------------------------------------------
    public String getOwner()
    {
        return owner;
    }

    public void setOwner(String owner)
    {
        this.owner = owner ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE owner = '" + owner + "'" 
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
                getOwner() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of GageOwnerRecord class

