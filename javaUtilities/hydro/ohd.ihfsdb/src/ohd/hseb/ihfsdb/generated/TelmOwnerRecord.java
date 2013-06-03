// filename: TelmOwnerRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              TelmOwner table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class TelmOwnerRecord extends DbRecord
{
    private String owner;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public TelmOwnerRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public TelmOwnerRecord(TelmOwnerRecord origRecord)
    {
        setOwner(origRecord.getOwner());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a TelmOwner record

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
} // end of TelmOwnerRecord class

