// filename: CoopRecipRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:13 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              CoopRecip table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class CoopRecipRecord extends DbRecord
{
    private String recip;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public CoopRecipRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public CoopRecipRecord(CoopRecipRecord origRecord)
    {
        setRecip(origRecord.getRecip());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a CoopRecip record

    //-----------------------------------------------------------------
    public String getRecip()
    {
        return recip;
    }

    public void setRecip(String recip)
    {
        this.recip = recip ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE recip = '" + recip + "'" 
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
                getRecip() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of CoopRecipRecord class

