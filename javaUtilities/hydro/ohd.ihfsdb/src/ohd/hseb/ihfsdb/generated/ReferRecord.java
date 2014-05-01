// filename: ReferRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Refer table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class ReferRecord extends DbRecord
{
    private String lid;

    private String reference;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public ReferRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public ReferRecord(ReferRecord origRecord)
    {
        setLid(origRecord.getLid());
        setReference(origRecord.getReference());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Refer record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getReference()
    {
        return reference;
    }

    public void setReference(String reference)
    {
        this.reference = reference ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE lid = '" + lid + "'" 
                 + " AND reference = '" + reference + "'" 
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
                getLid() + " " +
                getReference() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of ReferRecord class

