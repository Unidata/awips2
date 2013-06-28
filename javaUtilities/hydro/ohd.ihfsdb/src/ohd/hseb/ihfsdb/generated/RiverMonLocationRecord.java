// filename: RiverMonLocationRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              RiverMonLocation table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class RiverMonLocationRecord extends DbRecord
{
    private String lid;

    private String group_id;

    private int ordinal;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public RiverMonLocationRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public RiverMonLocationRecord(RiverMonLocationRecord origRecord)
    {
        setLid(origRecord.getLid());
        setGroup_id(origRecord.getGroup_id());
        setOrdinal(origRecord.getOrdinal());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a RiverMonLocation record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getGroup_id()
    {
        return group_id;
    }

    public void setGroup_id(String group_id)
    {
        this.group_id = group_id ;
    }

    public int getOrdinal()
    {
        return ordinal;
    }

    public void setOrdinal(int ordinal)
    {
        this.ordinal = ordinal ;
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
                getGroup_id() + " " +
                getOrdinal() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of RiverMonLocationRecord class

