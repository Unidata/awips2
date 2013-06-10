// filename: RpfFcstGroupRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              RpfFcstGroup table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class RpfFcstGroupRecord extends DbRecord
{
    private String group_id;

    private String group_name;

    private int ordinal;

    private String rec_all_included;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public RpfFcstGroupRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public RpfFcstGroupRecord(RpfFcstGroupRecord origRecord)
    {
        setGroup_id(origRecord.getGroup_id());
        setGroup_name(origRecord.getGroup_name());
        setOrdinal(origRecord.getOrdinal());
        setRec_all_included(origRecord.getRec_all_included());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a RpfFcstGroup record

    //-----------------------------------------------------------------
    public String getGroup_id()
    {
        return group_id;
    }

    public void setGroup_id(String group_id)
    {
        this.group_id = group_id ;
    }

    public String getGroup_name()
    {
        return group_name;
    }

    public void setGroup_name(String group_name)
    {
        this.group_name = group_name ;
    }

    public int getOrdinal()
    {
        return ordinal;
    }

    public void setOrdinal(int ordinal)
    {
        this.ordinal = ordinal ;
    }

    public String getRec_all_included()
    {
        return rec_all_included;
    }

    public void setRec_all_included(String rec_all_included)
    {
        this.rec_all_included = rec_all_included ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE group_id = '" + group_id + "'" 
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
                getGroup_id() + " " +
                getGroup_name() + " " +
                getOrdinal() + " " +
                getRec_all_included() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of RpfFcstGroupRecord class

