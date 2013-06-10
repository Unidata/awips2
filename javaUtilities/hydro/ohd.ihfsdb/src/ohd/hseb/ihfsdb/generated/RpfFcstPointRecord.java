// filename: RpfFcstPointRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              RpfFcstPoint table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class RpfFcstPointRecord extends DbRecord
{
    private String lid;

    private String group_id;

    private int ordinal;

    private double chg_threshold;

    private String rec_type;

    private String primary_back;

    private String secondary_back;

    private int backhrs;

    private int forwardhrs;

    private double adjustendhrs;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public RpfFcstPointRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public RpfFcstPointRecord(RpfFcstPointRecord origRecord)
    {
        setLid(origRecord.getLid());
        setGroup_id(origRecord.getGroup_id());
        setOrdinal(origRecord.getOrdinal());
        setChg_threshold(origRecord.getChg_threshold());
        setRec_type(origRecord.getRec_type());
        setPrimary_back(origRecord.getPrimary_back());
        setSecondary_back(origRecord.getSecondary_back());
        setBackhrs(origRecord.getBackhrs());
        setForwardhrs(origRecord.getForwardhrs());
        setAdjustendhrs(origRecord.getAdjustendhrs());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a RpfFcstPoint record

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

    public double getChg_threshold()
    {
        return chg_threshold;
    }

    public void setChg_threshold(double chg_threshold)
    {
        this.chg_threshold = chg_threshold ;
    }

    public String getRec_type()
    {
        return rec_type;
    }

    public void setRec_type(String rec_type)
    {
        this.rec_type = rec_type ;
    }

    public String getPrimary_back()
    {
        return primary_back;
    }

    public void setPrimary_back(String primary_back)
    {
        this.primary_back = primary_back ;
    }

    public String getSecondary_back()
    {
        return secondary_back;
    }

    public void setSecondary_back(String secondary_back)
    {
        this.secondary_back = secondary_back ;
    }

    public int getBackhrs()
    {
        return backhrs;
    }

    public void setBackhrs(int backhrs)
    {
        this.backhrs = backhrs ;
    }

    public int getForwardhrs()
    {
        return forwardhrs;
    }

    public void setForwardhrs(int forwardhrs)
    {
        this.forwardhrs = forwardhrs ;
    }

    public double getAdjustendhrs()
    {
        return adjustendhrs;
    }

    public void setAdjustendhrs(double adjustendhrs)
    {
        this.adjustendhrs = adjustendhrs ;
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
                getChg_threshold() + " " +
                getRec_type() + " " +
                getPrimary_back() + " " +
                getSecondary_back() + " " +
                getBackhrs() + " " +
                getForwardhrs() + " " +
                getAdjustendhrs() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of RpfFcstPointRecord class

