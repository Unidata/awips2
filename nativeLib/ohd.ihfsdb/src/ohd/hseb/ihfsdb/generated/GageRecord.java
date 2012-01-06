// filename: GageRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:15 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Gage table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class GageRecord extends DbRecord
{
    private String lid;

    private long gbegin;

    private String type;

    private long gend;

    private String remark;

    private String maint;

    private String owner;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public GageRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public GageRecord(GageRecord origRecord)
    {
        setLid(origRecord.getLid());
        setGbegin(origRecord.getGbegin());
        setType(origRecord.getType());
        setGend(origRecord.getGend());
        setRemark(origRecord.getRemark());
        setMaint(origRecord.getMaint());
        setOwner(origRecord.getOwner());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Gage record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public long getGbegin()
    {
        return gbegin;
    }

    public void setGbegin(long gbegin)
    {
        this.gbegin = gbegin ;
    }

    public String getType()
    {
        return type;
    }

    public void setType(String type)
    {
        this.type = type ;
    }

    public long getGend()
    {
        return gend;
    }

    public void setGend(long gend)
    {
        this.gend = gend ;
    }

    public String getRemark()
    {
        return remark;
    }

    public void setRemark(String remark)
    {
        this.remark = remark ;
    }

    public String getMaint()
    {
        return maint;
    }

    public void setMaint(String maint)
    {
        this.maint = maint ;
    }

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
                "WHERE lid = '" + lid + "'" 
                 + " AND gbegin = '" +  getDateStringFromLongTime(gbegin) + "'" 
                 + " AND type = '" + type + "'" 
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
                getDateStringFromLongTime(getGbegin()) + " " +
                getType() + " " +
                getDateStringFromLongTime(getGend()) + " " +
                getRemark() + " " +
                getMaint() + " " +
                getOwner() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of GageRecord class

