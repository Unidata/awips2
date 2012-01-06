// filename: velocityRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              velocity table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class velocityRecord extends DbRecord
{
    private String mosaicid;

    private long createtime;

    private double umean;

    private double vmean;

    private short count;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public velocityRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public velocityRecord(velocityRecord origRecord)
    {
        setMosaicid(origRecord.getMosaicid());
        setCreatetime(origRecord.getCreatetime());
        setUmean(origRecord.getUmean());
        setVmean(origRecord.getVmean());
        setCount(origRecord.getCount());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a velocity record

    //-----------------------------------------------------------------
    public String getMosaicid()
    {
        return mosaicid;
    }

    public void setMosaicid(String mosaicid)
    {
        this.mosaicid = mosaicid ;
    }

    public long getCreatetime()
    {
        return createtime;
    }

    public void setCreatetime(long createtime)
    {
        this.createtime = createtime ;
    }

    public double getUmean()
    {
        return umean;
    }

    public void setUmean(double umean)
    {
        this.umean = umean ;
    }

    public double getVmean()
    {
        return vmean;
    }

    public void setVmean(double vmean)
    {
        this.vmean = vmean ;
    }

    public short getCount()
    {
        return count;
    }

    public void setCount(short count)
    {
        this.count = count ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE mosaicid = '" + mosaicid + "'" 
                 + " AND createtime = '" +  getDateTimeStringFromLongTime(createtime) + "'" 
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
                getMosaicid() + " " +
                getDateTimeStringFromLongTime(getCreatetime()) + " " +
                getUmean() + " " +
                getVmean() + " " +
                getCount() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of velocityRecord class

