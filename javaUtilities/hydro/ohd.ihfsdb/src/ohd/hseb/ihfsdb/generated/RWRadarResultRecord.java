// filename: RWRadarResultRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              RWRadarResult table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class RWRadarResultRecord extends DbRecord
{
    private String radid;

    private long obstime;

    private short num_gages;

    private String rad_avail;

    private double rw_bias_val_used;

    private double mem_span_used;

    private String edit_bias;

    private String ignore_radar;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public RWRadarResultRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public RWRadarResultRecord(RWRadarResultRecord origRecord)
    {
        setRadid(origRecord.getRadid());
        setObstime(origRecord.getObstime());
        setNum_gages(origRecord.getNum_gages());
        setRad_avail(origRecord.getRad_avail());
        setRw_bias_val_used(origRecord.getRw_bias_val_used());
        setMem_span_used(origRecord.getMem_span_used());
        setEdit_bias(origRecord.getEdit_bias());
        setIgnore_radar(origRecord.getIgnore_radar());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a RWRadarResult record

    //-----------------------------------------------------------------
    public String getRadid()
    {
        return radid;
    }

    public void setRadid(String radid)
    {
        this.radid = radid ;
    }

    public long getObstime()
    {
        return obstime;
    }

    public void setObstime(long obstime)
    {
        this.obstime = obstime ;
    }

    public short getNum_gages()
    {
        return num_gages;
    }

    public void setNum_gages(short num_gages)
    {
        this.num_gages = num_gages ;
    }

    public String getRad_avail()
    {
        return rad_avail;
    }

    public void setRad_avail(String rad_avail)
    {
        this.rad_avail = rad_avail ;
    }

    public double getRw_bias_val_used()
    {
        return rw_bias_val_used;
    }

    public void setRw_bias_val_used(double rw_bias_val_used)
    {
        this.rw_bias_val_used = rw_bias_val_used ;
    }

    public double getMem_span_used()
    {
        return mem_span_used;
    }

    public void setMem_span_used(double mem_span_used)
    {
        this.mem_span_used = mem_span_used ;
    }

    public String getEdit_bias()
    {
        return edit_bias;
    }

    public void setEdit_bias(String edit_bias)
    {
        this.edit_bias = edit_bias ;
    }

    public String getIgnore_radar()
    {
        return ignore_radar;
    }

    public void setIgnore_radar(String ignore_radar)
    {
        this.ignore_radar = ignore_radar ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE radid = '" + radid + "'" 
                 + " AND obstime = '" +  getDateTimeStringFromLongTime(obstime) + "'" 
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
                getRadid() + " " +
                getDateTimeStringFromLongTime(getObstime()) + " " +
                getNum_gages() + " " +
                getRad_avail() + " " +
                getRw_bias_val_used() + " " +
                getMem_span_used() + " " +
                getEdit_bias() + " " +
                getIgnore_radar() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of RWRadarResultRecord class

