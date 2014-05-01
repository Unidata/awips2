// filename: dspradarRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:14 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              dspradar table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class dspradarRecord extends DbRecord
{
    private String radid;

    private long obstime;

    private short volcovpat;

    private short opermode;

    private float minval;

    private float maxval;

    private float num_data_lev;

    private float scale_factor;

    private long begin_time;

    private long end_time;

    private short j_beg_date;

    private short j_beg_time;

    private short j_end_date;

    private short j_end_time;

    private short mean_field_bias;

    private short sample_size;

    private String grid_filename;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public dspradarRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public dspradarRecord(dspradarRecord origRecord)
    {
        setRadid(origRecord.getRadid());
        setObstime(origRecord.getObstime());
        setVolcovpat(origRecord.getVolcovpat());
        setOpermode(origRecord.getOpermode());
        setMinval(origRecord.getMinval());
        setMaxval(origRecord.getMaxval());
        setNum_data_lev(origRecord.getNum_data_lev());
        setScale_factor(origRecord.getScale_factor());
        setBegin_time(origRecord.getBegin_time());
        setEnd_time(origRecord.getEnd_time());
        setJ_beg_date(origRecord.getJ_beg_date());
        setJ_beg_time(origRecord.getJ_beg_time());
        setJ_end_date(origRecord.getJ_end_date());
        setJ_end_time(origRecord.getJ_end_time());
        setMean_field_bias(origRecord.getMean_field_bias());
        setSample_size(origRecord.getSample_size());
        setGrid_filename(origRecord.getGrid_filename());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a dspradar record

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

    public short getVolcovpat()
    {
        return volcovpat;
    }

    public void setVolcovpat(short volcovpat)
    {
        this.volcovpat = volcovpat ;
    }

    public short getOpermode()
    {
        return opermode;
    }

    public void setOpermode(short opermode)
    {
        this.opermode = opermode ;
    }

    public float getMinval()
    {
        return minval;
    }

    public void setMinval(float minval)
    {
        this.minval = minval ;
    }

    public float getMaxval()
    {
        return maxval;
    }

    public void setMaxval(float maxval)
    {
        this.maxval = maxval ;
    }

    public float getNum_data_lev()
    {
        return num_data_lev;
    }

    public void setNum_data_lev(float num_data_lev)
    {
        this.num_data_lev = num_data_lev ;
    }

    public float getScale_factor()
    {
        return scale_factor;
    }

    public void setScale_factor(float scale_factor)
    {
        this.scale_factor = scale_factor ;
    }

    public long getBegin_time()
    {
        return begin_time;
    }

    public void setBegin_time(long begin_time)
    {
        this.begin_time = begin_time ;
    }

    public long getEnd_time()
    {
        return end_time;
    }

    public void setEnd_time(long end_time)
    {
        this.end_time = end_time ;
    }

    public short getJ_beg_date()
    {
        return j_beg_date;
    }

    public void setJ_beg_date(short j_beg_date)
    {
        this.j_beg_date = j_beg_date ;
    }

    public short getJ_beg_time()
    {
        return j_beg_time;
    }

    public void setJ_beg_time(short j_beg_time)
    {
        this.j_beg_time = j_beg_time ;
    }

    public short getJ_end_date()
    {
        return j_end_date;
    }

    public void setJ_end_date(short j_end_date)
    {
        this.j_end_date = j_end_date ;
    }

    public short getJ_end_time()
    {
        return j_end_time;
    }

    public void setJ_end_time(short j_end_time)
    {
        this.j_end_time = j_end_time ;
    }

    public short getMean_field_bias()
    {
        return mean_field_bias;
    }

    public void setMean_field_bias(short mean_field_bias)
    {
        this.mean_field_bias = mean_field_bias ;
    }

    public short getSample_size()
    {
        return sample_size;
    }

    public void setSample_size(short sample_size)
    {
        this.sample_size = sample_size ;
    }

    public String getGrid_filename()
    {
        return grid_filename;
    }

    public void setGrid_filename(String grid_filename)
    {
        this.grid_filename = grid_filename ;
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
                getVolcovpat() + " " +
                getOpermode() + " " +
                getMinval() + " " +
                getMaxval() + " " +
                getNum_data_lev() + " " +
                getScale_factor() + " " +
                getDateTimeStringFromLongTime(getBegin_time()) + " " +
                getDateTimeStringFromLongTime(getEnd_time()) + " " +
                getJ_beg_date() + " " +
                getJ_beg_time() + " " +
                getJ_end_date() + " " +
                getJ_end_time() + " " +
                getMean_field_bias() + " " +
                getSample_size() + " " +
                getGrid_filename() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of dspradarRecord class

