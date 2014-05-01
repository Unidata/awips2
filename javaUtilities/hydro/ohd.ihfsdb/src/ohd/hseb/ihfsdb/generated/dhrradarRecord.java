// filename: dhrradarRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:14 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              dhrradar table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class dhrradarRecord extends DbRecord
{
    private String radid;

    private long obstime;

    private short volcovpat;

    private short opermode;

    private float dbzmin;

    private float dbzinc;

    private float dbzcnt;

    private short j_date;

    private short j_time;

    private short mean_field_bias;

    private short sample_size;

    private String grid_filename;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public dhrradarRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public dhrradarRecord(dhrradarRecord origRecord)
    {
        setRadid(origRecord.getRadid());
        setObstime(origRecord.getObstime());
        setVolcovpat(origRecord.getVolcovpat());
        setOpermode(origRecord.getOpermode());
        setDbzmin(origRecord.getDbzmin());
        setDbzinc(origRecord.getDbzinc());
        setDbzcnt(origRecord.getDbzcnt());
        setJ_date(origRecord.getJ_date());
        setJ_time(origRecord.getJ_time());
        setMean_field_bias(origRecord.getMean_field_bias());
        setSample_size(origRecord.getSample_size());
        setGrid_filename(origRecord.getGrid_filename());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a dhrradar record

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

    public float getDbzmin()
    {
        return dbzmin;
    }

    public void setDbzmin(float dbzmin)
    {
        this.dbzmin = dbzmin ;
    }

    public float getDbzinc()
    {
        return dbzinc;
    }

    public void setDbzinc(float dbzinc)
    {
        this.dbzinc = dbzinc ;
    }

    public float getDbzcnt()
    {
        return dbzcnt;
    }

    public void setDbzcnt(float dbzcnt)
    {
        this.dbzcnt = dbzcnt ;
    }

    public short getJ_date()
    {
        return j_date;
    }

    public void setJ_date(short j_date)
    {
        this.j_date = j_date ;
    }

    public short getJ_time()
    {
        return j_time;
    }

    public void setJ_time(short j_time)
    {
        this.j_time = j_time ;
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
                getDbzmin() + " " +
                getDbzinc() + " " +
                getDbzcnt() + " " +
                getJ_date() + " " +
                getJ_time() + " " +
                getMean_field_bias() + " " +
                getSample_size() + " " +
                getGrid_filename() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of dhrradarRecord class

