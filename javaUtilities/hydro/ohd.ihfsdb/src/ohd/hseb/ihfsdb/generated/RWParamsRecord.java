// filename: RWParamsRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              RWParams table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class RWParamsRecord extends DbRecord
{
    private float rw_min_rain;

    private float rw_sep_dist;

    private float rw_lag0_ind_corr;

    private float rw_lag0_cond_corr;

    private short num_near_gages;

    private short num_near_rad_bins;

    private float def_cond_var_rad;

    private float def_ind_corr_scl;

    private float def_cond_corr_scl;

    private float min_ind_corr_scl;

    private float min_cond_corr_scl;

    private float max_ind_corr_scl;

    private float max_cond_corr_scl;

    private short nn_srch_method;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public RWParamsRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public RWParamsRecord(RWParamsRecord origRecord)
    {
        setRw_min_rain(origRecord.getRw_min_rain());
        setRw_sep_dist(origRecord.getRw_sep_dist());
        setRw_lag0_ind_corr(origRecord.getRw_lag0_ind_corr());
        setRw_lag0_cond_corr(origRecord.getRw_lag0_cond_corr());
        setNum_near_gages(origRecord.getNum_near_gages());
        setNum_near_rad_bins(origRecord.getNum_near_rad_bins());
        setDef_cond_var_rad(origRecord.getDef_cond_var_rad());
        setDef_ind_corr_scl(origRecord.getDef_ind_corr_scl());
        setDef_cond_corr_scl(origRecord.getDef_cond_corr_scl());
        setMin_ind_corr_scl(origRecord.getMin_ind_corr_scl());
        setMin_cond_corr_scl(origRecord.getMin_cond_corr_scl());
        setMax_ind_corr_scl(origRecord.getMax_ind_corr_scl());
        setMax_cond_corr_scl(origRecord.getMax_cond_corr_scl());
        setNn_srch_method(origRecord.getNn_srch_method());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a RWParams record

    //-----------------------------------------------------------------
    public float getRw_min_rain()
    {
        return rw_min_rain;
    }

    public void setRw_min_rain(float rw_min_rain)
    {
        this.rw_min_rain = rw_min_rain ;
    }

    public float getRw_sep_dist()
    {
        return rw_sep_dist;
    }

    public void setRw_sep_dist(float rw_sep_dist)
    {
        this.rw_sep_dist = rw_sep_dist ;
    }

    public float getRw_lag0_ind_corr()
    {
        return rw_lag0_ind_corr;
    }

    public void setRw_lag0_ind_corr(float rw_lag0_ind_corr)
    {
        this.rw_lag0_ind_corr = rw_lag0_ind_corr ;
    }

    public float getRw_lag0_cond_corr()
    {
        return rw_lag0_cond_corr;
    }

    public void setRw_lag0_cond_corr(float rw_lag0_cond_corr)
    {
        this.rw_lag0_cond_corr = rw_lag0_cond_corr ;
    }

    public short getNum_near_gages()
    {
        return num_near_gages;
    }

    public void setNum_near_gages(short num_near_gages)
    {
        this.num_near_gages = num_near_gages ;
    }

    public short getNum_near_rad_bins()
    {
        return num_near_rad_bins;
    }

    public void setNum_near_rad_bins(short num_near_rad_bins)
    {
        this.num_near_rad_bins = num_near_rad_bins ;
    }

    public float getDef_cond_var_rad()
    {
        return def_cond_var_rad;
    }

    public void setDef_cond_var_rad(float def_cond_var_rad)
    {
        this.def_cond_var_rad = def_cond_var_rad ;
    }

    public float getDef_ind_corr_scl()
    {
        return def_ind_corr_scl;
    }

    public void setDef_ind_corr_scl(float def_ind_corr_scl)
    {
        this.def_ind_corr_scl = def_ind_corr_scl ;
    }

    public float getDef_cond_corr_scl()
    {
        return def_cond_corr_scl;
    }

    public void setDef_cond_corr_scl(float def_cond_corr_scl)
    {
        this.def_cond_corr_scl = def_cond_corr_scl ;
    }

    public float getMin_ind_corr_scl()
    {
        return min_ind_corr_scl;
    }

    public void setMin_ind_corr_scl(float min_ind_corr_scl)
    {
        this.min_ind_corr_scl = min_ind_corr_scl ;
    }

    public float getMin_cond_corr_scl()
    {
        return min_cond_corr_scl;
    }

    public void setMin_cond_corr_scl(float min_cond_corr_scl)
    {
        this.min_cond_corr_scl = min_cond_corr_scl ;
    }

    public float getMax_ind_corr_scl()
    {
        return max_ind_corr_scl;
    }

    public void setMax_ind_corr_scl(float max_ind_corr_scl)
    {
        this.max_ind_corr_scl = max_ind_corr_scl ;
    }

    public float getMax_cond_corr_scl()
    {
        return max_cond_corr_scl;
    }

    public void setMax_cond_corr_scl(float max_cond_corr_scl)
    {
        this.max_cond_corr_scl = max_cond_corr_scl ;
    }

    public short getNn_srch_method()
    {
        return nn_srch_method;
    }

    public void setNn_srch_method(short nn_srch_method)
    {
        this.nn_srch_method = nn_srch_method ;
    }

//-----------------------------------------------------------------
//  toString() - this method is called with no arguments
//  and returns a String of the internal values
//-----------------------------------------------------------------
    public String toString()
    {
        String outString = 
                getRw_min_rain() + " " +
                getRw_sep_dist() + " " +
                getRw_lag0_ind_corr() + " " +
                getRw_lag0_cond_corr() + " " +
                getNum_near_gages() + " " +
                getNum_near_rad_bins() + " " +
                getDef_cond_var_rad() + " " +
                getDef_ind_corr_scl() + " " +
                getDef_cond_corr_scl() + " " +
                getMin_ind_corr_scl() + " " +
                getMin_cond_corr_scl() + " " +
                getMax_ind_corr_scl() + " " +
                getMax_cond_corr_scl() + " " +
                getNn_srch_method() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of RWParamsRecord class

