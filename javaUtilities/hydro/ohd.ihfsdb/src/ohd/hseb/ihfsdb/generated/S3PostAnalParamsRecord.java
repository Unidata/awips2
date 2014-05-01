// filename: S3PostAnalParamsRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              S3PostAnalParams table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class S3PostAnalParamsRecord extends DbRecord
{
    private short gg_weighting;

    private float gg_min_gage_val;

    private short gg_min_dist;

    private float kernel_est_scale;

    private float rhat;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public S3PostAnalParamsRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public S3PostAnalParamsRecord(S3PostAnalParamsRecord origRecord)
    {
        setGg_weighting(origRecord.getGg_weighting());
        setGg_min_gage_val(origRecord.getGg_min_gage_val());
        setGg_min_dist(origRecord.getGg_min_dist());
        setKernel_est_scale(origRecord.getKernel_est_scale());
        setRhat(origRecord.getRhat());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a S3PostAnalParams record

    //-----------------------------------------------------------------
    public short getGg_weighting()
    {
        return gg_weighting;
    }

    public void setGg_weighting(short gg_weighting)
    {
        this.gg_weighting = gg_weighting ;
    }

    public float getGg_min_gage_val()
    {
        return gg_min_gage_val;
    }

    public void setGg_min_gage_val(float gg_min_gage_val)
    {
        this.gg_min_gage_val = gg_min_gage_val ;
    }

    public short getGg_min_dist()
    {
        return gg_min_dist;
    }

    public void setGg_min_dist(short gg_min_dist)
    {
        this.gg_min_dist = gg_min_dist ;
    }

    public float getKernel_est_scale()
    {
        return kernel_est_scale;
    }

    public void setKernel_est_scale(float kernel_est_scale)
    {
        this.kernel_est_scale = kernel_est_scale ;
    }

    public float getRhat()
    {
        return rhat;
    }

    public void setRhat(float rhat)
    {
        this.rhat = rhat ;
    }

//-----------------------------------------------------------------
//  toString() - this method is called with no arguments
//  and returns a String of the internal values
//-----------------------------------------------------------------
    public String toString()
    {
        String outString = 
                getGg_weighting() + " " +
                getGg_min_gage_val() + " " +
                getGg_min_dist() + " " +
                getKernel_est_scale() + " " +
                getRhat() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of S3PostAnalParamsRecord class

