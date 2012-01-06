// filename: dspadaptRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:14 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              dspadapt table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class dspadaptRecord extends DbRecord
{
    private String radid;

    private long obstime;

    private float min_reflth;

    private float max_reflth;

    private float ref_tltest;

    private float rng_tltin;

    private float rng_tltout;

    private float max_birng;

    private float min_birng;

    private float min_echoar;

    private float min_awrefl;

    private float max_pctred;

    private float mlt_zrcoef;

    private float pwr_zrcoef;

    private float min_zrefl;

    private float max_zrefl;

    private float max_stmspd;

    private float max_timdif;

    private float min_artcon;

    private float tim_p1cont;

    private float tim_p2cont;

    private float max_ecarch;

    private float rng_cutoff;

    private float rng_e1coef;

    private float rng_e2coef;

    private float rng_e3coef;

    private float min_prate;

    private float max_prate;

    private float tim_restrt;

    private float max_timint;

    private float min_timprd;

    private float thr_hlyout;

    private float end_timgag;

    private float max_prdval;

    private float max_hlyval;

    private float tim_biest;

    private float thr_nosets;

    private float res_bias;

    private float longest_lag;

    private String bias_applied;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public dspadaptRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public dspadaptRecord(dspadaptRecord origRecord)
    {
        setRadid(origRecord.getRadid());
        setObstime(origRecord.getObstime());
        setMin_reflth(origRecord.getMin_reflth());
        setMax_reflth(origRecord.getMax_reflth());
        setRef_tltest(origRecord.getRef_tltest());
        setRng_tltin(origRecord.getRng_tltin());
        setRng_tltout(origRecord.getRng_tltout());
        setMax_birng(origRecord.getMax_birng());
        setMin_birng(origRecord.getMin_birng());
        setMin_echoar(origRecord.getMin_echoar());
        setMin_awrefl(origRecord.getMin_awrefl());
        setMax_pctred(origRecord.getMax_pctred());
        setMlt_zrcoef(origRecord.getMlt_zrcoef());
        setPwr_zrcoef(origRecord.getPwr_zrcoef());
        setMin_zrefl(origRecord.getMin_zrefl());
        setMax_zrefl(origRecord.getMax_zrefl());
        setMax_stmspd(origRecord.getMax_stmspd());
        setMax_timdif(origRecord.getMax_timdif());
        setMin_artcon(origRecord.getMin_artcon());
        setTim_p1cont(origRecord.getTim_p1cont());
        setTim_p2cont(origRecord.getTim_p2cont());
        setMax_ecarch(origRecord.getMax_ecarch());
        setRng_cutoff(origRecord.getRng_cutoff());
        setRng_e1coef(origRecord.getRng_e1coef());
        setRng_e2coef(origRecord.getRng_e2coef());
        setRng_e3coef(origRecord.getRng_e3coef());
        setMin_prate(origRecord.getMin_prate());
        setMax_prate(origRecord.getMax_prate());
        setTim_restrt(origRecord.getTim_restrt());
        setMax_timint(origRecord.getMax_timint());
        setMin_timprd(origRecord.getMin_timprd());
        setThr_hlyout(origRecord.getThr_hlyout());
        setEnd_timgag(origRecord.getEnd_timgag());
        setMax_prdval(origRecord.getMax_prdval());
        setMax_hlyval(origRecord.getMax_hlyval());
        setTim_biest(origRecord.getTim_biest());
        setThr_nosets(origRecord.getThr_nosets());
        setRes_bias(origRecord.getRes_bias());
        setLongest_lag(origRecord.getLongest_lag());
        setBias_applied(origRecord.getBias_applied());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a dspadapt record

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

    public float getMin_reflth()
    {
        return min_reflth;
    }

    public void setMin_reflth(float min_reflth)
    {
        this.min_reflth = min_reflth ;
    }

    public float getMax_reflth()
    {
        return max_reflth;
    }

    public void setMax_reflth(float max_reflth)
    {
        this.max_reflth = max_reflth ;
    }

    public float getRef_tltest()
    {
        return ref_tltest;
    }

    public void setRef_tltest(float ref_tltest)
    {
        this.ref_tltest = ref_tltest ;
    }

    public float getRng_tltin()
    {
        return rng_tltin;
    }

    public void setRng_tltin(float rng_tltin)
    {
        this.rng_tltin = rng_tltin ;
    }

    public float getRng_tltout()
    {
        return rng_tltout;
    }

    public void setRng_tltout(float rng_tltout)
    {
        this.rng_tltout = rng_tltout ;
    }

    public float getMax_birng()
    {
        return max_birng;
    }

    public void setMax_birng(float max_birng)
    {
        this.max_birng = max_birng ;
    }

    public float getMin_birng()
    {
        return min_birng;
    }

    public void setMin_birng(float min_birng)
    {
        this.min_birng = min_birng ;
    }

    public float getMin_echoar()
    {
        return min_echoar;
    }

    public void setMin_echoar(float min_echoar)
    {
        this.min_echoar = min_echoar ;
    }

    public float getMin_awrefl()
    {
        return min_awrefl;
    }

    public void setMin_awrefl(float min_awrefl)
    {
        this.min_awrefl = min_awrefl ;
    }

    public float getMax_pctred()
    {
        return max_pctred;
    }

    public void setMax_pctred(float max_pctred)
    {
        this.max_pctred = max_pctred ;
    }

    public float getMlt_zrcoef()
    {
        return mlt_zrcoef;
    }

    public void setMlt_zrcoef(float mlt_zrcoef)
    {
        this.mlt_zrcoef = mlt_zrcoef ;
    }

    public float getPwr_zrcoef()
    {
        return pwr_zrcoef;
    }

    public void setPwr_zrcoef(float pwr_zrcoef)
    {
        this.pwr_zrcoef = pwr_zrcoef ;
    }

    public float getMin_zrefl()
    {
        return min_zrefl;
    }

    public void setMin_zrefl(float min_zrefl)
    {
        this.min_zrefl = min_zrefl ;
    }

    public float getMax_zrefl()
    {
        return max_zrefl;
    }

    public void setMax_zrefl(float max_zrefl)
    {
        this.max_zrefl = max_zrefl ;
    }

    public float getMax_stmspd()
    {
        return max_stmspd;
    }

    public void setMax_stmspd(float max_stmspd)
    {
        this.max_stmspd = max_stmspd ;
    }

    public float getMax_timdif()
    {
        return max_timdif;
    }

    public void setMax_timdif(float max_timdif)
    {
        this.max_timdif = max_timdif ;
    }

    public float getMin_artcon()
    {
        return min_artcon;
    }

    public void setMin_artcon(float min_artcon)
    {
        this.min_artcon = min_artcon ;
    }

    public float getTim_p1cont()
    {
        return tim_p1cont;
    }

    public void setTim_p1cont(float tim_p1cont)
    {
        this.tim_p1cont = tim_p1cont ;
    }

    public float getTim_p2cont()
    {
        return tim_p2cont;
    }

    public void setTim_p2cont(float tim_p2cont)
    {
        this.tim_p2cont = tim_p2cont ;
    }

    public float getMax_ecarch()
    {
        return max_ecarch;
    }

    public void setMax_ecarch(float max_ecarch)
    {
        this.max_ecarch = max_ecarch ;
    }

    public float getRng_cutoff()
    {
        return rng_cutoff;
    }

    public void setRng_cutoff(float rng_cutoff)
    {
        this.rng_cutoff = rng_cutoff ;
    }

    public float getRng_e1coef()
    {
        return rng_e1coef;
    }

    public void setRng_e1coef(float rng_e1coef)
    {
        this.rng_e1coef = rng_e1coef ;
    }

    public float getRng_e2coef()
    {
        return rng_e2coef;
    }

    public void setRng_e2coef(float rng_e2coef)
    {
        this.rng_e2coef = rng_e2coef ;
    }

    public float getRng_e3coef()
    {
        return rng_e3coef;
    }

    public void setRng_e3coef(float rng_e3coef)
    {
        this.rng_e3coef = rng_e3coef ;
    }

    public float getMin_prate()
    {
        return min_prate;
    }

    public void setMin_prate(float min_prate)
    {
        this.min_prate = min_prate ;
    }

    public float getMax_prate()
    {
        return max_prate;
    }

    public void setMax_prate(float max_prate)
    {
        this.max_prate = max_prate ;
    }

    public float getTim_restrt()
    {
        return tim_restrt;
    }

    public void setTim_restrt(float tim_restrt)
    {
        this.tim_restrt = tim_restrt ;
    }

    public float getMax_timint()
    {
        return max_timint;
    }

    public void setMax_timint(float max_timint)
    {
        this.max_timint = max_timint ;
    }

    public float getMin_timprd()
    {
        return min_timprd;
    }

    public void setMin_timprd(float min_timprd)
    {
        this.min_timprd = min_timprd ;
    }

    public float getThr_hlyout()
    {
        return thr_hlyout;
    }

    public void setThr_hlyout(float thr_hlyout)
    {
        this.thr_hlyout = thr_hlyout ;
    }

    public float getEnd_timgag()
    {
        return end_timgag;
    }

    public void setEnd_timgag(float end_timgag)
    {
        this.end_timgag = end_timgag ;
    }

    public float getMax_prdval()
    {
        return max_prdval;
    }

    public void setMax_prdval(float max_prdval)
    {
        this.max_prdval = max_prdval ;
    }

    public float getMax_hlyval()
    {
        return max_hlyval;
    }

    public void setMax_hlyval(float max_hlyval)
    {
        this.max_hlyval = max_hlyval ;
    }

    public float getTim_biest()
    {
        return tim_biest;
    }

    public void setTim_biest(float tim_biest)
    {
        this.tim_biest = tim_biest ;
    }

    public float getThr_nosets()
    {
        return thr_nosets;
    }

    public void setThr_nosets(float thr_nosets)
    {
        this.thr_nosets = thr_nosets ;
    }

    public float getRes_bias()
    {
        return res_bias;
    }

    public void setRes_bias(float res_bias)
    {
        this.res_bias = res_bias ;
    }

    public float getLongest_lag()
    {
        return longest_lag;
    }

    public void setLongest_lag(float longest_lag)
    {
        this.longest_lag = longest_lag ;
    }

    public String getBias_applied()
    {
        return bias_applied;
    }

    public void setBias_applied(String bias_applied)
    {
        this.bias_applied = bias_applied ;
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
                getMin_reflth() + " " +
                getMax_reflth() + " " +
                getRef_tltest() + " " +
                getRng_tltin() + " " +
                getRng_tltout() + " " +
                getMax_birng() + " " +
                getMin_birng() + " " +
                getMin_echoar() + " " +
                getMin_awrefl() + " " +
                getMax_pctred() + " " +
                getMlt_zrcoef() + " " +
                getPwr_zrcoef() + " " +
                getMin_zrefl() + " " +
                getMax_zrefl() + " " +
                getMax_stmspd() + " " +
                getMax_timdif() + " " +
                getMin_artcon() + " " +
                getTim_p1cont() + " " +
                getTim_p2cont() + " " +
                getMax_ecarch() + " " +
                getRng_cutoff() + " " +
                getRng_e1coef() + " " +
                getRng_e2coef() + " " +
                getRng_e3coef() + " " +
                getMin_prate() + " " +
                getMax_prate() + " " +
                getTim_restrt() + " " +
                getMax_timint() + " " +
                getMin_timprd() + " " +
                getThr_hlyout() + " " +
                getEnd_timgag() + " " +
                getMax_prdval() + " " +
                getMax_hlyval() + " " +
                getTim_biest() + " " +
                getThr_nosets() + " " +
                getRes_bias() + " " +
                getLongest_lag() + " " +
                getBias_applied() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of dspadaptRecord class

