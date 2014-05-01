// filename: RiverstatRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Riverstat table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class RiverstatRecord extends DbRecord
{
    private String lid;

    private String primary_pe;

    private double bf;

    private double cb;

    private double da;

    private double response_time;

    private double threshold_runoff;

    private double fq;

    private double fs;

    private String gsno;

    private String level;

    private double mile;

    private double pool;

    private String por;

    private String rated;

    private double lat;

    private double lon;

    private String remark;

    private long rrevise;

    private String rsource;

    private String stream;

    private String tide;

    private String backwater;

    private String vdatum;

    private double action_flow;

    private double wstg;

    private double zd;

    private long ratedat;

    private String usgs_ratenum;

    private int uhgdur;

    private String use_latest_fcst;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public RiverstatRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public RiverstatRecord(RiverstatRecord origRecord)
    {
        setLid(origRecord.getLid());
        setPrimary_pe(origRecord.getPrimary_pe());
        setBf(origRecord.getBf());
        setCb(origRecord.getCb());
        setDa(origRecord.getDa());
        setResponse_time(origRecord.getResponse_time());
        setThreshold_runoff(origRecord.getThreshold_runoff());
        setFq(origRecord.getFq());
        setFs(origRecord.getFs());
        setGsno(origRecord.getGsno());
        setLevel(origRecord.getLevel());
        setMile(origRecord.getMile());
        setPool(origRecord.getPool());
        setPor(origRecord.getPor());
        setRated(origRecord.getRated());
        setLat(origRecord.getLat());
        setLon(origRecord.getLon());
        setRemark(origRecord.getRemark());
        setRrevise(origRecord.getRrevise());
        setRsource(origRecord.getRsource());
        setStream(origRecord.getStream());
        setTide(origRecord.getTide());
        setBackwater(origRecord.getBackwater());
        setVdatum(origRecord.getVdatum());
        setAction_flow(origRecord.getAction_flow());
        setWstg(origRecord.getWstg());
        setZd(origRecord.getZd());
        setRatedat(origRecord.getRatedat());
        setUsgs_ratenum(origRecord.getUsgs_ratenum());
        setUhgdur(origRecord.getUhgdur());
        setUse_latest_fcst(origRecord.getUse_latest_fcst());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Riverstat record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getPrimary_pe()
    {
        return primary_pe;
    }

    public void setPrimary_pe(String primary_pe)
    {
        this.primary_pe = primary_pe ;
    }

    public double getBf()
    {
        return bf;
    }

    public void setBf(double bf)
    {
        this.bf = bf ;
    }

    public double getCb()
    {
        return cb;
    }

    public void setCb(double cb)
    {
        this.cb = cb ;
    }

    public double getDa()
    {
        return da;
    }

    public void setDa(double da)
    {
        this.da = da ;
    }

    public double getResponse_time()
    {
        return response_time;
    }

    public void setResponse_time(double response_time)
    {
        this.response_time = response_time ;
    }

    public double getThreshold_runoff()
    {
        return threshold_runoff;
    }

    public void setThreshold_runoff(double threshold_runoff)
    {
        this.threshold_runoff = threshold_runoff ;
    }

    public double getFq()
    {
        return fq;
    }

    public void setFq(double fq)
    {
        this.fq = fq ;
    }

    public double getFs()
    {
        return fs;
    }

    public void setFs(double fs)
    {
        this.fs = fs ;
    }

    public String getGsno()
    {
        return gsno;
    }

    public void setGsno(String gsno)
    {
        this.gsno = gsno ;
    }

    public String getLevel()
    {
        return level;
    }

    public void setLevel(String level)
    {
        this.level = level ;
    }

    public double getMile()
    {
        return mile;
    }

    public void setMile(double mile)
    {
        this.mile = mile ;
    }

    public double getPool()
    {
        return pool;
    }

    public void setPool(double pool)
    {
        this.pool = pool ;
    }

    public String getPor()
    {
        return por;
    }

    public void setPor(String por)
    {
        this.por = por ;
    }

    public String getRated()
    {
        return rated;
    }

    public void setRated(String rated)
    {
        this.rated = rated ;
    }

    public double getLat()
    {
        return lat;
    }

    public void setLat(double lat)
    {
        this.lat = lat ;
    }

    public double getLon()
    {
        return lon;
    }

    public void setLon(double lon)
    {
        this.lon = lon ;
    }

    public String getRemark()
    {
        return remark;
    }

    public void setRemark(String remark)
    {
        this.remark = remark ;
    }

    public long getRrevise()
    {
        return rrevise;
    }

    public void setRrevise(long rrevise)
    {
        this.rrevise = rrevise ;
    }

    public String getRsource()
    {
        return rsource;
    }

    public void setRsource(String rsource)
    {
        this.rsource = rsource ;
    }

    public String getStream()
    {
        return stream;
    }

    public void setStream(String stream)
    {
        this.stream = stream ;
    }

    public String getTide()
    {
        return tide;
    }

    public void setTide(String tide)
    {
        this.tide = tide ;
    }

    public String getBackwater()
    {
        return backwater;
    }

    public void setBackwater(String backwater)
    {
        this.backwater = backwater ;
    }

    public String getVdatum()
    {
        return vdatum;
    }

    public void setVdatum(String vdatum)
    {
        this.vdatum = vdatum ;
    }

    public double getAction_flow()
    {
        return action_flow;
    }

    public void setAction_flow(double action_flow)
    {
        this.action_flow = action_flow ;
    }

    public double getWstg()
    {
        return wstg;
    }

    public void setWstg(double wstg)
    {
        this.wstg = wstg ;
    }

    public double getZd()
    {
        return zd;
    }

    public void setZd(double zd)
    {
        this.zd = zd ;
    }

    public long getRatedat()
    {
        return ratedat;
    }

    public void setRatedat(long ratedat)
    {
        this.ratedat = ratedat ;
    }

    public String getUsgs_ratenum()
    {
        return usgs_ratenum;
    }

    public void setUsgs_ratenum(String usgs_ratenum)
    {
        this.usgs_ratenum = usgs_ratenum ;
    }

    public int getUhgdur()
    {
        return uhgdur;
    }

    public void setUhgdur(int uhgdur)
    {
        this.uhgdur = uhgdur ;
    }

    public String getUse_latest_fcst()
    {
        return use_latest_fcst;
    }

    public void setUse_latest_fcst(String use_latest_fcst)
    {
        this.use_latest_fcst = use_latest_fcst ;
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
                getPrimary_pe() + " " +
                getBf() + " " +
                getCb() + " " +
                getDa() + " " +
                getResponse_time() + " " +
                getThreshold_runoff() + " " +
                getFq() + " " +
                getFs() + " " +
                getGsno() + " " +
                getLevel() + " " +
                getMile() + " " +
                getPool() + " " +
                getPor() + " " +
                getRated() + " " +
                getLat() + " " +
                getLon() + " " +
                getRemark() + " " +
                getDateStringFromLongTime(getRrevise()) + " " +
                getRsource() + " " +
                getStream() + " " +
                getTide() + " " +
                getBackwater() + " " +
                getVdatum() + " " +
                getAction_flow() + " " +
                getWstg() + " " +
                getZd() + " " +
                getDateStringFromLongTime(getRatedat()) + " " +
                getUsgs_ratenum() + " " +
                getUhgdur() + " " +
                getUse_latest_fcst() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of RiverstatRecord class

