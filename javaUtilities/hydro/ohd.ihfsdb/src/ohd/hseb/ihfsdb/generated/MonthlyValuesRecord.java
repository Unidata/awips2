// filename: MonthlyValuesRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              MonthlyValues table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class MonthlyValuesRecord extends DbRecord
{
    private String lid;

    private String pe;

    private short dur;

    private String ts;

    private String extremum;

    private String adjustment;

    private long postingtime;

    private double jan_value;

    private double feb_value;

    private double mar_value;

    private double apr_value;

    private double may_value;

    private double jun_value;

    private double jul_value;

    private double aug_value;

    private double sep_value;

    private double oct_value;

    private double nov_value;

    private double dec_value;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public MonthlyValuesRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public MonthlyValuesRecord(MonthlyValuesRecord origRecord)
    {
        setLid(origRecord.getLid());
        setPe(origRecord.getPe());
        setDur(origRecord.getDur());
        setTs(origRecord.getTs());
        setExtremum(origRecord.getExtremum());
        setAdjustment(origRecord.getAdjustment());
        setPostingtime(origRecord.getPostingtime());
        setJan_value(origRecord.getJan_value());
        setFeb_value(origRecord.getFeb_value());
        setMar_value(origRecord.getMar_value());
        setApr_value(origRecord.getApr_value());
        setMay_value(origRecord.getMay_value());
        setJun_value(origRecord.getJun_value());
        setJul_value(origRecord.getJul_value());
        setAug_value(origRecord.getAug_value());
        setSep_value(origRecord.getSep_value());
        setOct_value(origRecord.getOct_value());
        setNov_value(origRecord.getNov_value());
        setDec_value(origRecord.getDec_value());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a MonthlyValues record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getPe()
    {
        return pe;
    }

    public void setPe(String pe)
    {
        this.pe = pe ;
    }

    public short getDur()
    {
        return dur;
    }

    public void setDur(short dur)
    {
        this.dur = dur ;
    }

    public String getTs()
    {
        return ts;
    }

    public void setTs(String ts)
    {
        this.ts = ts ;
    }

    public String getExtremum()
    {
        return extremum;
    }

    public void setExtremum(String extremum)
    {
        this.extremum = extremum ;
    }

    public String getAdjustment()
    {
        return adjustment;
    }

    public void setAdjustment(String adjustment)
    {
        this.adjustment = adjustment ;
    }

    public long getPostingtime()
    {
        return postingtime;
    }

    public void setPostingtime(long postingtime)
    {
        this.postingtime = postingtime ;
    }

    public double getJan_value()
    {
        return jan_value;
    }

    public void setJan_value(double jan_value)
    {
        this.jan_value = jan_value ;
    }

    public double getFeb_value()
    {
        return feb_value;
    }

    public void setFeb_value(double feb_value)
    {
        this.feb_value = feb_value ;
    }

    public double getMar_value()
    {
        return mar_value;
    }

    public void setMar_value(double mar_value)
    {
        this.mar_value = mar_value ;
    }

    public double getApr_value()
    {
        return apr_value;
    }

    public void setApr_value(double apr_value)
    {
        this.apr_value = apr_value ;
    }

    public double getMay_value()
    {
        return may_value;
    }

    public void setMay_value(double may_value)
    {
        this.may_value = may_value ;
    }

    public double getJun_value()
    {
        return jun_value;
    }

    public void setJun_value(double jun_value)
    {
        this.jun_value = jun_value ;
    }

    public double getJul_value()
    {
        return jul_value;
    }

    public void setJul_value(double jul_value)
    {
        this.jul_value = jul_value ;
    }

    public double getAug_value()
    {
        return aug_value;
    }

    public void setAug_value(double aug_value)
    {
        this.aug_value = aug_value ;
    }

    public double getSep_value()
    {
        return sep_value;
    }

    public void setSep_value(double sep_value)
    {
        this.sep_value = sep_value ;
    }

    public double getOct_value()
    {
        return oct_value;
    }

    public void setOct_value(double oct_value)
    {
        this.oct_value = oct_value ;
    }

    public double getNov_value()
    {
        return nov_value;
    }

    public void setNov_value(double nov_value)
    {
        this.nov_value = nov_value ;
    }

    public double getDec_value()
    {
        return dec_value;
    }

    public void setDec_value(double dec_value)
    {
        this.dec_value = dec_value ;
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
                 + " AND pe = '" + pe + "'" 
                 + " AND dur = '" + dur + "'" 
                 + " AND ts = '" + ts + "'" 
                 + " AND extremum = '" + extremum + "'" 
                 + " AND adjustment = '" + adjustment + "'" 
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
                getPe() + " " +
                getDur() + " " +
                getTs() + " " +
                getExtremum() + " " +
                getAdjustment() + " " +
                getDateTimeStringFromLongTime(getPostingtime()) + " " +
                getJan_value() + " " +
                getFeb_value() + " " +
                getMar_value() + " " +
                getApr_value() + " " +
                getMay_value() + " " +
                getJun_value() + " " +
                getJul_value() + " " +
                getAug_value() + " " +
                getSep_value() + " " +
                getOct_value() + " " +
                getNov_value() + " " +
                getDec_value() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of MonthlyValuesRecord class

