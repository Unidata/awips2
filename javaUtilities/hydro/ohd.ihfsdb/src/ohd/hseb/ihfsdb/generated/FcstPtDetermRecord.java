// filename: FcstPtDetermRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:14 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              FcstPtDeterm table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class FcstPtDetermRecord extends DbRecord
{
    private String lid;

    private String snow_method;

    private String hydrol_method;

    private String reservoir_model;

    private String upstream_seg;

    private String hydraul_method;

    private String def_issue_crit;

    private short hours_qpf;

    private String frequpd_normal;

    private String frequpd_flood;

    private String frequpd_drought;

    private String fcst_horizon;

    private short hours_qtf;

    private short hours_qzf;

    private short num_elev_zones;

    private String consumptive_use;

    private String channel_loss;

    private String fcst_gen_method;

    private long impl_date;

    private long web_date;

    private String var_usage;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public FcstPtDetermRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public FcstPtDetermRecord(FcstPtDetermRecord origRecord)
    {
        setLid(origRecord.getLid());
        setSnow_method(origRecord.getSnow_method());
        setHydrol_method(origRecord.getHydrol_method());
        setReservoir_model(origRecord.getReservoir_model());
        setUpstream_seg(origRecord.getUpstream_seg());
        setHydraul_method(origRecord.getHydraul_method());
        setDef_issue_crit(origRecord.getDef_issue_crit());
        setHours_qpf(origRecord.getHours_qpf());
        setFrequpd_normal(origRecord.getFrequpd_normal());
        setFrequpd_flood(origRecord.getFrequpd_flood());
        setFrequpd_drought(origRecord.getFrequpd_drought());
        setFcst_horizon(origRecord.getFcst_horizon());
        setHours_qtf(origRecord.getHours_qtf());
        setHours_qzf(origRecord.getHours_qzf());
        setNum_elev_zones(origRecord.getNum_elev_zones());
        setConsumptive_use(origRecord.getConsumptive_use());
        setChannel_loss(origRecord.getChannel_loss());
        setFcst_gen_method(origRecord.getFcst_gen_method());
        setImpl_date(origRecord.getImpl_date());
        setWeb_date(origRecord.getWeb_date());
        setVar_usage(origRecord.getVar_usage());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a FcstPtDeterm record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getSnow_method()
    {
        return snow_method;
    }

    public void setSnow_method(String snow_method)
    {
        this.snow_method = snow_method ;
    }

    public String getHydrol_method()
    {
        return hydrol_method;
    }

    public void setHydrol_method(String hydrol_method)
    {
        this.hydrol_method = hydrol_method ;
    }

    public String getReservoir_model()
    {
        return reservoir_model;
    }

    public void setReservoir_model(String reservoir_model)
    {
        this.reservoir_model = reservoir_model ;
    }

    public String getUpstream_seg()
    {
        return upstream_seg;
    }

    public void setUpstream_seg(String upstream_seg)
    {
        this.upstream_seg = upstream_seg ;
    }

    public String getHydraul_method()
    {
        return hydraul_method;
    }

    public void setHydraul_method(String hydraul_method)
    {
        this.hydraul_method = hydraul_method ;
    }

    public String getDef_issue_crit()
    {
        return def_issue_crit;
    }

    public void setDef_issue_crit(String def_issue_crit)
    {
        this.def_issue_crit = def_issue_crit ;
    }

    public short getHours_qpf()
    {
        return hours_qpf;
    }

    public void setHours_qpf(short hours_qpf)
    {
        this.hours_qpf = hours_qpf ;
    }

    public String getFrequpd_normal()
    {
        return frequpd_normal;
    }

    public void setFrequpd_normal(String frequpd_normal)
    {
        this.frequpd_normal = frequpd_normal ;
    }

    public String getFrequpd_flood()
    {
        return frequpd_flood;
    }

    public void setFrequpd_flood(String frequpd_flood)
    {
        this.frequpd_flood = frequpd_flood ;
    }

    public String getFrequpd_drought()
    {
        return frequpd_drought;
    }

    public void setFrequpd_drought(String frequpd_drought)
    {
        this.frequpd_drought = frequpd_drought ;
    }

    public String getFcst_horizon()
    {
        return fcst_horizon;
    }

    public void setFcst_horizon(String fcst_horizon)
    {
        this.fcst_horizon = fcst_horizon ;
    }

    public short getHours_qtf()
    {
        return hours_qtf;
    }

    public void setHours_qtf(short hours_qtf)
    {
        this.hours_qtf = hours_qtf ;
    }

    public short getHours_qzf()
    {
        return hours_qzf;
    }

    public void setHours_qzf(short hours_qzf)
    {
        this.hours_qzf = hours_qzf ;
    }

    public short getNum_elev_zones()
    {
        return num_elev_zones;
    }

    public void setNum_elev_zones(short num_elev_zones)
    {
        this.num_elev_zones = num_elev_zones ;
    }

    public String getConsumptive_use()
    {
        return consumptive_use;
    }

    public void setConsumptive_use(String consumptive_use)
    {
        this.consumptive_use = consumptive_use ;
    }

    public String getChannel_loss()
    {
        return channel_loss;
    }

    public void setChannel_loss(String channel_loss)
    {
        this.channel_loss = channel_loss ;
    }

    public String getFcst_gen_method()
    {
        return fcst_gen_method;
    }

    public void setFcst_gen_method(String fcst_gen_method)
    {
        this.fcst_gen_method = fcst_gen_method ;
    }

    public long getImpl_date()
    {
        return impl_date;
    }

    public void setImpl_date(long impl_date)
    {
        this.impl_date = impl_date ;
    }

    public long getWeb_date()
    {
        return web_date;
    }

    public void setWeb_date(long web_date)
    {
        this.web_date = web_date ;
    }

    public String getVar_usage()
    {
        return var_usage;
    }

    public void setVar_usage(String var_usage)
    {
        this.var_usage = var_usage ;
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
                 + " AND snow_method = '" + snow_method + "'" 
                 + " AND hydrol_method = '" + hydrol_method + "'" 
                 + " AND reservoir_model = '" + reservoir_model + "'" 
                 + " AND upstream_seg = '" + upstream_seg + "'" 
                 + " AND hydraul_method = '" + hydraul_method + "'" 
                 + " AND def_issue_crit = '" + def_issue_crit + "'" 
                 + " AND hours_qpf = '" + hours_qpf + "'" 
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
                getSnow_method() + " " +
                getHydrol_method() + " " +
                getReservoir_model() + " " +
                getUpstream_seg() + " " +
                getHydraul_method() + " " +
                getDef_issue_crit() + " " +
                getHours_qpf() + " " +
                getFrequpd_normal() + " " +
                getFrequpd_flood() + " " +
                getFrequpd_drought() + " " +
                getFcst_horizon() + " " +
                getHours_qtf() + " " +
                getHours_qzf() + " " +
                getNum_elev_zones() + " " +
                getConsumptive_use() + " " +
                getChannel_loss() + " " +
                getFcst_gen_method() + " " +
                getDateStringFromLongTime(getImpl_date()) + " " +
                getDateStringFromLongTime(getWeb_date()) + " " +
                getVar_usage() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of FcstPtDetermRecord class

