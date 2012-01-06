// filename: FcstPtESPRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:15 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              FcstPtESP table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class FcstPtESPRecord extends DbRecord
{
    private String lid;

    private String snow_method;

    private String hydrol_method;

    private String reservoir_model;

    private String upstream_seg;

    private String hydraul_method;

    private String flowtype;

    private String fcsttype;

    private String frequpd_normal;

    private String frequpd_flood;

    private String frequpd_drought;

    private String fcst_horizon;

    private short nummonclim;

    private short numdayhyd;

    private short num_elev_zones;

    private String consumptive_use;

    private String channel_loss;

    private String post_processor;

    private long impl_date;

    private long external_date;

    private long web_date;

    private String var_usage;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public FcstPtESPRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public FcstPtESPRecord(FcstPtESPRecord origRecord)
    {
        setLid(origRecord.getLid());
        setSnow_method(origRecord.getSnow_method());
        setHydrol_method(origRecord.getHydrol_method());
        setReservoir_model(origRecord.getReservoir_model());
        setUpstream_seg(origRecord.getUpstream_seg());
        setHydraul_method(origRecord.getHydraul_method());
        setFlowtype(origRecord.getFlowtype());
        setFcsttype(origRecord.getFcsttype());
        setFrequpd_normal(origRecord.getFrequpd_normal());
        setFrequpd_flood(origRecord.getFrequpd_flood());
        setFrequpd_drought(origRecord.getFrequpd_drought());
        setFcst_horizon(origRecord.getFcst_horizon());
        setNummonclim(origRecord.getNummonclim());
        setNumdayhyd(origRecord.getNumdayhyd());
        setNum_elev_zones(origRecord.getNum_elev_zones());
        setConsumptive_use(origRecord.getConsumptive_use());
        setChannel_loss(origRecord.getChannel_loss());
        setPost_processor(origRecord.getPost_processor());
        setImpl_date(origRecord.getImpl_date());
        setExternal_date(origRecord.getExternal_date());
        setWeb_date(origRecord.getWeb_date());
        setVar_usage(origRecord.getVar_usage());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a FcstPtESP record

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

    public String getFlowtype()
    {
        return flowtype;
    }

    public void setFlowtype(String flowtype)
    {
        this.flowtype = flowtype ;
    }

    public String getFcsttype()
    {
        return fcsttype;
    }

    public void setFcsttype(String fcsttype)
    {
        this.fcsttype = fcsttype ;
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

    public short getNummonclim()
    {
        return nummonclim;
    }

    public void setNummonclim(short nummonclim)
    {
        this.nummonclim = nummonclim ;
    }

    public short getNumdayhyd()
    {
        return numdayhyd;
    }

    public void setNumdayhyd(short numdayhyd)
    {
        this.numdayhyd = numdayhyd ;
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

    public String getPost_processor()
    {
        return post_processor;
    }

    public void setPost_processor(String post_processor)
    {
        this.post_processor = post_processor ;
    }

    public long getImpl_date()
    {
        return impl_date;
    }

    public void setImpl_date(long impl_date)
    {
        this.impl_date = impl_date ;
    }

    public long getExternal_date()
    {
        return external_date;
    }

    public void setExternal_date(long external_date)
    {
        this.external_date = external_date ;
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
                 + " AND flowtype = '" + flowtype + "'" 
                 + " AND fcsttype = '" + fcsttype + "'" 
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
                getFlowtype() + " " +
                getFcsttype() + " " +
                getFrequpd_normal() + " " +
                getFrequpd_flood() + " " +
                getFrequpd_drought() + " " +
                getFcst_horizon() + " " +
                getNummonclim() + " " +
                getNumdayhyd() + " " +
                getNum_elev_zones() + " " +
                getConsumptive_use() + " " +
                getChannel_loss() + " " +
                getPost_processor() + " " +
                getDateStringFromLongTime(getImpl_date()) + " " +
                getDateStringFromLongTime(getExternal_date()) + " " +
                getDateStringFromLongTime(getWeb_date()) + " " +
                getVar_usage() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of FcstPtESPRecord class

