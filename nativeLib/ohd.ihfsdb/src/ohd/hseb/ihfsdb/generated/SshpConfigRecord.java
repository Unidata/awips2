// filename: SshpConfigRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              SshpConfig table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class SshpConfigRecord extends DbRecord
{
    private String lid;

    private String basin_id;

    private long postingtime;

    private String model_pref;

    private String auto_process;

    private String source_pref;

    private String use_static_evap;

    private String use_blend;

    private String blend_method;

    private int blend_hours;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public SshpConfigRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public SshpConfigRecord(SshpConfigRecord origRecord)
    {
        setLid(origRecord.getLid());
        setBasin_id(origRecord.getBasin_id());
        setPostingtime(origRecord.getPostingtime());
        setModel_pref(origRecord.getModel_pref());
        setAuto_process(origRecord.getAuto_process());
        setSource_pref(origRecord.getSource_pref());
        setUse_static_evap(origRecord.getUse_static_evap());
        setUse_blend(origRecord.getUse_blend());
        setBlend_method(origRecord.getBlend_method());
        setBlend_hours(origRecord.getBlend_hours());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a SshpConfig record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getBasin_id()
    {
        return basin_id;
    }

    public void setBasin_id(String basin_id)
    {
        this.basin_id = basin_id ;
    }

    public long getPostingtime()
    {
        return postingtime;
    }

    public void setPostingtime(long postingtime)
    {
        this.postingtime = postingtime ;
    }

    public String getModel_pref()
    {
        return model_pref;
    }

    public void setModel_pref(String model_pref)
    {
        this.model_pref = model_pref ;
    }

    public String getAuto_process()
    {
        return auto_process;
    }

    public void setAuto_process(String auto_process)
    {
        this.auto_process = auto_process ;
    }

    public String getSource_pref()
    {
        return source_pref;
    }

    public void setSource_pref(String source_pref)
    {
        this.source_pref = source_pref ;
    }

    public String getUse_static_evap()
    {
        return use_static_evap;
    }

    public void setUse_static_evap(String use_static_evap)
    {
        this.use_static_evap = use_static_evap ;
    }

    public String getUse_blend()
    {
        return use_blend;
    }

    public void setUse_blend(String use_blend)
    {
        this.use_blend = use_blend ;
    }

    public String getBlend_method()
    {
        return blend_method;
    }

    public void setBlend_method(String blend_method)
    {
        this.blend_method = blend_method ;
    }

    public int getBlend_hours()
    {
        return blend_hours;
    }

    public void setBlend_hours(int blend_hours)
    {
        this.blend_hours = blend_hours ;
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
                getBasin_id() + " " +
                getDateTimeStringFromLongTime(getPostingtime()) + " " +
                getModel_pref() + " " +
                getAuto_process() + " " +
                getSource_pref() + " " +
                getUse_static_evap() + " " +
                getUse_blend() + " " +
                getBlend_method() + " " +
                getBlend_hours() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of SshpConfigRecord class

