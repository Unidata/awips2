// This is a view record !
// filename: HvStationRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              HvStation table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class HvStationRecord extends DbRecord
{
    private String lid;

    private String name;

    private double lat;

    private double lon;

    private String stream_name;

    private String primary_pe;

    private double flood_stage;

    private double flood_flow;

    private double action_stage;

    private double action_flow;

    private String disp_class;

    private String is_dcp;

    private String is_observer;

    private String telem_type;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public HvStationRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public HvStationRecord(HvStationRecord origRecord)
    {
        setLid(origRecord.getLid());
        setName(origRecord.getName());
        setLat(origRecord.getLat());
        setLon(origRecord.getLon());
        setStream_name(origRecord.getStream_name());
        setPrimary_pe(origRecord.getPrimary_pe());
        setFlood_stage(origRecord.getFlood_stage());
        setFlood_flow(origRecord.getFlood_flow());
        setAction_stage(origRecord.getAction_stage());
        setAction_flow(origRecord.getAction_flow());
        setDisp_class(origRecord.getDisp_class());
        setIs_dcp(origRecord.getIs_dcp());
        setIs_observer(origRecord.getIs_observer());
        setTelem_type(origRecord.getTelem_type());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a HvStation record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name ;
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

    public String getStream_name()
    {
        return stream_name;
    }

    public void setStream_name(String stream_name)
    {
        this.stream_name = stream_name ;
    }

    public String getPrimary_pe()
    {
        return primary_pe;
    }

    public void setPrimary_pe(String primary_pe)
    {
        this.primary_pe = primary_pe ;
    }

    public double getFlood_stage()
    {
        return flood_stage;
    }

    public void setFlood_stage(double flood_stage)
    {
        this.flood_stage = flood_stage ;
    }

    public double getFlood_flow()
    {
        return flood_flow;
    }

    public void setFlood_flow(double flood_flow)
    {
        this.flood_flow = flood_flow ;
    }

    public double getAction_stage()
    {
        return action_stage;
    }

    public void setAction_stage(double action_stage)
    {
        this.action_stage = action_stage ;
    }

    public double getAction_flow()
    {
        return action_flow;
    }

    public void setAction_flow(double action_flow)
    {
        this.action_flow = action_flow ;
    }

    public String getDisp_class()
    {
        return disp_class;
    }

    public void setDisp_class(String disp_class)
    {
        this.disp_class = disp_class ;
    }

    public String getIs_dcp()
    {
        return is_dcp;
    }

    public void setIs_dcp(String is_dcp)
    {
        this.is_dcp = is_dcp ;
    }

    public String getIs_observer()
    {
        return is_observer;
    }

    public void setIs_observer(String is_observer)
    {
        this.is_observer = is_observer ;
    }

    public String getTelem_type()
    {
        return telem_type;
    }

    public void setTelem_type(String telem_type)
    {
        this.telem_type = telem_type ;
    }

//-----------------------------------------------------------------
//  toString() - this method is called with no arguments
//  and returns a String of the internal values
//-----------------------------------------------------------------
    public String toString()
    {
        String outString = 
                getLid() + " " +
                getName() + " " +
                getLat() + " " +
                getLon() + " " +
                getStream_name() + " " +
                getPrimary_pe() + " " +
                getFlood_stage() + " " +
                getFlood_flow() + " " +
                getAction_stage() + " " +
                getAction_flow() + " " +
                getDisp_class() + " " +
                getIs_dcp() + " " +
                getIs_observer() + " " +
                getTelem_type() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of HvStationRecord class

