// This is a view record !
// filename: LocClassRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              LocClass table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class LocClassRecord extends DbRecord
{
    private String lid;

    private String name;

    private double lat;

    private double lon;

    private String wfo;

    private String hsa;

    private int post;

    private String disp_class;

    private String is_dcp;

    private String is_observer;

    private String telem_type;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public LocClassRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public LocClassRecord(LocClassRecord origRecord)
    {
        setLid(origRecord.getLid());
        setName(origRecord.getName());
        setLat(origRecord.getLat());
        setLon(origRecord.getLon());
        setWfo(origRecord.getWfo());
        setHsa(origRecord.getHsa());
        setPost(origRecord.getPost());
        setDisp_class(origRecord.getDisp_class());
        setIs_dcp(origRecord.getIs_dcp());
        setIs_observer(origRecord.getIs_observer());
        setTelem_type(origRecord.getTelem_type());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a LocClass record

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

    public String getWfo()
    {
        return wfo;
    }

    public void setWfo(String wfo)
    {
        this.wfo = wfo ;
    }

    public String getHsa()
    {
        return hsa;
    }

    public void setHsa(String hsa)
    {
        this.hsa = hsa ;
    }

    public int getPost()
    {
        return post;
    }

    public void setPost(int post)
    {
        this.post = post ;
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
                getWfo() + " " +
                getHsa() + " " +
                getPost() + " " +
                getDisp_class() + " " +
                getIs_dcp() + " " +
                getIs_observer() + " " +
                getTelem_type() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of LocClassRecord class

