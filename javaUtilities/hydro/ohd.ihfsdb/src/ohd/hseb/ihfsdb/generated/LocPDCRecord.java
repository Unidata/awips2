// This is a view record !
// filename: LocPDCRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              LocPDC table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class LocPDCRecord extends DbRecord
{
    private String lid;

    private String name;

    private double lat;

    private double lon;

    private String hsa;

    private int post;

    private double elev;

    private String primary_pe;

    private double fs;

    private double fq;

    private String disp_class;

    private String is_dcp;

    private String is_observer;

    private String telem_type;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public LocPDCRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public LocPDCRecord(LocPDCRecord origRecord)
    {
        setLid(origRecord.getLid());
        setName(origRecord.getName());
        setLat(origRecord.getLat());
        setLon(origRecord.getLon());
        setHsa(origRecord.getHsa());
        setPost(origRecord.getPost());
        setElev(origRecord.getElev());
        setPrimary_pe(origRecord.getPrimary_pe());
        setFs(origRecord.getFs());
        setFq(origRecord.getFq());
        setDisp_class(origRecord.getDisp_class());
        setIs_dcp(origRecord.getIs_dcp());
        setIs_observer(origRecord.getIs_observer());
        setTelem_type(origRecord.getTelem_type());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a LocPDC record

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

    public double getElev()
    {
        return elev;
    }

    public void setElev(double elev)
    {
        this.elev = elev ;
    }

    public String getPrimary_pe()
    {
        return primary_pe;
    }

    public void setPrimary_pe(String primary_pe)
    {
        this.primary_pe = primary_pe ;
    }

    public double getFs()
    {
        return fs;
    }

    public void setFs(double fs)
    {
        this.fs = fs ;
    }

    public double getFq()
    {
        return fq;
    }

    public void setFq(double fq)
    {
        this.fq = fq ;
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
                getHsa() + " " +
                getPost() + " " +
                getElev() + " " +
                getPrimary_pe() + " " +
                getFs() + " " +
                getFq() + " " +
                getDisp_class() + " " +
                getIs_dcp() + " " +
                getIs_observer() + " " +
                getTelem_type() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of LocPDCRecord class

