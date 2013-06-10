// This is a view record !
// filename: LocViewRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              LocView table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class LocViewRecord extends DbRecord
{
    private String lid;

    private String name;

    private double lat;

    private double lon;

    private String rb;

    private String state;

    private String county;

    private String type;

    private String wfo;

    private String hsa;

    private int post;

    private String stream;

    private String gsno;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public LocViewRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public LocViewRecord(LocViewRecord origRecord)
    {
        setLid(origRecord.getLid());
        setName(origRecord.getName());
        setLat(origRecord.getLat());
        setLon(origRecord.getLon());
        setRb(origRecord.getRb());
        setState(origRecord.getState());
        setCounty(origRecord.getCounty());
        setType(origRecord.getType());
        setWfo(origRecord.getWfo());
        setHsa(origRecord.getHsa());
        setPost(origRecord.getPost());
        setStream(origRecord.getStream());
        setGsno(origRecord.getGsno());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a LocView record

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

    public String getRb()
    {
        return rb;
    }

    public void setRb(String rb)
    {
        this.rb = rb ;
    }

    public String getState()
    {
        return state;
    }

    public void setState(String state)
    {
        this.state = state ;
    }

    public String getCounty()
    {
        return county;
    }

    public void setCounty(String county)
    {
        this.county = county ;
    }

    public String getType()
    {
        return type;
    }

    public void setType(String type)
    {
        this.type = type ;
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

    public String getStream()
    {
        return stream;
    }

    public void setStream(String stream)
    {
        this.stream = stream ;
    }

    public String getGsno()
    {
        return gsno;
    }

    public void setGsno(String gsno)
    {
        this.gsno = gsno ;
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
                getRb() + " " +
                getState() + " " +
                getCounty() + " " +
                getType() + " " +
                getWfo() + " " +
                getHsa() + " " +
                getPost() + " " +
                getStream() + " " +
                getGsno() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of LocViewRecord class

