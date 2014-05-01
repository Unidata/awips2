// filename: LocationRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Location table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class LocationRecord extends DbRecord
{
    private String lid;

    private String county;

    private String coe;

    private String cpm;

    private String detail;

    private double elev;

    private String hdatum;

    private String hsa;

    private String hu;

    private double lat;

    private double lon;

    private String lremark;

    private long lrevise;

    private String name;

    private String network;

    private String rb;

    private String rfc;

    private long sbd;

    private String sn;

    private String state;

    private String waro;

    private String wfo;

    private String wsfo;

    private String type;

    private String des;

    private String det;

    private int post;

    private String stntype;

    private String tzone;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public LocationRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public LocationRecord(LocationRecord origRecord)
    {
        setLid(origRecord.getLid());
        setCounty(origRecord.getCounty());
        setCoe(origRecord.getCoe());
        setCpm(origRecord.getCpm());
        setDetail(origRecord.getDetail());
        setElev(origRecord.getElev());
        setHdatum(origRecord.getHdatum());
        setHsa(origRecord.getHsa());
        setHu(origRecord.getHu());
        setLat(origRecord.getLat());
        setLon(origRecord.getLon());
        setLremark(origRecord.getLremark());
        setLrevise(origRecord.getLrevise());
        setName(origRecord.getName());
        setNetwork(origRecord.getNetwork());
        setRb(origRecord.getRb());
        setRfc(origRecord.getRfc());
        setSbd(origRecord.getSbd());
        setSn(origRecord.getSn());
        setState(origRecord.getState());
        setWaro(origRecord.getWaro());
        setWfo(origRecord.getWfo());
        setWsfo(origRecord.getWsfo());
        setType(origRecord.getType());
        setDes(origRecord.getDes());
        setDet(origRecord.getDet());
        setPost(origRecord.getPost());
        setStntype(origRecord.getStntype());
        setTzone(origRecord.getTzone());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Location record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getCounty()
    {
        return county;
    }

    public void setCounty(String county)
    {
        this.county = county ;
    }

    public String getCoe()
    {
        return coe;
    }

    public void setCoe(String coe)
    {
        this.coe = coe ;
    }

    public String getCpm()
    {
        return cpm;
    }

    public void setCpm(String cpm)
    {
        this.cpm = cpm ;
    }

    public String getDetail()
    {
        return detail;
    }

    public void setDetail(String detail)
    {
        this.detail = detail ;
    }

    public double getElev()
    {
        return elev;
    }

    public void setElev(double elev)
    {
        this.elev = elev ;
    }

    public String getHdatum()
    {
        return hdatum;
    }

    public void setHdatum(String hdatum)
    {
        this.hdatum = hdatum ;
    }

    public String getHsa()
    {
        return hsa;
    }

    public void setHsa(String hsa)
    {
        this.hsa = hsa ;
    }

    public String getHu()
    {
        return hu;
    }

    public void setHu(String hu)
    {
        this.hu = hu ;
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

    public String getLremark()
    {
        return lremark;
    }

    public void setLremark(String lremark)
    {
        this.lremark = lremark ;
    }

    public long getLrevise()
    {
        return lrevise;
    }

    public void setLrevise(long lrevise)
    {
        this.lrevise = lrevise ;
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name ;
    }

    public String getNetwork()
    {
        return network;
    }

    public void setNetwork(String network)
    {
        this.network = network ;
    }

    public String getRb()
    {
        return rb;
    }

    public void setRb(String rb)
    {
        this.rb = rb ;
    }

    public String getRfc()
    {
        return rfc;
    }

    public void setRfc(String rfc)
    {
        this.rfc = rfc ;
    }

    public long getSbd()
    {
        return sbd;
    }

    public void setSbd(long sbd)
    {
        this.sbd = sbd ;
    }

    public String getSn()
    {
        return sn;
    }

    public void setSn(String sn)
    {
        this.sn = sn ;
    }

    public String getState()
    {
        return state;
    }

    public void setState(String state)
    {
        this.state = state ;
    }

    public String getWaro()
    {
        return waro;
    }

    public void setWaro(String waro)
    {
        this.waro = waro ;
    }

    public String getWfo()
    {
        return wfo;
    }

    public void setWfo(String wfo)
    {
        this.wfo = wfo ;
    }

    public String getWsfo()
    {
        return wsfo;
    }

    public void setWsfo(String wsfo)
    {
        this.wsfo = wsfo ;
    }

    public String getType()
    {
        return type;
    }

    public void setType(String type)
    {
        this.type = type ;
    }

    public String getDes()
    {
        return des;
    }

    public void setDes(String des)
    {
        this.des = des ;
    }

    public String getDet()
    {
        return det;
    }

    public void setDet(String det)
    {
        this.det = det ;
    }

    public int getPost()
    {
        return post;
    }

    public void setPost(int post)
    {
        this.post = post ;
    }

    public String getStntype()
    {
        return stntype;
    }

    public void setStntype(String stntype)
    {
        this.stntype = stntype ;
    }

    public String getTzone()
    {
        return tzone;
    }

    public void setTzone(String tzone)
    {
        this.tzone = tzone ;
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
                getCounty() + " " +
                getCoe() + " " +
                getCpm() + " " +
                getDetail() + " " +
                getElev() + " " +
                getHdatum() + " " +
                getHsa() + " " +
                getHu() + " " +
                getLat() + " " +
                getLon() + " " +
                getLremark() + " " +
                getDateStringFromLongTime(getLrevise()) + " " +
                getName() + " " +
                getNetwork() + " " +
                getRb() + " " +
                getRfc() + " " +
                getDateStringFromLongTime(getSbd()) + " " +
                getSn() + " " +
                getState() + " " +
                getWaro() + " " +
                getWfo() + " " +
                getWsfo() + " " +
                getType() + " " +
                getDes() + " " +
                getDet() + " " +
                getPost() + " " +
                getStntype() + " " +
                getTzone() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of LocationRecord class

