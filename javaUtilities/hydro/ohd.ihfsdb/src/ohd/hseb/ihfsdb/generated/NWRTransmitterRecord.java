// filename: NWRTransmitterRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              NWRTransmitter table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class NWRTransmitterRecord extends DbRecord
{
    private String call_sign;

    private String wfo;

    private String city;

    private String county;

    private String state;

    private String coverage_area;

    private double lat;

    private double lon;

    private double transmit_freq;

    private int transmit_power;

    private String transmit_prod_code;

    private String transmit_countynum;

    private String use_transmitter;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public NWRTransmitterRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public NWRTransmitterRecord(NWRTransmitterRecord origRecord)
    {
        setCall_sign(origRecord.getCall_sign());
        setWfo(origRecord.getWfo());
        setCity(origRecord.getCity());
        setCounty(origRecord.getCounty());
        setState(origRecord.getState());
        setCoverage_area(origRecord.getCoverage_area());
        setLat(origRecord.getLat());
        setLon(origRecord.getLon());
        setTransmit_freq(origRecord.getTransmit_freq());
        setTransmit_power(origRecord.getTransmit_power());
        setTransmit_prod_code(origRecord.getTransmit_prod_code());
        setTransmit_countynum(origRecord.getTransmit_countynum());
        setUse_transmitter(origRecord.getUse_transmitter());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a NWRTransmitter record

    //-----------------------------------------------------------------
    public String getCall_sign()
    {
        return call_sign;
    }

    public void setCall_sign(String call_sign)
    {
        this.call_sign = call_sign ;
    }

    public String getWfo()
    {
        return wfo;
    }

    public void setWfo(String wfo)
    {
        this.wfo = wfo ;
    }

    public String getCity()
    {
        return city;
    }

    public void setCity(String city)
    {
        this.city = city ;
    }

    public String getCounty()
    {
        return county;
    }

    public void setCounty(String county)
    {
        this.county = county ;
    }

    public String getState()
    {
        return state;
    }

    public void setState(String state)
    {
        this.state = state ;
    }

    public String getCoverage_area()
    {
        return coverage_area;
    }

    public void setCoverage_area(String coverage_area)
    {
        this.coverage_area = coverage_area ;
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

    public double getTransmit_freq()
    {
        return transmit_freq;
    }

    public void setTransmit_freq(double transmit_freq)
    {
        this.transmit_freq = transmit_freq ;
    }

    public int getTransmit_power()
    {
        return transmit_power;
    }

    public void setTransmit_power(int transmit_power)
    {
        this.transmit_power = transmit_power ;
    }

    public String getTransmit_prod_code()
    {
        return transmit_prod_code;
    }

    public void setTransmit_prod_code(String transmit_prod_code)
    {
        this.transmit_prod_code = transmit_prod_code ;
    }

    public String getTransmit_countynum()
    {
        return transmit_countynum;
    }

    public void setTransmit_countynum(String transmit_countynum)
    {
        this.transmit_countynum = transmit_countynum ;
    }

    public String getUse_transmitter()
    {
        return use_transmitter;
    }

    public void setUse_transmitter(String use_transmitter)
    {
        this.use_transmitter = use_transmitter ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE call_sign = '" + call_sign + "'" 
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
                getCall_sign() + " " +
                getWfo() + " " +
                getCity() + " " +
                getCounty() + " " +
                getState() + " " +
                getCoverage_area() + " " +
                getLat() + " " +
                getLon() + " " +
                getTransmit_freq() + " " +
                getTransmit_power() + " " +
                getTransmit_prod_code() + " " +
                getTransmit_countynum() + " " +
                getUse_transmitter() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of NWRTransmitterRecord class

