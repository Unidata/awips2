// filename: RadarLocRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              RadarLoc table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class RadarLocRecord extends DbRecord
{
    private String radid;

    private String name;

    private String radid_prefix;

    private short radar_num;

    private String state;

    private double lat;

    private double lon;

    private double elev;

    private double tower_ht;

    private String use_radar;

    private String office_id;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public RadarLocRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public RadarLocRecord(RadarLocRecord origRecord)
    {
        setRadid(origRecord.getRadid());
        setName(origRecord.getName());
        setRadid_prefix(origRecord.getRadid_prefix());
        setRadar_num(origRecord.getRadar_num());
        setState(origRecord.getState());
        setLat(origRecord.getLat());
        setLon(origRecord.getLon());
        setElev(origRecord.getElev());
        setTower_ht(origRecord.getTower_ht());
        setUse_radar(origRecord.getUse_radar());
        setOffice_id(origRecord.getOffice_id());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a RadarLoc record

    //-----------------------------------------------------------------
    public String getRadid()
    {
        return radid;
    }

    public void setRadid(String radid)
    {
        this.radid = radid ;
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name ;
    }

    public String getRadid_prefix()
    {
        return radid_prefix;
    }

    public void setRadid_prefix(String radid_prefix)
    {
        this.radid_prefix = radid_prefix ;
    }

    public short getRadar_num()
    {
        return radar_num;
    }

    public void setRadar_num(short radar_num)
    {
        this.radar_num = radar_num ;
    }

    public String getState()
    {
        return state;
    }

    public void setState(String state)
    {
        this.state = state ;
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

    public double getElev()
    {
        return elev;
    }

    public void setElev(double elev)
    {
        this.elev = elev ;
    }

    public double getTower_ht()
    {
        return tower_ht;
    }

    public void setTower_ht(double tower_ht)
    {
        this.tower_ht = tower_ht ;
    }

    public String getUse_radar()
    {
        return use_radar;
    }

    public void setUse_radar(String use_radar)
    {
        this.use_radar = use_radar ;
    }

    public String getOffice_id()
    {
        return office_id;
    }

    public void setOffice_id(String office_id)
    {
        this.office_id = office_id ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE radid = '" + radid + "'" 
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
                getRadid() + " " +
                getName() + " " +
                getRadid_prefix() + " " +
                getRadar_num() + " " +
                getState() + " " +
                getLat() + " " +
                getLon() + " " +
                getElev() + " " +
                getTower_ht() + " " +
                getUse_radar() + " " +
                getOffice_id() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of RadarLocRecord class

