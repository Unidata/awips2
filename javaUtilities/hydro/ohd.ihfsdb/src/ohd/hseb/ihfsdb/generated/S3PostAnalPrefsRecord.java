// filename: S3PostAnalPrefsRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              S3PostAnalPrefs table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class S3PostAnalPrefsRecord extends DbRecord
{
    private String userid;

    private String state_overlay;

    private String city_overlay;

    private String river_overlay;

    private String basin_overlay;

    private String radar_overlay;

    private short num_hours_wind;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public S3PostAnalPrefsRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public S3PostAnalPrefsRecord(S3PostAnalPrefsRecord origRecord)
    {
        setUserid(origRecord.getUserid());
        setState_overlay(origRecord.getState_overlay());
        setCity_overlay(origRecord.getCity_overlay());
        setRiver_overlay(origRecord.getRiver_overlay());
        setBasin_overlay(origRecord.getBasin_overlay());
        setRadar_overlay(origRecord.getRadar_overlay());
        setNum_hours_wind(origRecord.getNum_hours_wind());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a S3PostAnalPrefs record

    //-----------------------------------------------------------------
    public String getUserid()
    {
        return userid;
    }

    public void setUserid(String userid)
    {
        this.userid = userid ;
    }

    public String getState_overlay()
    {
        return state_overlay;
    }

    public void setState_overlay(String state_overlay)
    {
        this.state_overlay = state_overlay ;
    }

    public String getCity_overlay()
    {
        return city_overlay;
    }

    public void setCity_overlay(String city_overlay)
    {
        this.city_overlay = city_overlay ;
    }

    public String getRiver_overlay()
    {
        return river_overlay;
    }

    public void setRiver_overlay(String river_overlay)
    {
        this.river_overlay = river_overlay ;
    }

    public String getBasin_overlay()
    {
        return basin_overlay;
    }

    public void setBasin_overlay(String basin_overlay)
    {
        this.basin_overlay = basin_overlay ;
    }

    public String getRadar_overlay()
    {
        return radar_overlay;
    }

    public void setRadar_overlay(String radar_overlay)
    {
        this.radar_overlay = radar_overlay ;
    }

    public short getNum_hours_wind()
    {
        return num_hours_wind;
    }

    public void setNum_hours_wind(short num_hours_wind)
    {
        this.num_hours_wind = num_hours_wind ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE userid = '" + userid + "'" 
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
                getUserid() + " " +
                getState_overlay() + " " +
                getCity_overlay() + " " +
                getRiver_overlay() + " " +
                getBasin_overlay() + " " +
                getRadar_overlay() + " " +
                getNum_hours_wind() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of S3PostAnalPrefsRecord class

