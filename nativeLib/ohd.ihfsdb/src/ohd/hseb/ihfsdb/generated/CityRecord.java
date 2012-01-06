// filename: CityRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:13 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              City table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class CityRecord extends DbRecord
{
    private String name;

    private String state;

    private double lat;

    private double lon;

    private int disp_precedence;

    private int population;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public CityRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public CityRecord(CityRecord origRecord)
    {
        setName(origRecord.getName());
        setState(origRecord.getState());
        setLat(origRecord.getLat());
        setLon(origRecord.getLon());
        setDisp_precedence(origRecord.getDisp_precedence());
        setPopulation(origRecord.getPopulation());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a City record

    //-----------------------------------------------------------------
    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name ;
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

    public int getDisp_precedence()
    {
        return disp_precedence;
    }

    public void setDisp_precedence(int disp_precedence)
    {
        this.disp_precedence = disp_precedence ;
    }

    public int getPopulation()
    {
        return population;
    }

    public void setPopulation(int population)
    {
        this.population = population ;
    }

//-----------------------------------------------------------------
//  toString() - this method is called with no arguments
//  and returns a String of the internal values
//-----------------------------------------------------------------
    public String toString()
    {
        String outString = 
                getName() + " " +
                getState() + " " +
                getLat() + " " +
                getLon() + " " +
                getDisp_precedence() + " " +
                getPopulation() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of CityRecord class

