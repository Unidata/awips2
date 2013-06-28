// filename: PseudoGageValRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              PseudoGageVal table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class PseudoGageValRecord extends DbRecord
{
    private String pseudo_gage_id;

    private long obstime;

    private double lat;

    private double lon;

    private float gage_value;

    private String man_edited;

    private float prev_gage_value;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public PseudoGageValRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public PseudoGageValRecord(PseudoGageValRecord origRecord)
    {
        setPseudo_gage_id(origRecord.getPseudo_gage_id());
        setObstime(origRecord.getObstime());
        setLat(origRecord.getLat());
        setLon(origRecord.getLon());
        setGage_value(origRecord.getGage_value());
        setMan_edited(origRecord.getMan_edited());
        setPrev_gage_value(origRecord.getPrev_gage_value());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a PseudoGageVal record

    //-----------------------------------------------------------------
    public String getPseudo_gage_id()
    {
        return pseudo_gage_id;
    }

    public void setPseudo_gage_id(String pseudo_gage_id)
    {
        this.pseudo_gage_id = pseudo_gage_id ;
    }

    public long getObstime()
    {
        return obstime;
    }

    public void setObstime(long obstime)
    {
        this.obstime = obstime ;
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

    public float getGage_value()
    {
        return gage_value;
    }

    public void setGage_value(float gage_value)
    {
        this.gage_value = gage_value ;
    }

    public String getMan_edited()
    {
        return man_edited;
    }

    public void setMan_edited(String man_edited)
    {
        this.man_edited = man_edited ;
    }

    public float getPrev_gage_value()
    {
        return prev_gage_value;
    }

    public void setPrev_gage_value(float prev_gage_value)
    {
        this.prev_gage_value = prev_gage_value ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE pseudo_gage_id = '" + pseudo_gage_id + "'" 
                 + " AND obstime = '" +  getDateTimeStringFromLongTime(obstime) + "'" 
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
                getPseudo_gage_id() + " " +
                getDateTimeStringFromLongTime(getObstime()) + " " +
                getLat() + " " +
                getLon() + " " +
                getGage_value() + " " +
                getMan_edited() + " " +
                getPrev_gage_value() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of PseudoGageValRecord class

