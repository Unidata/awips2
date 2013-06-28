// filename: ColorValueRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:13 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              ColorValue table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class ColorValueRecord extends DbRecord
{
    private String userid;

    private String application_name;

    private String color_use_name;

    private int duration;

    private double threshold_value;

    private String threshold_unit;

    private String color_name;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public ColorValueRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public ColorValueRecord(ColorValueRecord origRecord)
    {
        setUserid(origRecord.getUserid());
        setApplication_name(origRecord.getApplication_name());
        setColor_use_name(origRecord.getColor_use_name());
        setDuration(origRecord.getDuration());
        setThreshold_value(origRecord.getThreshold_value());
        setThreshold_unit(origRecord.getThreshold_unit());
        setColor_name(origRecord.getColor_name());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a ColorValue record

    //-----------------------------------------------------------------
    public String getUserid()
    {
        return userid;
    }

    public void setUserid(String userid)
    {
        this.userid = userid ;
    }

    public String getApplication_name()
    {
        return application_name;
    }

    public void setApplication_name(String application_name)
    {
        this.application_name = application_name ;
    }

    public String getColor_use_name()
    {
        return color_use_name;
    }

    public void setColor_use_name(String color_use_name)
    {
        this.color_use_name = color_use_name ;
    }

    public int getDuration()
    {
        return duration;
    }

    public void setDuration(int duration)
    {
        this.duration = duration ;
    }

    public double getThreshold_value()
    {
        return threshold_value;
    }

    public void setThreshold_value(double threshold_value)
    {
        this.threshold_value = threshold_value ;
    }

    public String getThreshold_unit()
    {
        return threshold_unit;
    }

    public void setThreshold_unit(String threshold_unit)
    {
        this.threshold_unit = threshold_unit ;
    }

    public String getColor_name()
    {
        return color_name;
    }

    public void setColor_name(String color_name)
    {
        this.color_name = color_name ;
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
                 + " AND application_name = '" + application_name + "'" 
                 + " AND color_use_name = '" + color_use_name + "'" 
                 + " AND duration = '" + duration + "'" 
                 + " AND threshold_value = '" + threshold_value + "'" 
                 + " AND threshold_unit = '" + threshold_unit + "'" 
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
                getApplication_name() + " " +
                getColor_use_name() + " " +
                getDuration() + " " +
                getThreshold_value() + " " +
                getThreshold_unit() + " " +
                getColor_name() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of ColorValueRecord class

