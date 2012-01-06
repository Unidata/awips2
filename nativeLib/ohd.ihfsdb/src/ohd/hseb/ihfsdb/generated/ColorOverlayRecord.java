// filename: ColorOverlayRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:13 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              ColorOverlay table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class ColorOverlayRecord extends DbRecord
{
    private String userid;

    private String application_name;

    private String overlay_type;

    private String color_name;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public ColorOverlayRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public ColorOverlayRecord(ColorOverlayRecord origRecord)
    {
        setUserid(origRecord.getUserid());
        setApplication_name(origRecord.getApplication_name());
        setOverlay_type(origRecord.getOverlay_type());
        setColor_name(origRecord.getColor_name());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a ColorOverlay record

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

    public String getOverlay_type()
    {
        return overlay_type;
    }

    public void setOverlay_type(String overlay_type)
    {
        this.overlay_type = overlay_type ;
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
                 + " AND overlay_type = '" + overlay_type + "'" 
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
                getOverlay_type() + " " +
                getColor_name() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of ColorOverlayRecord class

