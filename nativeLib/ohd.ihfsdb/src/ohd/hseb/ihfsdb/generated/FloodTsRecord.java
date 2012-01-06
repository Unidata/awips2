// filename: FloodTsRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:15 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              FloodTs table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class FloodTsRecord extends DbRecord
{
    private String lid;

    private long obstime;

    private int flood_event_id;

    private double value;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public FloodTsRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public FloodTsRecord(FloodTsRecord origRecord)
    {
        setLid(origRecord.getLid());
        setObstime(origRecord.getObstime());
        setFlood_event_id(origRecord.getFlood_event_id());
        setValue(origRecord.getValue());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a FloodTs record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public long getObstime()
    {
        return obstime;
    }

    public void setObstime(long obstime)
    {
        this.obstime = obstime ;
    }

    public int getFlood_event_id()
    {
        return flood_event_id;
    }

    public void setFlood_event_id(int flood_event_id)
    {
        this.flood_event_id = flood_event_id ;
    }

    public double getValue()
    {
        return value;
    }

    public void setValue(double value)
    {
        this.value = value ;
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
                getLid() + " " +
                getDateTimeStringFromLongTime(getObstime()) + " " +
                getFlood_event_id() + " " +
                getValue() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of FloodTsRecord class

