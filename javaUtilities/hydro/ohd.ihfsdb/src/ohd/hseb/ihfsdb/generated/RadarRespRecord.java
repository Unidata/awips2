// filename: RadarRespRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              RadarResp table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class RadarRespRecord extends DbRecord
{
    private String radid;

    private String site_id;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public RadarRespRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public RadarRespRecord(RadarRespRecord origRecord)
    {
        setRadid(origRecord.getRadid());
        setSite_id(origRecord.getSite_id());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a RadarResp record

    //-----------------------------------------------------------------
    public String getRadid()
    {
        return radid;
    }

    public void setRadid(String radid)
    {
        this.radid = radid ;
    }

    public String getSite_id()
    {
        return site_id;
    }

    public void setSite_id(String site_id)
    {
        this.site_id = site_id ;
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
                getSite_id() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of RadarRespRecord class

