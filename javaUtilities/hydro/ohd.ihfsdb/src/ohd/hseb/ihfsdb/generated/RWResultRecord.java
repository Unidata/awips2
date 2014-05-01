// filename: RWResultRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              RWResult table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class RWResultRecord extends DbRecord
{
    private String rfc;

    private long obstime;

    private short num_gag_avail;

    private int num_rad_avail;

    private int num_pseudo_gages;

    private String sat_avail;

    private String mapx_field_type;

    private String draw_precip;

    private String auto_save;

    private long last_exec_time;

    private long last_save_time;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public RWResultRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public RWResultRecord(RWResultRecord origRecord)
    {
        setRfc(origRecord.getRfc());
        setObstime(origRecord.getObstime());
        setNum_gag_avail(origRecord.getNum_gag_avail());
        setNum_rad_avail(origRecord.getNum_rad_avail());
        setNum_pseudo_gages(origRecord.getNum_pseudo_gages());
        setSat_avail(origRecord.getSat_avail());
        setMapx_field_type(origRecord.getMapx_field_type());
        setDraw_precip(origRecord.getDraw_precip());
        setAuto_save(origRecord.getAuto_save());
        setLast_exec_time(origRecord.getLast_exec_time());
        setLast_save_time(origRecord.getLast_save_time());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a RWResult record

    //-----------------------------------------------------------------
    public String getRfc()
    {
        return rfc;
    }

    public void setRfc(String rfc)
    {
        this.rfc = rfc ;
    }

    public long getObstime()
    {
        return obstime;
    }

    public void setObstime(long obstime)
    {
        this.obstime = obstime ;
    }

    public short getNum_gag_avail()
    {
        return num_gag_avail;
    }

    public void setNum_gag_avail(short num_gag_avail)
    {
        this.num_gag_avail = num_gag_avail ;
    }

    public int getNum_rad_avail()
    {
        return num_rad_avail;
    }

    public void setNum_rad_avail(int num_rad_avail)
    {
        this.num_rad_avail = num_rad_avail ;
    }

    public int getNum_pseudo_gages()
    {
        return num_pseudo_gages;
    }

    public void setNum_pseudo_gages(int num_pseudo_gages)
    {
        this.num_pseudo_gages = num_pseudo_gages ;
    }

    public String getSat_avail()
    {
        return sat_avail;
    }

    public void setSat_avail(String sat_avail)
    {
        this.sat_avail = sat_avail ;
    }

    public String getMapx_field_type()
    {
        return mapx_field_type;
    }

    public void setMapx_field_type(String mapx_field_type)
    {
        this.mapx_field_type = mapx_field_type ;
    }

    public String getDraw_precip()
    {
        return draw_precip;
    }

    public void setDraw_precip(String draw_precip)
    {
        this.draw_precip = draw_precip ;
    }

    public String getAuto_save()
    {
        return auto_save;
    }

    public void setAuto_save(String auto_save)
    {
        this.auto_save = auto_save ;
    }

    public long getLast_exec_time()
    {
        return last_exec_time;
    }

    public void setLast_exec_time(long last_exec_time)
    {
        this.last_exec_time = last_exec_time ;
    }

    public long getLast_save_time()
    {
        return last_save_time;
    }

    public void setLast_save_time(long last_save_time)
    {
        this.last_save_time = last_save_time ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE rfc = '" + rfc + "'" 
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
                getRfc() + " " +
                getDateTimeStringFromLongTime(getObstime()) + " " +
                getNum_gag_avail() + " " +
                getNum_rad_avail() + " " +
                getNum_pseudo_gages() + " " +
                getSat_avail() + " " +
                getMapx_field_type() + " " +
                getDraw_precip() + " " +
                getAuto_save() + " " +
                getDateTimeStringFromLongTime(getLast_exec_time()) + " " +
                getDateTimeStringFromLongTime(getLast_save_time()) + " " +
                "" ;
        return outString;
    } // end toString()
} // end of RWResultRecord class

