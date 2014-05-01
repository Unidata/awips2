// filename: OFSDataTransRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              OFSDataTrans table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class OFSDataTransRecord extends DbRecord
{
    private String pe;

    private short dur;

    private String extremum;

    private String ofs_data_type;

    private float fwd_time_window;

    private float bkw_time_window;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public OFSDataTransRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public OFSDataTransRecord(OFSDataTransRecord origRecord)
    {
        setPe(origRecord.getPe());
        setDur(origRecord.getDur());
        setExtremum(origRecord.getExtremum());
        setOfs_data_type(origRecord.getOfs_data_type());
        setFwd_time_window(origRecord.getFwd_time_window());
        setBkw_time_window(origRecord.getBkw_time_window());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a OFSDataTrans record

    //-----------------------------------------------------------------
    public String getPe()
    {
        return pe;
    }

    public void setPe(String pe)
    {
        this.pe = pe ;
    }

    public short getDur()
    {
        return dur;
    }

    public void setDur(short dur)
    {
        this.dur = dur ;
    }

    public String getExtremum()
    {
        return extremum;
    }

    public void setExtremum(String extremum)
    {
        this.extremum = extremum ;
    }

    public String getOfs_data_type()
    {
        return ofs_data_type;
    }

    public void setOfs_data_type(String ofs_data_type)
    {
        this.ofs_data_type = ofs_data_type ;
    }

    public float getFwd_time_window()
    {
        return fwd_time_window;
    }

    public void setFwd_time_window(float fwd_time_window)
    {
        this.fwd_time_window = fwd_time_window ;
    }

    public float getBkw_time_window()
    {
        return bkw_time_window;
    }

    public void setBkw_time_window(float bkw_time_window)
    {
        this.bkw_time_window = bkw_time_window ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE pe = '" + pe + "'" 
                 + " AND dur = '" + dur + "'" 
                 + " AND extremum = '" + extremum + "'" 
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
                getPe() + " " +
                getDur() + " " +
                getExtremum() + " " +
                getOfs_data_type() + " " +
                getFwd_time_window() + " " +
                getBkw_time_window() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of OFSDataTransRecord class

