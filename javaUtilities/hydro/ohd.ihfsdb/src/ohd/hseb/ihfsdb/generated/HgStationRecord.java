// filename: HgStationRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:15 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              HgStation table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class HgStationRecord extends DbRecord
{
    private String lid;

    private String pe;

    private String ts;

    private String fcstts;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public HgStationRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public HgStationRecord(HgStationRecord origRecord)
    {
        setLid(origRecord.getLid());
        setPe(origRecord.getPe());
        setTs(origRecord.getTs());
        setFcstts(origRecord.getFcstts());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a HgStation record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getPe()
    {
        return pe;
    }

    public void setPe(String pe)
    {
        this.pe = pe ;
    }

    public String getTs()
    {
        return ts;
    }

    public void setTs(String ts)
    {
        this.ts = ts ;
    }

    public String getFcstts()
    {
        return fcstts;
    }

    public void setFcstts(String fcstts)
    {
        this.fcstts = fcstts ;
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
                 + " AND pe = '" + pe + "'" 
                 + " AND ts = '" + ts + "'" 
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
                getPe() + " " +
                getTs() + " " +
                getFcstts() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of HgStationRecord class

