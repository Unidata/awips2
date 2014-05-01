// filename: IngestFilterRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:15 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              IngestFilter table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class IngestFilterRecord extends DbRecord
{
    private String lid;

    private String pe;

    private short dur;

    private String ts;

    private String extremum;

    private short ts_rank;

    private String ingest;

    private String ofs_input;

    private String stg2_input;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public IngestFilterRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public IngestFilterRecord(IngestFilterRecord origRecord)
    {
        setLid(origRecord.getLid());
        setPe(origRecord.getPe());
        setDur(origRecord.getDur());
        setTs(origRecord.getTs());
        setExtremum(origRecord.getExtremum());
        setTs_rank(origRecord.getTs_rank());
        setIngest(origRecord.getIngest());
        setOfs_input(origRecord.getOfs_input());
        setStg2_input(origRecord.getStg2_input());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a IngestFilter record

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

    public short getDur()
    {
        return dur;
    }

    public void setDur(short dur)
    {
        this.dur = dur ;
    }

    public String getTs()
    {
        return ts;
    }

    public void setTs(String ts)
    {
        this.ts = ts ;
    }

    public String getExtremum()
    {
        return extremum;
    }

    public void setExtremum(String extremum)
    {
        this.extremum = extremum ;
    }

    public short getTs_rank()
    {
        return ts_rank;
    }

    public void setTs_rank(short ts_rank)
    {
        this.ts_rank = ts_rank ;
    }

    public String getIngest()
    {
        return ingest;
    }

    public void setIngest(String ingest)
    {
        this.ingest = ingest ;
    }

    public String getOfs_input()
    {
        return ofs_input;
    }

    public void setOfs_input(String ofs_input)
    {
        this.ofs_input = ofs_input ;
    }

    public String getStg2_input()
    {
        return stg2_input;
    }

    public void setStg2_input(String stg2_input)
    {
        this.stg2_input = stg2_input ;
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
                 + " AND dur = '" + dur + "'" 
                 + " AND ts = '" + ts + "'" 
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
                getLid() + " " +
                getPe() + " " +
                getDur() + " " +
                getTs() + " " +
                getExtremum() + " " +
                getTs_rank() + " " +
                getIngest() + " " +
                getOfs_input() + " " +
                getStg2_input() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of IngestFilterRecord class

