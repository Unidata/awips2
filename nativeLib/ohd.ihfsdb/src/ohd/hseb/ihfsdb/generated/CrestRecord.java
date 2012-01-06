// filename: CrestRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:13 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Crest table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class CrestRecord extends DbRecord
{
    private String lid;

    private long datcrst;

    private String cremark;

    private String hw;

    private String jam;

    private String olddatum;

    private int q;

    private double stage;

    private String suppress;

    private String timcrst;

    private String prelim;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public CrestRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public CrestRecord(CrestRecord origRecord)
    {
        setLid(origRecord.getLid());
        setDatcrst(origRecord.getDatcrst());
        setCremark(origRecord.getCremark());
        setHw(origRecord.getHw());
        setJam(origRecord.getJam());
        setOlddatum(origRecord.getOlddatum());
        setQ(origRecord.getQ());
        setStage(origRecord.getStage());
        setSuppress(origRecord.getSuppress());
        setTimcrst(origRecord.getTimcrst());
        setPrelim(origRecord.getPrelim());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Crest record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public long getDatcrst()
    {
        return datcrst;
    }

    public void setDatcrst(long datcrst)
    {
        this.datcrst = datcrst ;
    }

    public String getCremark()
    {
        return cremark;
    }

    public void setCremark(String cremark)
    {
        this.cremark = cremark ;
    }

    public String getHw()
    {
        return hw;
    }

    public void setHw(String hw)
    {
        this.hw = hw ;
    }

    public String getJam()
    {
        return jam;
    }

    public void setJam(String jam)
    {
        this.jam = jam ;
    }

    public String getOlddatum()
    {
        return olddatum;
    }

    public void setOlddatum(String olddatum)
    {
        this.olddatum = olddatum ;
    }

    public int getQ()
    {
        return q;
    }

    public void setQ(int q)
    {
        this.q = q ;
    }

    public double getStage()
    {
        return stage;
    }

    public void setStage(double stage)
    {
        this.stage = stage ;
    }

    public String getSuppress()
    {
        return suppress;
    }

    public void setSuppress(String suppress)
    {
        this.suppress = suppress ;
    }

    public String getTimcrst()
    {
        return timcrst;
    }

    public void setTimcrst(String timcrst)
    {
        this.timcrst = timcrst ;
    }

    public String getPrelim()
    {
        return prelim;
    }

    public void setPrelim(String prelim)
    {
        this.prelim = prelim ;
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
                 + " AND datcrst = '" +  getDateStringFromLongTime(datcrst) + "'" 
                 + " AND timcrst = '" + timcrst + "'" 
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
                getDateStringFromLongTime(getDatcrst()) + " " +
                getCremark() + " " +
                getHw() + " " +
                getJam() + " " +
                getOlddatum() + " " +
                getQ() + " " +
                getStage() + " " +
                getSuppress() + " " +
                getTimcrst() + " " +
                getPrelim() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of CrestRecord class

