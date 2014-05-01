// filename: PubRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Pub table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class PubRecord extends DbRecord
{
    private String lid;

    private long pbegin;

    private String ppub;

    private long pend;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public PubRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public PubRecord(PubRecord origRecord)
    {
        setLid(origRecord.getLid());
        setPbegin(origRecord.getPbegin());
        setPpub(origRecord.getPpub());
        setPend(origRecord.getPend());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Pub record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public long getPbegin()
    {
        return pbegin;
    }

    public void setPbegin(long pbegin)
    {
        this.pbegin = pbegin ;
    }

    public String getPpub()
    {
        return ppub;
    }

    public void setPpub(String ppub)
    {
        this.ppub = ppub ;
    }

    public long getPend()
    {
        return pend;
    }

    public void setPend(long pend)
    {
        this.pend = pend ;
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
                 + " AND pbegin = '" +  getDateStringFromLongTime(pbegin) + "'" 
                 + " AND ppub = '" + ppub + "'" 
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
                getDateStringFromLongTime(getPbegin()) + " " +
                getPpub() + " " +
                getDateStringFromLongTime(getPend()) + " " +
                "" ;
        return outString;
    } // end toString()
} // end of PubRecord class

