// filename: DcpRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:14 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Dcp table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class DcpRecord extends DbRecord
{
    private String lid;

    private String criteria;

    private String owner;

    private String goes;

    private String rptfreq;

    private String rptime;

    private String notify;

    private String obsvfreq;

    private String randrept;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public DcpRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public DcpRecord(DcpRecord origRecord)
    {
        setLid(origRecord.getLid());
        setCriteria(origRecord.getCriteria());
        setOwner(origRecord.getOwner());
        setGoes(origRecord.getGoes());
        setRptfreq(origRecord.getRptfreq());
        setRptime(origRecord.getRptime());
        setNotify(origRecord.getNotify());
        setObsvfreq(origRecord.getObsvfreq());
        setRandrept(origRecord.getRandrept());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Dcp record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getCriteria()
    {
        return criteria;
    }

    public void setCriteria(String criteria)
    {
        this.criteria = criteria ;
    }

    public String getOwner()
    {
        return owner;
    }

    public void setOwner(String owner)
    {
        this.owner = owner ;
    }

    public String getGoes()
    {
        return goes;
    }

    public void setGoes(String goes)
    {
        this.goes = goes ;
    }

    public String getRptfreq()
    {
        return rptfreq;
    }

    public void setRptfreq(String rptfreq)
    {
        this.rptfreq = rptfreq ;
    }

    public String getRptime()
    {
        return rptime;
    }

    public void setRptime(String rptime)
    {
        this.rptime = rptime ;
    }

    public String getNotify()
    {
        return notify;
    }

    public void setNotify(String notify)
    {
        this.notify = notify ;
    }

    public String getObsvfreq()
    {
        return obsvfreq;
    }

    public void setObsvfreq(String obsvfreq)
    {
        this.obsvfreq = obsvfreq ;
    }

    public String getRandrept()
    {
        return randrept;
    }

    public void setRandrept(String randrept)
    {
        this.randrept = randrept ;
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
                getCriteria() + " " +
                getOwner() + " " +
                getGoes() + " " +
                getRptfreq() + " " +
                getRptime() + " " +
                getNotify() + " " +
                getObsvfreq() + " " +
                getRandrept() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of DcpRecord class

