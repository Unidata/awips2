// filename: TelemRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Telem table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class TelemRecord extends DbRecord
{
    private String lid;

    private String type;

    private String payor;

    private double cost;

    private String criteria;

    private String owner;

    private String phone;

    private String sensorid;

    private String rptfreq;

    private String notify;

    private String obsvfreq;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public TelemRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public TelemRecord(TelemRecord origRecord)
    {
        setLid(origRecord.getLid());
        setType(origRecord.getType());
        setPayor(origRecord.getPayor());
        setCost(origRecord.getCost());
        setCriteria(origRecord.getCriteria());
        setOwner(origRecord.getOwner());
        setPhone(origRecord.getPhone());
        setSensorid(origRecord.getSensorid());
        setRptfreq(origRecord.getRptfreq());
        setNotify(origRecord.getNotify());
        setObsvfreq(origRecord.getObsvfreq());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Telem record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getType()
    {
        return type;
    }

    public void setType(String type)
    {
        this.type = type ;
    }

    public String getPayor()
    {
        return payor;
    }

    public void setPayor(String payor)
    {
        this.payor = payor ;
    }

    public double getCost()
    {
        return cost;
    }

    public void setCost(double cost)
    {
        this.cost = cost ;
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

    public String getPhone()
    {
        return phone;
    }

    public void setPhone(String phone)
    {
        this.phone = phone ;
    }

    public String getSensorid()
    {
        return sensorid;
    }

    public void setSensorid(String sensorid)
    {
        this.sensorid = sensorid ;
    }

    public String getRptfreq()
    {
        return rptfreq;
    }

    public void setRptfreq(String rptfreq)
    {
        this.rptfreq = rptfreq ;
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
                getType() + " " +
                getPayor() + " " +
                getCost() + " " +
                getCriteria() + " " +
                getOwner() + " " +
                getPhone() + " " +
                getSensorid() + " " +
                getRptfreq() + " " +
                getNotify() + " " +
                getObsvfreq() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of TelemRecord class

