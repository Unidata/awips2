// filename: DescripRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:14 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Descrip table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class DescripRecord extends DbRecord
{
    private String lid;

    private String bed;

    private String divert;

    private String remark;

    private String ice;

    private String proximity;

    private String reach;

    private String res;

    private String topo;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public DescripRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public DescripRecord(DescripRecord origRecord)
    {
        setLid(origRecord.getLid());
        setBed(origRecord.getBed());
        setDivert(origRecord.getDivert());
        setRemark(origRecord.getRemark());
        setIce(origRecord.getIce());
        setProximity(origRecord.getProximity());
        setReach(origRecord.getReach());
        setRes(origRecord.getRes());
        setTopo(origRecord.getTopo());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Descrip record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getBed()
    {
        return bed;
    }

    public void setBed(String bed)
    {
        this.bed = bed ;
    }

    public String getDivert()
    {
        return divert;
    }

    public void setDivert(String divert)
    {
        this.divert = divert ;
    }

    public String getRemark()
    {
        return remark;
    }

    public void setRemark(String remark)
    {
        this.remark = remark ;
    }

    public String getIce()
    {
        return ice;
    }

    public void setIce(String ice)
    {
        this.ice = ice ;
    }

    public String getProximity()
    {
        return proximity;
    }

    public void setProximity(String proximity)
    {
        this.proximity = proximity ;
    }

    public String getReach()
    {
        return reach;
    }

    public void setReach(String reach)
    {
        this.reach = reach ;
    }

    public String getRes()
    {
        return res;
    }

    public void setRes(String res)
    {
        this.res = res ;
    }

    public String getTopo()
    {
        return topo;
    }

    public void setTopo(String topo)
    {
        this.topo = topo ;
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
                getBed() + " " +
                getDivert() + " " +
                getRemark() + " " +
                getIce() + " " +
                getProximity() + " " +
                getReach() + " " +
                getRes() + " " +
                getTopo() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of DescripRecord class

