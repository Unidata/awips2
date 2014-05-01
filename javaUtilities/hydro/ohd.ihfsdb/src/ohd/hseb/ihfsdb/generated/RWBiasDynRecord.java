// filename: RWBiasDynRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              RWBiasDyn table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class RWBiasDynRecord extends DbRecord
{
    private String radid;

    private String office_id;

    private long obstime;

    private short memspan_ind;

    private double numpairs;

    private float sumgag;

    private float sumrad;

    private float bias;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public RWBiasDynRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public RWBiasDynRecord(RWBiasDynRecord origRecord)
    {
        setRadid(origRecord.getRadid());
        setOffice_id(origRecord.getOffice_id());
        setObstime(origRecord.getObstime());
        setMemspan_ind(origRecord.getMemspan_ind());
        setNumpairs(origRecord.getNumpairs());
        setSumgag(origRecord.getSumgag());
        setSumrad(origRecord.getSumrad());
        setBias(origRecord.getBias());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a RWBiasDyn record

    //-----------------------------------------------------------------
    public String getRadid()
    {
        return radid;
    }

    public void setRadid(String radid)
    {
        this.radid = radid ;
    }

    public String getOffice_id()
    {
        return office_id;
    }

    public void setOffice_id(String office_id)
    {
        this.office_id = office_id ;
    }

    public long getObstime()
    {
        return obstime;
    }

    public void setObstime(long obstime)
    {
        this.obstime = obstime ;
    }

    public short getMemspan_ind()
    {
        return memspan_ind;
    }

    public void setMemspan_ind(short memspan_ind)
    {
        this.memspan_ind = memspan_ind ;
    }

    public double getNumpairs()
    {
        return numpairs;
    }

    public void setNumpairs(double numpairs)
    {
        this.numpairs = numpairs ;
    }

    public float getSumgag()
    {
        return sumgag;
    }

    public void setSumgag(float sumgag)
    {
        this.sumgag = sumgag ;
    }

    public float getSumrad()
    {
        return sumrad;
    }

    public void setSumrad(float sumrad)
    {
        this.sumrad = sumrad ;
    }

    public float getBias()
    {
        return bias;
    }

    public void setBias(float bias)
    {
        this.bias = bias ;
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
                 + " AND office_id = '" + office_id + "'" 
                 + " AND obstime = '" +  getDateTimeStringFromLongTime(obstime) + "'" 
                 + " AND memspan_ind = '" + memspan_ind + "'" 
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
                getOffice_id() + " " +
                getDateTimeStringFromLongTime(getObstime()) + " " +
                getMemspan_ind() + " " +
                getNumpairs() + " " +
                getSumgag() + " " +
                getSumrad() + " " +
                getBias() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of RWBiasDynRecord class

