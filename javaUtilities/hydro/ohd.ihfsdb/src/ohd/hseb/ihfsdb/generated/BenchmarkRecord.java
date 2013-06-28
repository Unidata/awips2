// filename: BenchmarkRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:13 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Benchmark table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class BenchmarkRecord extends DbRecord
{
    private String lid;

    private String bnum;

    private double elev;

    private String remark;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public BenchmarkRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public BenchmarkRecord(BenchmarkRecord origRecord)
    {
        setLid(origRecord.getLid());
        setBnum(origRecord.getBnum());
        setElev(origRecord.getElev());
        setRemark(origRecord.getRemark());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Benchmark record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getBnum()
    {
        return bnum;
    }

    public void setBnum(String bnum)
    {
        this.bnum = bnum ;
    }

    public double getElev()
    {
        return elev;
    }

    public void setElev(double elev)
    {
        this.elev = elev ;
    }

    public String getRemark()
    {
        return remark;
    }

    public void setRemark(String remark)
    {
        this.remark = remark ;
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
                 + " AND bnum = '" + bnum + "'" 
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
                getBnum() + " " +
                getElev() + " " +
                getRemark() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of BenchmarkRecord class

