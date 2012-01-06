// filename: FloodstmtRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:15 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Floodstmt table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class FloodstmtRecord extends DbRecord
{
    private String lid;

    private double impact_value;

    private String statement;

    private String rf;

    private String datestart;

    private String dateend;

    private String impact_pe;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public FloodstmtRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public FloodstmtRecord(FloodstmtRecord origRecord)
    {
        setLid(origRecord.getLid());
        setImpact_value(origRecord.getImpact_value());
        setStatement(origRecord.getStatement());
        setRf(origRecord.getRf());
        setDatestart(origRecord.getDatestart());
        setDateend(origRecord.getDateend());
        setImpact_pe(origRecord.getImpact_pe());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Floodstmt record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public double getImpact_value()
    {
        return impact_value;
    }

    public void setImpact_value(double impact_value)
    {
        this.impact_value = impact_value ;
    }

    public String getStatement()
    {
        return statement;
    }

    public void setStatement(String statement)
    {
        this.statement = statement ;
    }

    public String getRf()
    {
        return rf;
    }

    public void setRf(String rf)
    {
        this.rf = rf ;
    }

    public String getDatestart()
    {
        return datestart;
    }

    public void setDatestart(String datestart)
    {
        this.datestart = datestart ;
    }

    public String getDateend()
    {
        return dateend;
    }

    public void setDateend(String dateend)
    {
        this.dateend = dateend ;
    }

    public String getImpact_pe()
    {
        return impact_pe;
    }

    public void setImpact_pe(String impact_pe)
    {
        this.impact_pe = impact_pe ;
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
                 + " AND impact_value = '" + impact_value + "'" 
                 + " AND rf = '" + rf + "'" 
                 + " AND datestart = '" + datestart + "'" 
                 + " AND dateend = '" + dateend + "'" 
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
                getImpact_value() + " " +
                getStatement() + " " +
                getRf() + " " +
                getDatestart() + " " +
                getDateend() + " " +
                getImpact_pe() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of FloodstmtRecord class

