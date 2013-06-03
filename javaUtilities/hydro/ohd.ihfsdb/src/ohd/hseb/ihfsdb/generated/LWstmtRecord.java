// filename: LWstmtRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              LWstmt table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class LWstmtRecord extends DbRecord
{
    private String lid;

    private String pe;

    private double lower_value;

    private double upper_value;

    private int criteria_rank;

    private String statement;

    private String lw_criteria;

    private String lw_source;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public LWstmtRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public LWstmtRecord(LWstmtRecord origRecord)
    {
        setLid(origRecord.getLid());
        setPe(origRecord.getPe());
        setLower_value(origRecord.getLower_value());
        setUpper_value(origRecord.getUpper_value());
        setCriteria_rank(origRecord.getCriteria_rank());
        setStatement(origRecord.getStatement());
        setLw_criteria(origRecord.getLw_criteria());
        setLw_source(origRecord.getLw_source());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a LWstmt record

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

    public double getLower_value()
    {
        return lower_value;
    }

    public void setLower_value(double lower_value)
    {
        this.lower_value = lower_value ;
    }

    public double getUpper_value()
    {
        return upper_value;
    }

    public void setUpper_value(double upper_value)
    {
        this.upper_value = upper_value ;
    }

    public int getCriteria_rank()
    {
        return criteria_rank;
    }

    public void setCriteria_rank(int criteria_rank)
    {
        this.criteria_rank = criteria_rank ;
    }

    public String getStatement()
    {
        return statement;
    }

    public void setStatement(String statement)
    {
        this.statement = statement ;
    }

    public String getLw_criteria()
    {
        return lw_criteria;
    }

    public void setLw_criteria(String lw_criteria)
    {
        this.lw_criteria = lw_criteria ;
    }

    public String getLw_source()
    {
        return lw_source;
    }

    public void setLw_source(String lw_source)
    {
        this.lw_source = lw_source ;
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
                 + " AND lower_value = '" + lower_value + "'" 
                 + " AND criteria_rank = '" + criteria_rank + "'" 
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
                getLower_value() + " " +
                getUpper_value() + " " +
                getCriteria_rank() + " " +
                getStatement() + " " +
                getLw_criteria() + " " +
                getLw_source() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of LWstmtRecord class

