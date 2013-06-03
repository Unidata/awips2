// filename: FloodRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:15 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Flood table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class FloodRecord extends DbRecord
{
    private String lid;

    private double stage;

    private String damage;

    private String dispstmt;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public FloodRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public FloodRecord(FloodRecord origRecord)
    {
        setLid(origRecord.getLid());
        setStage(origRecord.getStage());
        setDamage(origRecord.getDamage());
        setDispstmt(origRecord.getDispstmt());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Flood record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public double getStage()
    {
        return stage;
    }

    public void setStage(double stage)
    {
        this.stage = stage ;
    }

    public String getDamage()
    {
        return damage;
    }

    public void setDamage(String damage)
    {
        this.damage = damage ;
    }

    public String getDispstmt()
    {
        return dispstmt;
    }

    public void setDispstmt(String dispstmt)
    {
        this.dispstmt = dispstmt ;
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
                 + " AND stage = '" + stage + "'" 
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
                getStage() + " " +
                getDamage() + " " +
                getDispstmt() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of FloodRecord class

