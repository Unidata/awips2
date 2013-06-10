// filename: RatingRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Rating table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class RatingRecord extends DbRecord
{
    private String lid;

    private double stage;

    private double discharge;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public RatingRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public RatingRecord(RatingRecord origRecord)
    {
        setLid(origRecord.getLid());
        setStage(origRecord.getStage());
        setDischarge(origRecord.getDischarge());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Rating record

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

    public double getDischarge()
    {
        return discharge;
    }

    public void setDischarge(double discharge)
    {
        this.discharge = discharge ;
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
                getDischarge() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of RatingRecord class

