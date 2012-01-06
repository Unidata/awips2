// filename: LightningRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:15 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Lightning table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class LightningRecord extends DbRecord
{
    private short x_hgrid;

    private short y_hgrid;

    private long obstime;

    private short no_of_strike;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public LightningRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public LightningRecord(LightningRecord origRecord)
    {
        setX_hgrid(origRecord.getX_hgrid());
        setY_hgrid(origRecord.getY_hgrid());
        setObstime(origRecord.getObstime());
        setNo_of_strike(origRecord.getNo_of_strike());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Lightning record

    //-----------------------------------------------------------------
    public short getX_hgrid()
    {
        return x_hgrid;
    }

    public void setX_hgrid(short x_hgrid)
    {
        this.x_hgrid = x_hgrid ;
    }

    public short getY_hgrid()
    {
        return y_hgrid;
    }

    public void setY_hgrid(short y_hgrid)
    {
        this.y_hgrid = y_hgrid ;
    }

    public long getObstime()
    {
        return obstime;
    }

    public void setObstime(long obstime)
    {
        this.obstime = obstime ;
    }

    public short getNo_of_strike()
    {
        return no_of_strike;
    }

    public void setNo_of_strike(short no_of_strike)
    {
        this.no_of_strike = no_of_strike ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE x_hgrid = '" + x_hgrid + "'" 
                 + " AND y_hgrid = '" + y_hgrid + "'" 
                 + " AND obstime = '" +  getDateTimeStringFromLongTime(obstime) + "'" 
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
                getX_hgrid() + " " +
                getY_hgrid() + " " +
                getDateTimeStringFromLongTime(getObstime()) + " " +
                getNo_of_strike() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of LightningRecord class

