// filename: LocAreaRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              LocArea table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class LocAreaRecord extends DbRecord
{
    private String lid;

    private String area;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public LocAreaRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public LocAreaRecord(LocAreaRecord origRecord)
    {
        setLid(origRecord.getLid());
        setArea(origRecord.getArea());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a LocArea record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getArea()
    {
        return area;
    }

    public void setArea(String area)
    {
        this.area = area ;
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
                getArea() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of LocAreaRecord class

