// filename: LineSegsRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              LineSegs table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class LineSegsRecord extends DbRecord
{
    private String area_id;

    private int hrap_row;

    private int hrap_beg_col;

    private int hrap_end_col;

    private double area;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public LineSegsRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public LineSegsRecord(LineSegsRecord origRecord)
    {
        setArea_id(origRecord.getArea_id());
        setHrap_row(origRecord.getHrap_row());
        setHrap_beg_col(origRecord.getHrap_beg_col());
        setHrap_end_col(origRecord.getHrap_end_col());
        setArea(origRecord.getArea());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a LineSegs record

    //-----------------------------------------------------------------
    public String getArea_id()
    {
        return area_id;
    }

    public void setArea_id(String area_id)
    {
        this.area_id = area_id ;
    }

    public int getHrap_row()
    {
        return hrap_row;
    }

    public void setHrap_row(int hrap_row)
    {
        this.hrap_row = hrap_row ;
    }

    public int getHrap_beg_col()
    {
        return hrap_beg_col;
    }

    public void setHrap_beg_col(int hrap_beg_col)
    {
        this.hrap_beg_col = hrap_beg_col ;
    }

    public int getHrap_end_col()
    {
        return hrap_end_col;
    }

    public void setHrap_end_col(int hrap_end_col)
    {
        this.hrap_end_col = hrap_end_col ;
    }

    public double getArea()
    {
        return area;
    }

    public void setArea(double area)
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
                "WHERE area_id = '" + area_id + "'" 
                 + " AND hrap_row = '" + hrap_row + "'" 
                 + " AND hrap_beg_col = '" + hrap_beg_col + "'" 
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
                getArea_id() + " " +
                getHrap_row() + " " +
                getHrap_beg_col() + " " +
                getHrap_end_col() + " " +
                getArea() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of LineSegsRecord class

