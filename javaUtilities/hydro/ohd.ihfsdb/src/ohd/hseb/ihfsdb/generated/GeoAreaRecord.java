// filename: GeoAreaRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:15 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              GeoArea table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class GeoAreaRecord extends DbRecord
{
    private String area_id;

    private String name;

    private String boundary_type;

    private double interior_lat;

    private double interior_lon;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public GeoAreaRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public GeoAreaRecord(GeoAreaRecord origRecord)
    {
        setArea_id(origRecord.getArea_id());
        setName(origRecord.getName());
        setBoundary_type(origRecord.getBoundary_type());
        setInterior_lat(origRecord.getInterior_lat());
        setInterior_lon(origRecord.getInterior_lon());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a GeoArea record

    //-----------------------------------------------------------------
    public String getArea_id()
    {
        return area_id;
    }

    public void setArea_id(String area_id)
    {
        this.area_id = area_id ;
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name ;
    }

    public String getBoundary_type()
    {
        return boundary_type;
    }

    public void setBoundary_type(String boundary_type)
    {
        this.boundary_type = boundary_type ;
    }

    public double getInterior_lat()
    {
        return interior_lat;
    }

    public void setInterior_lat(double interior_lat)
    {
        this.interior_lat = interior_lat ;
    }

    public double getInterior_lon()
    {
        return interior_lon;
    }

    public void setInterior_lon(double interior_lon)
    {
        this.interior_lon = interior_lon ;
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
                getName() + " " +
                getBoundary_type() + " " +
                getInterior_lat() + " " +
                getInterior_lon() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of GeoAreaRecord class

