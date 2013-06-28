// filename: VTECphenomRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              VTECphenom table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class VTECphenomRecord extends DbRecord
{
    private String phenom;

    private String name;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public VTECphenomRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public VTECphenomRecord(VTECphenomRecord origRecord)
    {
        setPhenom(origRecord.getPhenom());
        setName(origRecord.getName());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a VTECphenom record

    //-----------------------------------------------------------------
    public String getPhenom()
    {
        return phenom;
    }

    public void setPhenom(String phenom)
    {
        this.phenom = phenom ;
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE phenom = '" + phenom + "'" 
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
                getPhenom() + " " +
                getName() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of VTECphenomRecord class

