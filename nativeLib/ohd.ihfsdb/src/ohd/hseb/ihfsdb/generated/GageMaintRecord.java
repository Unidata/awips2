// filename: GageMaintRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:15 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              GageMaint table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class GageMaintRecord extends DbRecord
{
    private String maint;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public GageMaintRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public GageMaintRecord(GageMaintRecord origRecord)
    {
        setMaint(origRecord.getMaint());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a GageMaint record

    //-----------------------------------------------------------------
    public String getMaint()
    {
        return maint;
    }

    public void setMaint(String maint)
    {
        this.maint = maint ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE maint = '" + maint + "'" 
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
                getMaint() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of GageMaintRecord class

