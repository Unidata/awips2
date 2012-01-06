// filename: VTECsignifRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              VTECsignif table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class VTECsignifRecord extends DbRecord
{
    private String signif;

    private String name;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public VTECsignifRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public VTECsignifRecord(VTECsignifRecord origRecord)
    {
        setSignif(origRecord.getSignif());
        setName(origRecord.getName());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a VTECsignif record

    //-----------------------------------------------------------------
    public String getSignif()
    {
        return signif;
    }

    public void setSignif(String signif)
    {
        this.signif = signif ;
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
                "WHERE signif = '" + signif + "'" 
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
                getSignif() + " " +
                getName() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of VTECsignifRecord class

