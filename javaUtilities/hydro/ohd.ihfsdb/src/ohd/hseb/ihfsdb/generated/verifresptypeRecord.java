// filename: verifresptypeRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              verifresptype table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class verifresptypeRecord extends DbRecord
{
    private String verif_resp_type;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public verifresptypeRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public verifresptypeRecord(verifresptypeRecord origRecord)
    {
        setVerif_resp_type(origRecord.getVerif_resp_type());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a verifresptype record

    //-----------------------------------------------------------------
    public String getVerif_resp_type()
    {
        return verif_resp_type;
    }

    public void setVerif_resp_type(String verif_resp_type)
    {
        this.verif_resp_type = verif_resp_type ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE verif_resp_type = '" + verif_resp_type + "'" 
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
                getVerif_resp_type() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of verifresptypeRecord class

