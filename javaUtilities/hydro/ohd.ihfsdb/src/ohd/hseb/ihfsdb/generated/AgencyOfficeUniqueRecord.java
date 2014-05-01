// This is a view record !
// filename: AgencyOfficeUniqueRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:18 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              AgencyOfficeUnique table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class AgencyOfficeUniqueRecord extends DbRecord
{
    private String agency_code;

    private String office;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public AgencyOfficeUniqueRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public AgencyOfficeUniqueRecord(AgencyOfficeUniqueRecord origRecord)
    {
        setAgency_code(origRecord.getAgency_code());
        setOffice(origRecord.getOffice());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a AgencyOfficeUnique record

    //-----------------------------------------------------------------
    public String getAgency_code()
    {
        return agency_code;
    }

    public void setAgency_code(String agency_code)
    {
        this.agency_code = agency_code ;
    }

    public String getOffice()
    {
        return office;
    }

    public void setOffice(String office)
    {
        this.office = office ;
    }

//-----------------------------------------------------------------
//  toString() - this method is called with no arguments
//  and returns a String of the internal values
//-----------------------------------------------------------------
    public String toString()
    {
        String outString = 
                getAgency_code() + " " +
                getOffice() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of AgencyOfficeUniqueRecord class

