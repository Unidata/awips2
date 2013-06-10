// filename: LocExtAgencyRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              LocExtAgency table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class LocExtAgencyRecord extends DbRecord
{
    private String lid;

    private String agency_code;

    private String office;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public LocExtAgencyRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public LocExtAgencyRecord(LocExtAgencyRecord origRecord)
    {
        setLid(origRecord.getLid());
        setAgency_code(origRecord.getAgency_code());
        setOffice(origRecord.getOffice());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a LocExtAgency record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

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
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE lid = '" + lid + "'" 
                 + " AND agency_code = '" + agency_code + "'" 
                 + " AND office = '" + office + "'" 
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
                getAgency_code() + " " +
                getOffice() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of LocExtAgencyRecord class

