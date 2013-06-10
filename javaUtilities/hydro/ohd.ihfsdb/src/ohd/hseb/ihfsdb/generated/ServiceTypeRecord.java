// filename: ServiceTypeRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              ServiceType table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class ServiceTypeRecord extends DbRecord
{
    private String service_type;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public ServiceTypeRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public ServiceTypeRecord(ServiceTypeRecord origRecord)
    {
        setService_type(origRecord.getService_type());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a ServiceType record

    //-----------------------------------------------------------------
    public String getService_type()
    {
        return service_type;
    }

    public void setService_type(String service_type)
    {
        this.service_type = service_type ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE service_type = '" + service_type + "'" 
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
                getService_type() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of ServiceTypeRecord class

