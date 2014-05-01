// filename: ContactsRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:13 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Contacts table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class ContactsRecord extends DbRecord
{
    private String lid;

    private String contact;

    private String phone;

    private String email;

    private String remark;

    private int priority;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public ContactsRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public ContactsRecord(ContactsRecord origRecord)
    {
        setLid(origRecord.getLid());
        setContact(origRecord.getContact());
        setPhone(origRecord.getPhone());
        setEmail(origRecord.getEmail());
        setRemark(origRecord.getRemark());
        setPriority(origRecord.getPriority());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Contacts record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getContact()
    {
        return contact;
    }

    public void setContact(String contact)
    {
        this.contact = contact ;
    }

    public String getPhone()
    {
        return phone;
    }

    public void setPhone(String phone)
    {
        this.phone = phone ;
    }

    public String getEmail()
    {
        return email;
    }

    public void setEmail(String email)
    {
        this.email = email ;
    }

    public String getRemark()
    {
        return remark;
    }

    public void setRemark(String remark)
    {
        this.remark = remark ;
    }

    public int getPriority()
    {
        return priority;
    }

    public void setPriority(int priority)
    {
        this.priority = priority ;
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
                 + " AND contact = '" + contact + "'" 
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
                getContact() + " " +
                getPhone() + " " +
                getEmail() + " " +
                getRemark() + " " +
                getPriority() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of ContactsRecord class

