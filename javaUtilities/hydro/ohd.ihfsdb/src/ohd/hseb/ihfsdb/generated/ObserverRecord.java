// filename: ObserverRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Observer table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class ObserverRecord extends DbRecord
{
    private String lid;

    private String a1;

    private String a2;

    private String a3;

    private String city;

    private String state;

    private String zip;

    private String comm;

    private long dos;

    private String gn;

    private String hphone;

    private String firstname;

    private String lastname;

    private String phone;

    private String email;

    private String ornr;

    private double rate;

    private String recip;

    private String rprt;

    private String spons;

    private String ssn;

    private String tsk;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public ObserverRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public ObserverRecord(ObserverRecord origRecord)
    {
        setLid(origRecord.getLid());
        setA1(origRecord.getA1());
        setA2(origRecord.getA2());
        setA3(origRecord.getA3());
        setCity(origRecord.getCity());
        setState(origRecord.getState());
        setZip(origRecord.getZip());
        setComm(origRecord.getComm());
        setDos(origRecord.getDos());
        setGn(origRecord.getGn());
        setHphone(origRecord.getHphone());
        setFirstname(origRecord.getFirstname());
        setLastname(origRecord.getLastname());
        setPhone(origRecord.getPhone());
        setEmail(origRecord.getEmail());
        setOrnr(origRecord.getOrnr());
        setRate(origRecord.getRate());
        setRecip(origRecord.getRecip());
        setRprt(origRecord.getRprt());
        setSpons(origRecord.getSpons());
        setSsn(origRecord.getSsn());
        setTsk(origRecord.getTsk());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Observer record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getA1()
    {
        return a1;
    }

    public void setA1(String a1)
    {
        this.a1 = a1 ;
    }

    public String getA2()
    {
        return a2;
    }

    public void setA2(String a2)
    {
        this.a2 = a2 ;
    }

    public String getA3()
    {
        return a3;
    }

    public void setA3(String a3)
    {
        this.a3 = a3 ;
    }

    public String getCity()
    {
        return city;
    }

    public void setCity(String city)
    {
        this.city = city ;
    }

    public String getState()
    {
        return state;
    }

    public void setState(String state)
    {
        this.state = state ;
    }

    public String getZip()
    {
        return zip;
    }

    public void setZip(String zip)
    {
        this.zip = zip ;
    }

    public String getComm()
    {
        return comm;
    }

    public void setComm(String comm)
    {
        this.comm = comm ;
    }

    public long getDos()
    {
        return dos;
    }

    public void setDos(long dos)
    {
        this.dos = dos ;
    }

    public String getGn()
    {
        return gn;
    }

    public void setGn(String gn)
    {
        this.gn = gn ;
    }

    public String getHphone()
    {
        return hphone;
    }

    public void setHphone(String hphone)
    {
        this.hphone = hphone ;
    }

    public String getFirstname()
    {
        return firstname;
    }

    public void setFirstname(String firstname)
    {
        this.firstname = firstname ;
    }

    public String getLastname()
    {
        return lastname;
    }

    public void setLastname(String lastname)
    {
        this.lastname = lastname ;
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

    public String getOrnr()
    {
        return ornr;
    }

    public void setOrnr(String ornr)
    {
        this.ornr = ornr ;
    }

    public double getRate()
    {
        return rate;
    }

    public void setRate(double rate)
    {
        this.rate = rate ;
    }

    public String getRecip()
    {
        return recip;
    }

    public void setRecip(String recip)
    {
        this.recip = recip ;
    }

    public String getRprt()
    {
        return rprt;
    }

    public void setRprt(String rprt)
    {
        this.rprt = rprt ;
    }

    public String getSpons()
    {
        return spons;
    }

    public void setSpons(String spons)
    {
        this.spons = spons ;
    }

    public String getSsn()
    {
        return ssn;
    }

    public void setSsn(String ssn)
    {
        this.ssn = ssn ;
    }

    public String getTsk()
    {
        return tsk;
    }

    public void setTsk(String tsk)
    {
        this.tsk = tsk ;
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
                getA1() + " " +
                getA2() + " " +
                getA3() + " " +
                getCity() + " " +
                getState() + " " +
                getZip() + " " +
                getComm() + " " +
                getDateStringFromLongTime(getDos()) + " " +
                getGn() + " " +
                getHphone() + " " +
                getFirstname() + " " +
                getLastname() + " " +
                getPhone() + " " +
                getEmail() + " " +
                getOrnr() + " " +
                getRate() + " " +
                getRecip() + " " +
                getRprt() + " " +
                getSpons() + " " +
                getSsn() + " " +
                getTsk() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of ObserverRecord class

