// filename: HourlyPCRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:15 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              HourlyPC table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class HourlyPCRecord extends DbRecord
{
    private String lid;

    private String ts;

    private long obsdate;

    private String minute_offset;

    private String hourly_qc;

    private short hour1;

    private short hour2;

    private short hour3;

    private short hour4;

    private short hour5;

    private short hour6;

    private short hour7;

    private short hour8;

    private short hour9;

    private short hour10;

    private short hour11;

    private short hour12;

    private short hour13;

    private short hour14;

    private short hour15;

    private short hour16;

    private short hour17;

    private short hour18;

    private short hour19;

    private short hour20;

    private short hour21;

    private short hour22;

    private short hour23;

    private short hour24;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public HourlyPCRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public HourlyPCRecord(HourlyPCRecord origRecord)
    {
        setLid(origRecord.getLid());
        setTs(origRecord.getTs());
        setObsdate(origRecord.getObsdate());
        setMinute_offset(origRecord.getMinute_offset());
        setHourly_qc(origRecord.getHourly_qc());
        setHour1(origRecord.getHour1());
        setHour2(origRecord.getHour2());
        setHour3(origRecord.getHour3());
        setHour4(origRecord.getHour4());
        setHour5(origRecord.getHour5());
        setHour6(origRecord.getHour6());
        setHour7(origRecord.getHour7());
        setHour8(origRecord.getHour8());
        setHour9(origRecord.getHour9());
        setHour10(origRecord.getHour10());
        setHour11(origRecord.getHour11());
        setHour12(origRecord.getHour12());
        setHour13(origRecord.getHour13());
        setHour14(origRecord.getHour14());
        setHour15(origRecord.getHour15());
        setHour16(origRecord.getHour16());
        setHour17(origRecord.getHour17());
        setHour18(origRecord.getHour18());
        setHour19(origRecord.getHour19());
        setHour20(origRecord.getHour20());
        setHour21(origRecord.getHour21());
        setHour22(origRecord.getHour22());
        setHour23(origRecord.getHour23());
        setHour24(origRecord.getHour24());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a HourlyPC record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getTs()
    {
        return ts;
    }

    public void setTs(String ts)
    {
        this.ts = ts ;
    }

    public long getObsdate()
    {
        return obsdate;
    }

    public void setObsdate(long obsdate)
    {
        this.obsdate = obsdate ;
    }

    public String getMinute_offset()
    {
        return minute_offset;
    }

    public void setMinute_offset(String minute_offset)
    {
        this.minute_offset = minute_offset ;
    }

    public String getHourly_qc()
    {
        return hourly_qc;
    }

    public void setHourly_qc(String hourly_qc)
    {
        this.hourly_qc = hourly_qc ;
    }

    public short getHour1()
    {
        return hour1;
    }

    public void setHour1(short hour1)
    {
        this.hour1 = hour1 ;
    }

    public short getHour2()
    {
        return hour2;
    }

    public void setHour2(short hour2)
    {
        this.hour2 = hour2 ;
    }

    public short getHour3()
    {
        return hour3;
    }

    public void setHour3(short hour3)
    {
        this.hour3 = hour3 ;
    }

    public short getHour4()
    {
        return hour4;
    }

    public void setHour4(short hour4)
    {
        this.hour4 = hour4 ;
    }

    public short getHour5()
    {
        return hour5;
    }

    public void setHour5(short hour5)
    {
        this.hour5 = hour5 ;
    }

    public short getHour6()
    {
        return hour6;
    }

    public void setHour6(short hour6)
    {
        this.hour6 = hour6 ;
    }

    public short getHour7()
    {
        return hour7;
    }

    public void setHour7(short hour7)
    {
        this.hour7 = hour7 ;
    }

    public short getHour8()
    {
        return hour8;
    }

    public void setHour8(short hour8)
    {
        this.hour8 = hour8 ;
    }

    public short getHour9()
    {
        return hour9;
    }

    public void setHour9(short hour9)
    {
        this.hour9 = hour9 ;
    }

    public short getHour10()
    {
        return hour10;
    }

    public void setHour10(short hour10)
    {
        this.hour10 = hour10 ;
    }

    public short getHour11()
    {
        return hour11;
    }

    public void setHour11(short hour11)
    {
        this.hour11 = hour11 ;
    }

    public short getHour12()
    {
        return hour12;
    }

    public void setHour12(short hour12)
    {
        this.hour12 = hour12 ;
    }

    public short getHour13()
    {
        return hour13;
    }

    public void setHour13(short hour13)
    {
        this.hour13 = hour13 ;
    }

    public short getHour14()
    {
        return hour14;
    }

    public void setHour14(short hour14)
    {
        this.hour14 = hour14 ;
    }

    public short getHour15()
    {
        return hour15;
    }

    public void setHour15(short hour15)
    {
        this.hour15 = hour15 ;
    }

    public short getHour16()
    {
        return hour16;
    }

    public void setHour16(short hour16)
    {
        this.hour16 = hour16 ;
    }

    public short getHour17()
    {
        return hour17;
    }

    public void setHour17(short hour17)
    {
        this.hour17 = hour17 ;
    }

    public short getHour18()
    {
        return hour18;
    }

    public void setHour18(short hour18)
    {
        this.hour18 = hour18 ;
    }

    public short getHour19()
    {
        return hour19;
    }

    public void setHour19(short hour19)
    {
        this.hour19 = hour19 ;
    }

    public short getHour20()
    {
        return hour20;
    }

    public void setHour20(short hour20)
    {
        this.hour20 = hour20 ;
    }

    public short getHour21()
    {
        return hour21;
    }

    public void setHour21(short hour21)
    {
        this.hour21 = hour21 ;
    }

    public short getHour22()
    {
        return hour22;
    }

    public void setHour22(short hour22)
    {
        this.hour22 = hour22 ;
    }

    public short getHour23()
    {
        return hour23;
    }

    public void setHour23(short hour23)
    {
        this.hour23 = hour23 ;
    }

    public short getHour24()
    {
        return hour24;
    }

    public void setHour24(short hour24)
    {
        this.hour24 = hour24 ;
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
                 + " AND ts = '" + ts + "'" 
                 + " AND obsdate = '" +  getDateStringFromLongTime(obsdate) + "'" 
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
                getTs() + " " +
                getDateStringFromLongTime(getObsdate()) + " " +
                getMinute_offset() + " " +
                getHourly_qc() + " " +
                getHour1() + " " +
                getHour2() + " " +
                getHour3() + " " +
                getHour4() + " " +
                getHour5() + " " +
                getHour6() + " " +
                getHour7() + " " +
                getHour8() + " " +
                getHour9() + " " +
                getHour10() + " " +
                getHour11() + " " +
                getHour12() + " " +
                getHour13() + " " +
                getHour14() + " " +
                getHour15() + " " +
                getHour16() + " " +
                getHour17() + " " +
                getHour18() + " " +
                getHour19() + " " +
                getHour20() + " " +
                getHour21() + " " +
                getHour22() + " " +
                getHour23() + " " +
                getHour24() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of HourlyPCRecord class

