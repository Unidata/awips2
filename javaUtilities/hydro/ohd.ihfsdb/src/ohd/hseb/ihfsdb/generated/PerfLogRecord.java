// filename: PerfLogRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:16 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              PerfLog table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class PerfLogRecord extends DbRecord
{
    private String process;

    private long start_time;

    private int num_processed;

    private int num_reads;

    private int num_inserts;

    private int num_updates;

    private int num_deletes;

    private double elapsed_time;

    private double cpu_time;

    private double io_time;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public PerfLogRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public PerfLogRecord(PerfLogRecord origRecord)
    {
        setProcess(origRecord.getProcess());
        setStart_time(origRecord.getStart_time());
        setNum_processed(origRecord.getNum_processed());
        setNum_reads(origRecord.getNum_reads());
        setNum_inserts(origRecord.getNum_inserts());
        setNum_updates(origRecord.getNum_updates());
        setNum_deletes(origRecord.getNum_deletes());
        setElapsed_time(origRecord.getElapsed_time());
        setCpu_time(origRecord.getCpu_time());
        setIo_time(origRecord.getIo_time());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a PerfLog record

    //-----------------------------------------------------------------
    public String getProcess()
    {
        return process;
    }

    public void setProcess(String process)
    {
        this.process = process ;
    }

    public long getStart_time()
    {
        return start_time;
    }

    public void setStart_time(long start_time)
    {
        this.start_time = start_time ;
    }

    public int getNum_processed()
    {
        return num_processed;
    }

    public void setNum_processed(int num_processed)
    {
        this.num_processed = num_processed ;
    }

    public int getNum_reads()
    {
        return num_reads;
    }

    public void setNum_reads(int num_reads)
    {
        this.num_reads = num_reads ;
    }

    public int getNum_inserts()
    {
        return num_inserts;
    }

    public void setNum_inserts(int num_inserts)
    {
        this.num_inserts = num_inserts ;
    }

    public int getNum_updates()
    {
        return num_updates;
    }

    public void setNum_updates(int num_updates)
    {
        this.num_updates = num_updates ;
    }

    public int getNum_deletes()
    {
        return num_deletes;
    }

    public void setNum_deletes(int num_deletes)
    {
        this.num_deletes = num_deletes ;
    }

    public double getElapsed_time()
    {
        return elapsed_time;
    }

    public void setElapsed_time(double elapsed_time)
    {
        this.elapsed_time = elapsed_time ;
    }

    public double getCpu_time()
    {
        return cpu_time;
    }

    public void setCpu_time(double cpu_time)
    {
        this.cpu_time = cpu_time ;
    }

    public double getIo_time()
    {
        return io_time;
    }

    public void setIo_time(double io_time)
    {
        this.io_time = io_time ;
    }

//-----------------------------------------------------------------
//  getWhereString() - this method is called with no arguments
//  and returns a String that contains a valid where clause containing all the
//  primary key fields.
//-----------------------------------------------------------------
    public String getWhereString()
    {
        String outString = 
                "WHERE process = '" + process + "'" 
                 + " AND start_time = '" +  getDateTimeStringFromLongTime(start_time) + "'" 
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
                getProcess() + " " +
                getDateTimeStringFromLongTime(getStart_time()) + " " +
                getNum_processed() + " " +
                getNum_reads() + " " +
                getNum_inserts() + " " +
                getNum_updates() + " " +
                getNum_deletes() + " " +
                getElapsed_time() + " " +
                getCpu_time() + " " +
                getIo_time() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of PerfLogRecord class

