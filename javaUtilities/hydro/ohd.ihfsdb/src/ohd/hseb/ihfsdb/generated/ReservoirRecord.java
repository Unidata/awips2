// filename: ReservoirRecord.java
// author  : DBGEN
// created : Tue May 31 17:52:17 CDT 2011 using database hd_ob83oax
// description: This class is used to get data from and put data into a
//              Reservoir table record format
//

package ohd.hseb.ihfsdb.generated;

import ohd.hseb.db.*;

public class ReservoirRecord extends DbRecord
{
    private String lid;

    private String name;

    private String type;

    private String owner;

    private double deadpool;

    private double conserpool;

    private double floodpool;

    private double spillway;

    private double sill;

    private double top;

    private double surchg;

    private double elev;

    private int gates;

    private long impounded;

    private String uses;

    private String damids;

    private String damidn;

    //---------------------------------------------------------------
    // Empty constructor
    //---------------------------------------------------------------
    public ReservoirRecord()
    {
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public ReservoirRecord(ReservoirRecord origRecord)
    {
        setLid(origRecord.getLid());
        setName(origRecord.getName());
        setType(origRecord.getType());
        setOwner(origRecord.getOwner());
        setDeadpool(origRecord.getDeadpool());
        setConserpool(origRecord.getConserpool());
        setFloodpool(origRecord.getFloodpool());
        setSpillway(origRecord.getSpillway());
        setSill(origRecord.getSill());
        setTop(origRecord.getTop());
        setSurchg(origRecord.getSurchg());
        setElev(origRecord.getElev());
        setGates(origRecord.getGates());
        setImpounded(origRecord.getImpounded());
        setUses(origRecord.getUses());
        setDamids(origRecord.getDamids());
        setDamidn(origRecord.getDamidn());
    }

    //-----------------------------------------------------------------
    //  get and set methods for all data items in a Reservoir record

    //-----------------------------------------------------------------
    public String getLid()
    {
        return lid;
    }

    public void setLid(String lid)
    {
        this.lid = lid ;
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name ;
    }

    public String getType()
    {
        return type;
    }

    public void setType(String type)
    {
        this.type = type ;
    }

    public String getOwner()
    {
        return owner;
    }

    public void setOwner(String owner)
    {
        this.owner = owner ;
    }

    public double getDeadpool()
    {
        return deadpool;
    }

    public void setDeadpool(double deadpool)
    {
        this.deadpool = deadpool ;
    }

    public double getConserpool()
    {
        return conserpool;
    }

    public void setConserpool(double conserpool)
    {
        this.conserpool = conserpool ;
    }

    public double getFloodpool()
    {
        return floodpool;
    }

    public void setFloodpool(double floodpool)
    {
        this.floodpool = floodpool ;
    }

    public double getSpillway()
    {
        return spillway;
    }

    public void setSpillway(double spillway)
    {
        this.spillway = spillway ;
    }

    public double getSill()
    {
        return sill;
    }

    public void setSill(double sill)
    {
        this.sill = sill ;
    }

    public double getTop()
    {
        return top;
    }

    public void setTop(double top)
    {
        this.top = top ;
    }

    public double getSurchg()
    {
        return surchg;
    }

    public void setSurchg(double surchg)
    {
        this.surchg = surchg ;
    }

    public double getElev()
    {
        return elev;
    }

    public void setElev(double elev)
    {
        this.elev = elev ;
    }

    public int getGates()
    {
        return gates;
    }

    public void setGates(int gates)
    {
        this.gates = gates ;
    }

    public long getImpounded()
    {
        return impounded;
    }

    public void setImpounded(long impounded)
    {
        this.impounded = impounded ;
    }

    public String getUses()
    {
        return uses;
    }

    public void setUses(String uses)
    {
        this.uses = uses ;
    }

    public String getDamids()
    {
        return damids;
    }

    public void setDamids(String damids)
    {
        this.damids = damids ;
    }

    public String getDamidn()
    {
        return damidn;
    }

    public void setDamidn(String damidn)
    {
        this.damidn = damidn ;
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
                getName() + " " +
                getType() + " " +
                getOwner() + " " +
                getDeadpool() + " " +
                getConserpool() + " " +
                getFloodpool() + " " +
                getSpillway() + " " +
                getSill() + " " +
                getTop() + " " +
                getSurchg() + " " +
                getElev() + " " +
                getGates() + " " +
                getDateStringFromLongTime(getImpounded()) + " " +
                getUses() + " " +
                getDamids() + " " +
                getDamidn() + " " +
                "" ;
        return outString;
    } // end toString()
} // end of ReservoirRecord class

