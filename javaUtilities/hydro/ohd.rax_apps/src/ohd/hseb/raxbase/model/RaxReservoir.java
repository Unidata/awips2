package ohd.hseb.raxbase.model;

import ohd.hseb.raxdb.generated.RaxReservoirRecord;
import ohd.hseb.util.StringDataConverter;

public class RaxReservoir
{
    private String _lid;
    private long _beginDate;
    private long _endDate;
    private String _name;
    private String _type;
    private String _owner;
    private double _deadPool;
    private double _conserPool;
    private double _floodPool;
    private double _spillWay;
    private double _sill;
    private double _top;
    private double _surchg;
    private double _elev;
    private int _gates;
    private long _impounded;
    private String _uses;
    
    
    public RaxReservoir(){}
    
    public RaxReservoir( RaxReservoir raxReservoir )
    {
        setLid( raxReservoir.getLid() );
        setBeginDate( raxReservoir.getBeginDate() );
        setEndDate( raxReservoir.getEndDate() );
        setName( raxReservoir.getName() );
        setType( raxReservoir.getType() );
        setOwner( raxReservoir.getOwner() );
        setDeadPool( raxReservoir.getDeadPool() );
        setConserPool( raxReservoir.getConserPool() );
        setFloodPool( raxReservoir.getFloodPool() );
        setSpillWay( raxReservoir.getSpillWay() );
        setSill( raxReservoir.getSill() );
        setTop( raxReservoir.getTop() );
        setSurchg( raxReservoir.getSurchg() );
        setElev( raxReservoir.getElev() );
        setGates( raxReservoir.getGates() );
        setImpounded( raxReservoir.getImpounded() );
        setUses( raxReservoir.getUses() );
    }
    
    public RaxReservoir( RaxReservoirRecord raxReservoirRecord )
    {
        setLid( raxReservoirRecord.getLid() );
        setBeginDate( raxReservoirRecord.getSbd() );
        setEndDate( raxReservoirRecord.getSed() );
        setName( raxReservoirRecord.getName() );
        setType( raxReservoirRecord.getType() );
        setOwner( raxReservoirRecord.getOwner() );
        setDeadPool( raxReservoirRecord.getDeadpool() );
        setConserPool( raxReservoirRecord.getConserpool() );
        setFloodPool( raxReservoirRecord.getFloodpool() );
        setSpillWay( raxReservoirRecord.getSpillway() );
        setSill( raxReservoirRecord.getSill() );
        setTop( raxReservoirRecord.getTop() );
        setSurchg( raxReservoirRecord.getSurchg() );
        setElev( raxReservoirRecord.getElev() );
        setGates( raxReservoirRecord.getGates() );
        setImpounded( raxReservoirRecord.getImpounded() );
        setUses( raxReservoirRecord.getUses() );
    }
    
    public String toString()
    {
        StringDataConverter converter = new StringDataConverter();
        
        return "Lid = " + _lid + " | BeginDate = " + converter.getDateStringFromDateLong( _beginDate ) + " | EndDate = " + converter.getDateStringFromDateLong( _endDate ) + "\n" +
               " | Name = " + _name + "\n" + " | Type = " + _type + " | Owner = " + _owner + "\n" +
               " | DeadPool = " + _deadPool + " | ConserPool = " + _conserPool + " | FloodPool = " + _floodPool + "\n" +
               " | SpillWay = " + _spillWay + " | Sill = " + _sill + " | Top = " + _top + "\n" +
               " | Surchg = " + _surchg + " | Elev = " + _elev + " | Gates = " + _gates + "\n" +
               " | Impounded = " + _impounded + " | Uses = " + _uses;
    }
    
    public String keyString()
    {
        StringDataConverter converter = new StringDataConverter();

        return "Lid = " + _lid + " | BeginDate = " + converter.getDateStringFromDateLong( _beginDate );
    }
    
    public static RaxReservoir getRaxReservoir( RaxReservoirRecord raxReservoirRecord )
    {
        return ( new RaxReservoir( raxReservoirRecord ) );
    }
    
    public static RaxReservoirRecord getRaxReservoirRecord( RaxReservoir raxReservoir )
    {
        RaxReservoirRecord raxReservoirRecord = new RaxReservoirRecord();
        
        raxReservoirRecord.setLid( raxReservoir.getLid() );
        raxReservoirRecord.setSbd( raxReservoir.getBeginDate() );
        raxReservoirRecord.setSed( raxReservoir.getEndDate() );
        raxReservoirRecord.setName( raxReservoir.getName() );
        raxReservoirRecord.setType( raxReservoir.getType() );
        raxReservoirRecord.setOwner( raxReservoir.getOwner() );
        raxReservoirRecord.setDeadpool( raxReservoir.getDeadPool() );
        raxReservoirRecord.setConserpool( raxReservoir.getConserPool() );
        raxReservoirRecord.setFloodpool( raxReservoir.getFloodPool() );
        raxReservoirRecord.setSpillway( raxReservoir.getSpillWay() );
        raxReservoirRecord.setSill( raxReservoir.getSill() );
        raxReservoirRecord.setTop( raxReservoir.getTop() );
        raxReservoirRecord.setSurchg( raxReservoir.getSurchg() );
        raxReservoirRecord.setElev( raxReservoir.getElev() );
        raxReservoirRecord.setGates( raxReservoir.getGates() );
        raxReservoirRecord.setImpounded( raxReservoir.getImpounded() );
        raxReservoirRecord.setUses( raxReservoir.getUses() );

        return raxReservoirRecord;
    }

    public void setLid( String lid )
    {
        _lid = lid;
    }
    public String getLid()
    {
        return _lid;
    }
    public void setBeginDate( long beginDate )
    {
        _beginDate = beginDate;
    }
    public long getBeginDate()
    {
        return _beginDate;
    }
    public void setEndDate( long endDate )
    {
        _endDate = endDate;
    }
    public long getEndDate()
    {
        return _endDate;
    }
    public void setName( String name )
    {
        _name = name;
    }
    public String getName()
    {
        return _name;
    }
    public void setType( String type )
    {
        _type = type;
    }
    public String getType()
    {
        return _type;
    }
    public void setOwner( String owner )
    {
        _owner = owner;
    }
    public String getOwner()
    {
        return _owner;
    }
    public void setDeadPool( double deadPool )
    {
        _deadPool = deadPool;
    }
    public double getDeadPool()
    {
        return _deadPool;
    }
    public void setConserPool( double conserPool )
    {
        _conserPool = conserPool;
    }
    public double getConserPool()
    {
        return _conserPool;
    }
    public void setFloodPool( double floodPool )
    {
        _floodPool = floodPool;
    }
    public double getFloodPool()
    {
        return _floodPool;
    }
    public void setSpillWay( double spillWay )
    {
        _spillWay = spillWay;
    }
    public double getSpillWay()
    {
        return _spillWay;
    }
    public void setSill( double sill )
    {
        _sill = sill;
    }
    public double getSill()
    {
        return _sill;
    }
    public void setTop( double top )
    {
        _top = top;
    }
    public double getTop()
    {
        return _top;
    }
    public void setSurchg( double surchg )
    {
        _surchg = surchg;
    }
    public double getSurchg()
    {
        return _surchg;
    }
    public void setElev( double elev )
    {
        _elev = elev;
    }
    public double getElev()
    {
        return _elev;
    }
    public void setGates( int gates )
    {
        _gates = gates;
    }
    public int getGates()
    {
        return _gates;
    }
    public void setImpounded( long impounded )
    {
        _impounded = impounded;
    }
    public long getImpounded()
    {
        return _impounded;
    }
    public void setUses( String uses )
    {
        _uses = uses;
    }
    public String getUses()
    {
        return _uses;
    }

}
