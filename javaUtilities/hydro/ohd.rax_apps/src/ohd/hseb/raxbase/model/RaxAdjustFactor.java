package ohd.hseb.raxbase.model;

import ohd.hseb.raxbase.util.PEManager;
import ohd.hseb.raxbase.util.TSManager;
import ohd.hseb.raxdb.generated.RaxAdjustFactorRecord;

public class RaxAdjustFactor
{
    private String _lid;
    private String _pe;
    private String _dur;
    private short _idur;
    private String _ts;
    private String _extremum;
    private long _beginDate;
    private double _divisor;
    private double _base;
    private double _multiplier;
    private double _adder;
    
    public RaxAdjustFactor(){}
    
    public RaxAdjustFactor( RaxAdjustFactorRecord record )
    {
        setLid( record.getLid() );
        setPe( record.getPe1() + record.getPe2() );
        setDur( record.getDur() );
        setIdur( record.getIdur() );
        setTs( record.getT() + record.getS() );
        setExtremum( record.getE() );
        setBeginDate( record.getBegin_date() );
        setDivisor( record.getDivisor() );
        setBase( record.getBase() );
        setMultiplier( record.getMultiplier() );
        setAdder( record.getAdder() );
    }
    
    public RaxAdjustFactor( RaxAdjustFactor raxAdjustFactor )
    {
        setLid( raxAdjustFactor.getLid() );
        setPe( raxAdjustFactor.getPe() );
        setDur( raxAdjustFactor.getDur() );
        setIdur( raxAdjustFactor.getIdur() );
        setTs( raxAdjustFactor.getTs() );
        setExtremum( raxAdjustFactor.getExtremum() );
        setBeginDate( raxAdjustFactor.getBeginDate() );
        setDivisor( raxAdjustFactor.getDivisor() );
        setBase( raxAdjustFactor.getBase() );
        setMultiplier( raxAdjustFactor.getMultiplier() );
        setAdder( raxAdjustFactor.getAdder() );
    }
    
    public static RaxAdjustFactor getRaxAdjustFactor( RaxAdjustFactorRecord record )
    {
        return ( new RaxAdjustFactor( record ) );
    }
    
    public static RaxAdjustFactorRecord getRaxAdjustFactorRecord( RaxAdjustFactor raxAdjustFactor )
    {
        RaxAdjustFactorRecord record = new RaxAdjustFactorRecord();
        
        record.setLid( raxAdjustFactor.getLid() );
        record.setPe1( PEManager.getPe1FromPE( raxAdjustFactor.getPe() ) );
        record.setPe2( PEManager.getPe2FromPE( raxAdjustFactor.getPe() ) );
        record.setDur( raxAdjustFactor.getDur() );
        record.setIdur( raxAdjustFactor.getIdur() );
        record.setT( TSManager.getTFromTS( raxAdjustFactor.getTs() ) );
        record.setS( TSManager.getSFromTS( raxAdjustFactor.getTs() ) );
        record.setE( raxAdjustFactor.getExtremum() );
        record.setBegin_date( raxAdjustFactor.getBeginDate() );
        record.setDivisor( raxAdjustFactor.getDivisor() );
        record.setBase( raxAdjustFactor.getBase() );
        record.setMultiplier( raxAdjustFactor.getMultiplier() );
        record.setAdder( raxAdjustFactor.getAdder() );
        
        return record;
    }
    
    public String keyString()
    {
        return "Lid = " + _lid + " | Pe = " + _pe + " | Dur = " + _dur + " | IDur = " + _idur + " | Ts = " + _ts + " | Extremum = " + _extremum;
    }
    
    
    public void setLid( String lid )
    {
        _lid = lid;
    }
    public String getLid()
    {
        return _lid;
    }
    public void setPe( String pe )
    {
        _pe = pe;
    }
    public String getPe()
    {
        return _pe;
    }
    public void setDur( String dur )
    {
        _dur = dur;
    }
    public String getDur()
    {
        return _dur;
    }
    public void setIdur( short idur )
    {
        _idur = idur;
    }
    public short getIdur()
    {
        return _idur;
    }
    public void setTs( String ts )
    {
        _ts = ts;
    }
    public String getTs()
    {
        return _ts;
    }
    public void setExtremum( String extremum )
    {
        _extremum = extremum;
    }
    public String getExtremum()
    {
        return _extremum;
    }
    public void setBeginDate( long beginDate )
    {
        _beginDate = beginDate;
    }
    public long getBeginDate()
    {
        return _beginDate;
    }
    public void setDivisor( double divisor )
    {
        _divisor = divisor;
    }
    public double getDivisor()
    {
        return _divisor;
    }
    public void setBase( double base )
    {
        _base = base;
    }
    public double getBase()
    {
        return _base;
    }
    public void setMultiplier( double multiplier )
    {
        _multiplier = multiplier;
    }
    public double getMultiplier()
    {
        return _multiplier;
    }
    public void setAdder( double adder )
    {
        _adder = adder;
    }
    public double getAdder()
    {
        return _adder;
    }
}
