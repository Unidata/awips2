package ohd.hseb.raxbase.model;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.raxbase.util.PEManager;
import ohd.hseb.raxbase.util.TSManager;
import ohd.hseb.raxdb.generated.SensokRecord;

public class Sensok
{
    private static final short MISSING = -9999;
    
    private String _lid = null;
    private String _pe = null;
    private String _dur = null;
    private short _idur = MISSING;
    private String _ts = null;
    private String _extremum = null;
    private String _p = null;
    private long _oktime = MISSING;
    private String _ok = null;
    private String _init = null;
    private String _reason = null;
    private String _agcode = null;
    private String _agloc = null;
    private String _comment = null;

    public Sensok(){}
    
    public Sensok( Sensok sensok )
    {
        setLid( sensok.getLid() );
        setPe( sensok.getPe() );
        setDur( sensok.getDur() );
        setIdur( sensok.getIdur() );
        setTs( sensok.getTs() );
        setExtremum( sensok.getExtremum() );
        setP( sensok.getP() );
        setOktime( sensok.getOktime() );
        setOk( sensok.getOk() );
        setInit( sensok.getInit() );
        setReason( sensok.getReason() );
        setAgcode( sensok.getAgcode() );
        setAgloc( sensok.getAgloc() );
        setComment( sensok.getComment() );
    }
    
    public Sensok( SensokRecord sensokRecord )
    {
        setLid( sensokRecord.getLid() );
        setPe( PEManager.getPEFromPe1Pe2( sensokRecord.getPe1(), sensokRecord.getPe2() ) );
        setDur( sensokRecord.getDur() );
        setIdur( sensokRecord.getIdur() );
        setTs( TSManager.getTSFromTandS( sensokRecord.getT(), sensokRecord.getS() ) );
        setExtremum( sensokRecord.getE() );
        setP( sensokRecord.getP() );
        setOktime( sensokRecord.getOktime() );
        setOk( sensokRecord.getOk() );
        setInit( sensokRecord.getInit() );
        setReason( sensokRecord.getReason() );
        setAgcode( sensokRecord.getAgcode() );
        setAgloc( sensokRecord.getAgloc() );
        setComment( sensokRecord.getComment() );
    }
    
    public static Sensok getSensok( SensokRecord sensokRecord )
    {
        return ( new Sensok( sensokRecord ) );
    }
    
    public static SensokRecord getSensokRecord( Sensok sensok )
    {
        SensokRecord sensokRecord = new SensokRecord();
        
        sensokRecord.setLid( sensok.getLid() );
        sensokRecord.setPe1( PEManager.getPe1FromPE( sensok.getPe() ) );
        sensokRecord.setPe2( PEManager.getPe2FromPE( sensok.getPe() ) );
        sensokRecord.setDur( sensok.getDur() );
        sensokRecord.setIdur( sensok.getIdur() );
        sensokRecord.setT( TSManager.getTFromTS( sensok.getTs() ) );
        sensokRecord.setS( TSManager.getSFromTS( sensok.getTs() ) );
        sensokRecord.setE( sensok.getExtremum() );
        sensokRecord.setP( sensok.getP() );
        sensokRecord.setOktime( sensok.getOktime() );
        sensokRecord.setOk( sensok.getOk() );
        sensokRecord.setInit( sensok.getInit() );
        sensokRecord.setReason( sensok.getReason() );
        sensokRecord.setAgcode( sensok.getAgcode() );
        sensokRecord.setAgloc( sensok.getAgloc() );
        sensokRecord.setComment( sensok.getComment() );

        return sensokRecord;
    }

    public String keyString()
    {
        String keyString = "LID = " + getKeyValue( _lid ) + " | PE = " + getKeyValue( _pe ) + " | Dur = " + getKeyValue( _dur ) + 
                           " | IDur = " + getKeyValue( _idur ) + " | TS = " + getKeyValue( _ts ) + " | Extremum = " + getKeyValue( _extremum ) + 
                           " | P = " + getKeyValue( _p ) + " | OKTime: " + DbTimeHelper.getDateStringFromLongTime( _oktime );
        
        return keyString;
    }

    private String getKeyValue( String str )
    {
        String returnValue = "NULL";
        
        if ( str != null )
        {
            returnValue = str;
        }
        
        return returnValue;
    }
    
    private String getKeyValue( short number )
    {
        return ( getKeyValue( number + "" ) );  
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

    public void setP( String p )
    {
        _p = p;
    }

    public String getP()
    {
        return _p;
    }

    public void setOktime( long oktime )
    {
        _oktime = oktime;
    }

    public long getOktime()
    {
        return _oktime;
    }

    public void setOk( String ok )
    {
        _ok = ok;
    }

    public String getOk()
    {
        return _ok;
    }

    public void setInit( String init )
    {
        _init = init;
    }

    public String getInit()
    {
        return _init;
    }

    public void setReason( String reason )
    {
        _reason = reason;
    }

    public String getReason()
    {
        return _reason;
    }

    public void setAgcode( String agcode )
    {
        _agcode = agcode;
    }

    public String getAgcode()
    {
        return _agcode;
    }

    public void setAgloc( String agloc )
    {
        _agloc = agloc;
    }

    public String getAgloc()
    {
        return _agloc;
    }

    public void setComment( String comment )
    {
        _comment = comment;
    }

    public String getComment()
    {
        return _comment;
    }
    
}
