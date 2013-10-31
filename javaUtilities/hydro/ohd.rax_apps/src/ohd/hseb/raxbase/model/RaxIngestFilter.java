package ohd.hseb.raxbase.model;

import ohd.hseb.raxbase.util.PEManager;
import ohd.hseb.raxbase.util.TSManager;
import ohd.hseb.raxdb.generated.RaxIngestFilterRecord;

public class RaxIngestFilter 
{
    private static final int MISSING = -9999;
    
    private String _lid = null;
    private String _pe = null;
    private String _dur = null;
    private short _idur = MISSING;
    private String _ts = null;
    private String _extremum = null;
    private short _tsRank = MISSING;
    private String _detailInfo = null;
    private boolean _ingest = false;
    private boolean _newReport = false;
    private boolean _active = true;
    private boolean _ofsInput = false;
    private long    _obsTime = 0;
    private String _ownerAgency = null;
    private String _ownerLocation = null;
    private boolean _mpeInput = false;
    
    public RaxIngestFilter(){}
    
    public RaxIngestFilter( RaxIngestFilter raxIngestFilter )
    {
        setLid( raxIngestFilter.getLid() );
        setPe( raxIngestFilter.getPe() );
        setDur( raxIngestFilter.getDur() );
        setIdur( raxIngestFilter.getIdur() );
        setTs( raxIngestFilter.getTs() );
        setExtremum( raxIngestFilter.getExtremum() );
        setTsRank( raxIngestFilter.getTsRank() );
        setDetailInfo( raxIngestFilter.getDetailInfo() );
        setIngest( raxIngestFilter.isIngest() );
        setNewReport( raxIngestFilter.isNewReport() );
        setActive( raxIngestFilter.isActive() );
        setOfsInput( raxIngestFilter.isOfsInput() );
        setObsTime( raxIngestFilter.getObsTime() );
        setOwnerAgency( raxIngestFilter.getOwnerAgency() );
        setOwnerLocation( raxIngestFilter.getOwnerLocation() );
        setMpeInput( raxIngestFilter.isMpeInput() );
    }

    public String toString()
    {
        return "Lid = " + _lid + " | PE = " + _pe + " | Dur = " + _dur + " | IDur = " + _idur + " | TS = " + _ts + " | Extremum = " + _extremum +
               " | TsRank = " + _tsRank + " | DetailInfo = " + _detailInfo + " | Ingest = " + _ingest + " | NewReport = " + _newReport +
               " | Active = " + _active + " | OfsInput = " + _ofsInput + " | ObsTime = " + _obsTime + " | OwnerAgency = " + _ownerAgency +
               " | OwnerLocation = " + _ownerLocation + " | MpeInput = " + _mpeInput;
    }
    
    public String keyString()
    {
        return "Lid = " + _lid + " | PE = " + _pe + " | Dur = " + _dur + " | IDur = " + _idur + " | TS = " + _ts + " | Extremum = " + _extremum;
    }

    public RaxIngestFilter( RaxIngestFilterRecord raxIngestFilter )
    {
        setLid( raxIngestFilter.getLid() );
        setPe( PEManager.getPEFromPe1Pe2( raxIngestFilter.getPe1(), raxIngestFilter.getPe2() ) );
        setDur( raxIngestFilter.getDur() );
        setIdur( raxIngestFilter.getIdur() );
        setTs( TSManager.getTSFromTandS( raxIngestFilter.getT(), raxIngestFilter.getS() ) );
        setExtremum( raxIngestFilter.getE() );
        setTsRank( raxIngestFilter.getTs_rank() );
        setDetailInfo( raxIngestFilter.getDet() );
        
        if ( raxIngestFilter.getIngest() == 0 )
        {
            setIngest( false );
        }
        else
        {
            setIngest( true );
        }
        
        if ( ( raxIngestFilter.getNew_report() == null ) ||
             ( raxIngestFilter.getNew_report().equalsIgnoreCase( "N" ) ) )
            
        {
            setNewReport( false );
        }
        else
        {
            setNewReport( true );
        }

        if ( ( raxIngestFilter.getActive() == null ) || 
             ( raxIngestFilter.getActive().equalsIgnoreCase( "N" ) ) )
        {
            setActive( false );
        }
        else
        {
            setActive( true );
        }
        
        if ( ( raxIngestFilter.getOfs_input() == null ) ||
             ( raxIngestFilter.getOfs_input().equalsIgnoreCase( "0" ) ) ||
             ( raxIngestFilter.getOfs_input().equalsIgnoreCase( "F" ) ) )

        {
            setOfsInput( false );
        }
        else
        {
            setOfsInput( true );
        }

        setObsTime( raxIngestFilter.getObstime() );
        setOwnerAgency( raxIngestFilter.getOwnag() );
        setOwnerLocation( raxIngestFilter.getOwnloc() );

        if ( ( raxIngestFilter.getMpe_input() == null ) ||
                ( raxIngestFilter.getMpe_input().equalsIgnoreCase( "0" ) ) ||
                ( raxIngestFilter.getMpe_input().equalsIgnoreCase( "F" ) ) )
        {
            setMpeInput( false );
        }
        else
        {
            setMpeInput( true );
        }
    }
    
    public static RaxIngestFilter getRaxIngestFilter( RaxIngestFilterRecord raxIngestFilterRecord )
    {
        return ( new RaxIngestFilter( raxIngestFilterRecord ) );
    }

    public static RaxIngestFilterRecord getRaxIngestFilterRecord( RaxIngestFilter raxIngestFilter )
    {
        RaxIngestFilterRecord raxIngestFilterRecord = new RaxIngestFilterRecord();
        
        raxIngestFilterRecord.setLid( raxIngestFilter.getLid() );
        raxIngestFilterRecord.setPe1( PEManager.getPe1FromPE( raxIngestFilter.getPe() ) );
        raxIngestFilterRecord.setPe2( PEManager.getPe2FromPE( raxIngestFilter.getPe() ) );
        raxIngestFilterRecord.setDur( raxIngestFilter.getDur() );
        raxIngestFilterRecord.setIdur( raxIngestFilter.getIdur() );
        raxIngestFilterRecord.setT( TSManager.getTFromTS( raxIngestFilter.getTs() ) );
        raxIngestFilterRecord.setS( TSManager.getSFromTS( raxIngestFilter.getTs() ) );
        raxIngestFilterRecord.setE( raxIngestFilter.getExtremum() );
        raxIngestFilterRecord.setTs_rank( raxIngestFilter.getTsRank() );
        raxIngestFilterRecord.setDet( raxIngestFilter.getDetailInfo() );
        
        if ( raxIngestFilter.isIngest() )
        {
            raxIngestFilterRecord.setIngest( (short) 1 );
        }
        else
        {
            raxIngestFilterRecord.setIngest( (short) 0 );
        }
        
        if ( raxIngestFilter.isNewReport() )
        {
            raxIngestFilterRecord.setNew_report( "Y" );
        }
        else
        {
            raxIngestFilterRecord.setNew_report( "N" );
        }
        
        if ( raxIngestFilter.isActive() )
        {
            raxIngestFilterRecord.setActive( "Y" );
        }
        else
        {
            raxIngestFilterRecord.setActive( "N" );
        }
        
        if ( raxIngestFilter.isOfsInput() )
        {
            raxIngestFilterRecord.setOfs_input( "1" );
        }
        else
        {
            raxIngestFilterRecord.setOfs_input( "0" );
        }

        raxIngestFilterRecord.setObstime( raxIngestFilter.getObsTime() );
        raxIngestFilterRecord.setOwnag( raxIngestFilter.getOwnerAgency() );
        raxIngestFilterRecord.setOwnloc( raxIngestFilter.getOwnerLocation() );
        
        if ( raxIngestFilter.isMpeInput() )
        {
            raxIngestFilterRecord.setMpe_input( "1" );
        }
        else
        {
            raxIngestFilterRecord.setMpe_input( "0" );
        }
        
        return raxIngestFilterRecord;
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

    public void setTsRank( short tsRank )
    {
        _tsRank = tsRank;
    }

    public short getTsRank()
    {
        return _tsRank;
    }

    public void setDetailInfo( String detailInfo )
    {
        _detailInfo = detailInfo;
    }

    public String getDetailInfo()
    {
        return _detailInfo;
    }

    public void setIngest( boolean ingest )
    {
        _ingest = ingest;
    }

    public boolean isIngest()
    {
        return _ingest;
    }

    public void setNewReport( boolean newReport )
    {
        _newReport = newReport;
    }

    public boolean isNewReport()
    {
        return _newReport;
    }

    public void setActive( boolean active )
    {
        _active = active;
    }

    public boolean isActive()
    {
        return _active;
    }

    public void setOfsInput( boolean ofsInput )
    {
        _ofsInput = ofsInput;
    }

    public boolean isOfsInput()
    {
        return _ofsInput;
    }

    public void setObsTime( long obsTime )
    {
        _obsTime = obsTime;
    }

    public long getObsTime()
    {
        return _obsTime;
    }

    public void setOwnerAgency( String ownerAgency )
    {
        _ownerAgency = ownerAgency;
    }

    public String getOwnerAgency()
    {
        return _ownerAgency;
    }

    public void setOwnerLocation( String ownerLocation )
    {
        _ownerLocation = ownerLocation;
    }

    public String getOwnerLocation()
    {
        return _ownerLocation;
    }

    public void setMpeInput( boolean mpeInput )
    {
        _mpeInput = mpeInput;
    }

    public boolean isMpeInput()
    {
        return _mpeInput;
    }
    
    public String getPrimaryKey()
    {
        return ( _lid + "|" + _pe + "|" + _dur + "|" + _ts + "|" + _extremum );
    }
}
