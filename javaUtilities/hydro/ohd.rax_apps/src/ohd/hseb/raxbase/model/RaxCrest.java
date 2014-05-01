package ohd.hseb.raxbase.model;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.raxdb.generated.RaxCrestRecord;

public class RaxCrest
{
    private static final short MISSING = -9999;
    
    private String _lid = null;
    private long _dateCrest = MISSING;
    private String _crestDateTime = null;
    private double _stage = MISSING;
    private String _stgQual = null;
    private double _flow = MISSING;
    private String _flowQual = null;
    private String _hw = null;
    private String _jam = null;
    private String _oldDatum = null;
    private String _prelim = null;

    public RaxCrest(){}
    
    public RaxCrest( RaxCrest raxCrest )
    {
        setLid( raxCrest.getLid() );
        setDateCrest( raxCrest.getDateCrest() );
        setCrestDateTime( raxCrest.getCrestDateTime() );
        setStage( raxCrest.getStage() );
        setStgQual( raxCrest.getStgQual() );
        setFlow( raxCrest.getFlow() );
        setFlowQual( raxCrest.getFlowQual() );
        setHw( raxCrest.getHw() );
        setJam( raxCrest.getJam() );
        setOldDatum( raxCrest.getOldDatum() );
        setPrelim( raxCrest.getPrelim() ); 
    }
    
    public RaxCrest( RaxCrestRecord raxCrestRecord )
    {
        setLid( raxCrestRecord.getLid() );
        setDateCrest( raxCrestRecord.getDatecrst() );
        setCrestDateTime( raxCrestRecord.getCrstdatetime() );
        setStage( raxCrestRecord.getStage() );
        setStgQual( raxCrestRecord.getStg_qual() );
        setFlow( raxCrestRecord.getFlow() );
        setFlowQual( raxCrestRecord.getFlow_qual() );
        setHw( raxCrestRecord.getHw() );
        setJam( raxCrestRecord.getJam() );
        setOldDatum( raxCrestRecord.getOlddatum() );
        setPrelim( raxCrestRecord.getPrelim() ); 
    }
    
    public static RaxCrest getRaxCrest( RaxCrestRecord record )
    {
        return ( new RaxCrest( record ) );
    }
    
    public static RaxCrestRecord getRaxCrestRecord( RaxCrest raxCrest )
    {
        RaxCrestRecord raxCrestRecord = new RaxCrestRecord();
        
        raxCrestRecord.setLid( raxCrest.getLid() );
        raxCrestRecord.setDatecrst( raxCrest.getDateCrest() );
        raxCrestRecord.setCrstdatetime( raxCrest.getCrestDateTime() );
        raxCrestRecord.setStage( raxCrest.getStage() );
        raxCrestRecord.setStg_qual( raxCrest.getStgQual() );
        raxCrestRecord.setFlow( raxCrest.getFlow() );
        raxCrestRecord.setFlow_qual( raxCrest.getFlowQual() );
        raxCrestRecord.setHw( raxCrest.getHw() );
        raxCrestRecord.setJam( raxCrest.getJam() );
        raxCrestRecord.setOlddatum( raxCrest.getOldDatum() );
        raxCrestRecord.setPrelim( raxCrest.getPrelim() ); 

        return raxCrestRecord;
    }
    
    public String toString()
    {
        return "Lid = " + _lid + " | Datecrst = " + _dateCrest + " | Crstdatetime = " + _crestDateTime +
               " | Stage = " + _stage + " | Stg_qual = " + _stgQual + " | Flow = " + _flow + 
               " | Flow_qual = " + _flowQual + " | Hw = " + _hw + " | Jam = " + _jam + 
               " | Olddatum = " + _oldDatum + " | Prelim = " + _prelim;
    }
    
    public String keyString()
    {
        return "Lid = " + _lid + " | Datecrst = " + DbTimeHelper.getDateStringFromLongTime( _dateCrest );
    }
    
    public void setLid( String lid )
    {
        _lid = lid;
    }
    public String getLid()
    {
        return _lid;
    }
    public void setDateCrest( long dateCrest )
    {
        _dateCrest = dateCrest;
    }
    public long getDateCrest()
    {
        return _dateCrest;
    }
    public void setCrestDateTime( String crestDateTime )
    {
        _crestDateTime = crestDateTime;
    }
    public String getCrestDateTime()
    {
        return _crestDateTime;
    }
    public void setStage( double stage )
    {
        _stage = stage;
    }
    public double getStage()
    {
        return _stage;
    }
    public void setStgQual( String stgQual )
    {
        _stgQual = stgQual;
    }
    public String getStgQual()
    {
        return _stgQual;
    }
    public void setFlow( double flow )
    {
        _flow = flow;
    }
    public double getFlow()
    {
        return _flow;
    }
    public void setFlowQual( String flowQual )
    {
        _flowQual = flowQual;
    }
    public String getFlowQual()
    {
        return _flowQual;
    }
    public void setHw( String hw )
    {
        _hw = hw;
    }
    public String getHw()
    {
        return _hw;
    }
    public void setJam( String jam )
    {
        _jam = jam;
    }
    public String getJam()
    {
        return _jam;
    }
    public void setOldDatum( String oldDatum )
    {
        _oldDatum = oldDatum;
    }
    public String getOldDatum()
    {
        return _oldDatum;
    }
    public void setPrelim( String prelim )
    {
        _prelim = prelim;
    }
    public String getPrelim()
    {
        return _prelim;
    }
}
