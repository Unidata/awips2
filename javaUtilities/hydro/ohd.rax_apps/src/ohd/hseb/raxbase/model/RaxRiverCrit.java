    package ohd.hseb.raxbase.model;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.raxbase.util.PEManager;
import ohd.hseb.raxdb.generated.RaxRiverCritRecord;

public class RaxRiverCrit
{
    private static final int MISSING = -9999;
    
    private String _lid = null;
    private String _pe = null;
    private long _validTime = MISSING;
    private double _lowScreen = MISSING;
    private double _sigRate = MISSING;
    private double _screenRate = MISSING;
    private double _fis = MISSING;
    private double _action = MISSING;
    private double _alert = MISSING;
    private double _bank = MISSING;
    private double _minorFloodStage = MISSING;
    private double _moderateFloodStage = MISSING;
    private double _majorFloodStage = MISSING;
    private double _record = MISSING;
    private double _highScreen = MISSING;
    private double _damScreen = MISSING;
    private double _lowScreenF = MISSING;
    private double _sigRateF = MISSING;
    private double _screenRateF = MISSING;
    private double _fisf = MISSING;
    private double _actionF = MISSING;
    private double _alertF = MISSING;
    private double _bankF = MISSING;
    private double _minorFloodFlow = MISSING;
    private double _moderateFloodFlow = MISSING;
    private double _majorFloodFlow = MISSING;
    private double _recordF = MISSING;
    private double _highScreenF = MISSING;
    private double _damScreenF = MISSING;
    private double _sigRateT = MISSING;
    private double _screenRateT = MISSING;
    private String _lowScreenQ = null;
    private String _sigRateQ = null;
    private String _screenRateQ = null;
    private String _fisQ = null;
    private String _actionQ = null;
    private String _alertQ = null;
    private String _bankQ = null;
    private String _floodQ = null;
    private String _modFloodQ = null;
    private String _majFloodQ = null;
    private String _recordQ = null;
    private String _highScreenQ = null;
    private String _damScreenQ = null;
    private String _stream = null;
    private double _latitude = MISSING;
    private double _longitude = MISSING;
    private double _da = MISSING;
    private double _mile = MISSING;
    private double _zd = MISSING;
    private String _vdatum = null;
    private double _cb = MISSING;
    private String _level = null;
    private double _pool = MISSING;
    private String _por = null;
    private String _tide = null;
    private String _backwater = null;
    private long _rrevise = MISSING;
    private String _rsource = null;
    private double _responseTime = MISSING;
    private double _thresholdRunoff = MISSING;
    private int _uhgDur = MISSING;
    private String _remark = null;
    
    public RaxRiverCrit() {}

    public RaxRiverCrit( RaxRiverCritRecord record )
    {
        setLid( record.getLid() );
        setPe( record.getPe1() + record.getPe2() );
        setValidTime( record.getVdtime() );
        setLowScreen( record.getLowscreen() );
        setSigRate( record.getSigrate() );
        setScreenRate( record.getScreenrate() );
        setFis( record.getFis() );
        setAction( record.getAction() );
        setAlert( record.getAlert() );
        setBank( record.getBank() );
        setMinorFloodStage( record.getFlood() );
        setModerateFloodStage( record.getModflood() );
        setMajorFloodStage( record.getMajflood() );
        setRecord( record.getRecord() );
        setHighScreen( record.getHighscreen() );
        setDamScreen( record.getDamscreen() );
        setLowScreenF( record.getLowscreenf() );
        setSigRateF( record.getSigratef() );
        setScreenRateF( record.getScreenratef() );
        setFisF( record.getFisf() );
        setActionF( record.getActionf() );
        setAlertF( record.getAlertf() );
        setBankF( record.getBankf() );
        setMinorFloodFlow( record.getFloodf() );
        setModerateFloodFlow( record.getModfloodf() );
        setMajorFloodFlow( record.getMajfloodf() );
        setRecordF( record.getRecordf() );
        setHighScreenF( record.getHighscreenf() );
        setDamScreenF( record.getDamscreenf() );
        setSigRateT( record.getSigratet() );
        setScreenRateT( record.getScreenratet() );
        setLowScreenQ( record.getLowscreenq() );
        setSigRateQ( record.getSigrateq() );
        setScreenRateQ( record.getScreenrateq() );
        setFisQ( record.getFisq() );
        setActionQ( record.getActionq() );
        setAlertQ( record.getAlertq() );
        setBankQ( record.getBankq() );
        setFloodQ( record.getFloodq() );
        setModFloodQ( record.getModfloodq() );
        setMajFloodQ( record.getMajfloodq() );
        setRecordQ( record.getRecordq() );
        setHighScreenQ( record.getHighscreenq() );
        setDamScreenQ( record.getDamscreenq() );
        setStream( record.getStream() );
        setLatitude( record.getLat() );
        setLongitude( record.getLon() );
        setDa( record.getDa() );
        setMile( record.getMile() );
        setZd( record.getZd() );
        setVdatum( record.getVdatum() );
        setCb( record.getCb() );
        setLevel( record.getLevel() );
        setPool( record.getPool() );
        setPor( record.getPor() );
        setTide( record.getTide() );
        setBackwater( record.getBackwater() );
        setRrevise( record.getRrevise() );
        setRsource( record.getRsource() );
        setResponseTime( record.getResponse_time() );
        setThresholdRunoff( record.getThreshold_runoff() );
        setUhgdur( record.getUhgdur() );
        setRemark( record.getRemark() );
    }
    
    public RaxRiverCrit( RaxRiverCrit raxRiverCrit )
    {
        setLid( raxRiverCrit.getLid() );
        setPe( raxRiverCrit.getPe() );
        setValidTime( raxRiverCrit.getValidTime() );
        setLowScreen( raxRiverCrit.getLowScreen() );
        setSigRate( raxRiverCrit.getSigRate() );
        setScreenRate( raxRiverCrit.getScreenRate() );
        setFis( raxRiverCrit.getFis() );
        setAction( raxRiverCrit.getAction() );
        setAlert( raxRiverCrit.getAlert() );
        setBank( raxRiverCrit.getBank() );
        setMinorFloodStage( raxRiverCrit.getMinorFloodStage() );
        setModerateFloodStage( raxRiverCrit.getModerateFloodStage() );
        setMajorFloodStage( raxRiverCrit.getMajorFloodStage() );
        setRecord( raxRiverCrit.getRecord() );
        setHighScreen( raxRiverCrit.getHighScreen() );
        setDamScreen( raxRiverCrit.getDamScreen() );
        setLowScreenF( raxRiverCrit.getLowScreenF() );
        setSigRateF( raxRiverCrit.getSigRateF() );
        setScreenRateF( raxRiverCrit.getScreenRateF() );
        setFisF( raxRiverCrit.getFisF() );
        setActionF( raxRiverCrit.getActionF() );
        setAlertF( raxRiverCrit.getAlertF() );
        setBankF( raxRiverCrit.getBankF() );
        setMinorFloodFlow( raxRiverCrit.getMinorFloodFlow() );
        setModerateFloodFlow( raxRiverCrit.getModerateFloodFlow() );
        setMajorFloodFlow( raxRiverCrit.getMajorFloodFlow() );
        setRecordF( raxRiverCrit.getRecordF() );
        setHighScreenF( raxRiverCrit.getHighScreenF() );
        setDamScreenF( raxRiverCrit.getDamScreenF() );
        setSigRateT( raxRiverCrit.getSigRateT() );
        setScreenRateT( raxRiverCrit.getScreenRateT() );
        setLowScreenQ( raxRiverCrit.getLowScreenQ() );
        setSigRateQ( raxRiverCrit.getSigRateQ() );
        setScreenRateQ( raxRiverCrit.getScreenRateQ() );
        setFisQ( raxRiverCrit.getFisQ() );
        setActionQ( raxRiverCrit.getActionQ() );
        setAlertQ( raxRiverCrit.getAlertQ() );
        setBankQ( raxRiverCrit.getBankQ() );
        setFloodQ( raxRiverCrit.getFloodQ() );
        setModFloodQ( raxRiverCrit.getModFloodQ() );
        setMajFloodQ( raxRiverCrit.getMajFloodQ() );
        setRecordQ( raxRiverCrit.getRecordQ() );
        setHighScreenQ( raxRiverCrit.getHighScreenQ() );
        setDamScreenQ( raxRiverCrit.getDamScreenQ() );
        setStream( raxRiverCrit.getStream() );
        setLatitude( raxRiverCrit.getLatitude() );
        setLongitude( raxRiverCrit.getLongitude() );
        setDa( raxRiverCrit.getDa() );
        setMile( raxRiverCrit.getMile() );
        setZd( raxRiverCrit.getZd() );
        setVdatum( raxRiverCrit.getVdatum() );
        setCb( raxRiverCrit.getCb() );
        setLevel( raxRiverCrit.getLevel() );
        setPool( raxRiverCrit.getPool() );
        setPor( raxRiverCrit.getPor() );
        setTide( raxRiverCrit.getTide() );
        setBackwater( raxRiverCrit.getBackwater() );
        setRrevise( raxRiverCrit.getRrevise() );
        setRsource( raxRiverCrit.getRsource() );
        setResponseTime( raxRiverCrit.getResponseTime() );
        setThresholdRunoff( raxRiverCrit.getThresholdRunoff() );
        setUhgdur( raxRiverCrit.getUhgdur() );
        setRemark( raxRiverCrit.getRemark() );
    }
    
    public static RaxRiverCrit getRaxRiverCrit( RaxRiverCritRecord record )
    {
        return ( new RaxRiverCrit( record ) );
    }
    
    public static RaxRiverCritRecord getRaxRiverCritRecord( RaxRiverCrit raxRiverCrit )
    {
        RaxRiverCritRecord record = new RaxRiverCritRecord();
     
        record.setLid( raxRiverCrit.getLid() );
        record.setPe1( PEManager.getPe1FromPE( raxRiverCrit.getPe() ) );
        record.setPe2( PEManager.getPe2FromPE( raxRiverCrit.getPe() ) );
        record.setVdtime( raxRiverCrit.getValidTime() );
        record.setLowscreen( raxRiverCrit.getLowScreen() );
        record.setSigrate(raxRiverCrit.getSigRate());
        record.setScreenrate(raxRiverCrit.getScreenRate());
        record.setFis(raxRiverCrit.getFis());
        record.setAction(raxRiverCrit.getAction());
        record.setAlert(raxRiverCrit.getAlert());
        record.setBank(raxRiverCrit.getBank());
        record.setFlood(raxRiverCrit.getMinorFloodStage());
        record.setModflood(raxRiverCrit.getModerateFloodStage());
        record.setMajflood( raxRiverCrit.getMajorFloodStage() );
        record.setRecord( raxRiverCrit.getRecord() );
        record.setHighscreen( raxRiverCrit.getHighScreen() );
        record.setDamscreen( raxRiverCrit.getDamScreen() );
        record.setLowscreenf( raxRiverCrit.getLowScreenF() );
        record.setSigratef( raxRiverCrit.getSigRateF() );
        record.setScreenratef( raxRiverCrit.getScreenRateF() );
        record.setFisf( raxRiverCrit.getFisF() );
        record.setActionf( raxRiverCrit.getActionF() );
        record.setAlertf( raxRiverCrit.getAlertF() );
        record.setBankf( raxRiverCrit.getBankF() );
        record.setFloodf( raxRiverCrit.getMinorFloodFlow() );
        record.setModfloodf( raxRiverCrit.getModerateFloodFlow() );
        record.setMajfloodf( raxRiverCrit.getMajorFloodFlow() );
        record.setRecordf( raxRiverCrit.getRecordF() );
        record.setHighscreenf( raxRiverCrit.getHighScreenF() );
        record.setDamscreenf( raxRiverCrit.getDamScreenF() );
        record.setSigratet( raxRiverCrit.getSigRateT() );
        record.setScreenratet( raxRiverCrit.getScreenRateT() );
        record.setLowscreenq( raxRiverCrit.getLowScreenQ() );
        record.setSigrateq( raxRiverCrit.getSigRateQ() );
        record.setScreenrateq( raxRiverCrit.getScreenRateQ() );
        record.setFisq( raxRiverCrit.getFisQ() );
        record.setActionq( raxRiverCrit.getActionQ() );
        record.setAlertq( raxRiverCrit.getAlertQ() );
        record.setBankq( raxRiverCrit.getBankQ() );
        record.setFloodq( raxRiverCrit.getFloodQ() );
        record.setModfloodq( raxRiverCrit.getModFloodQ() );
        record.setMajfloodq( raxRiverCrit.getMajFloodQ() );
        record.setRecordq( raxRiverCrit.getRecordQ() );
        record.setHighscreenq( raxRiverCrit.getHighScreenQ() );
        record.setDamscreenq( raxRiverCrit.getDamScreenQ() );
        record.setStream( raxRiverCrit.getStream() );
        record.setLat( raxRiverCrit.getLatitude() );
        record.setLon( raxRiverCrit.getLongitude() );
        record.setDa( raxRiverCrit.getDa() );
        record.setMile( raxRiverCrit.getMile() );
        record.setZd( raxRiverCrit.getZd() );
        record.setVdatum( raxRiverCrit.getVdatum() );
        record.setCb( raxRiverCrit.getCb() );
        record.setLevel( raxRiverCrit.getLevel() );
        record.setPool( raxRiverCrit.getPool() );
        record.setPor( raxRiverCrit.getPor() );
        record.setTide( raxRiverCrit.getTide() );
        record.setBackwater( raxRiverCrit.getBackwater() );
        record.setRrevise( raxRiverCrit.getRrevise() );
        record.setRsource( raxRiverCrit.getRsource() );
        record.setResponse_time( raxRiverCrit.getResponseTime() );
        record.setThreshold_runoff( raxRiverCrit.getThresholdRunoff() );
        record.setUhgdur( raxRiverCrit.getUhgdur() );
        record.setRemark( raxRiverCrit.getRemark());

        return record;
    }
    
    public String keyString()
    {
        return "Lid = " + _lid + " | Pe = " + _pe + " ValidTime = " + DbTimeHelper.getDateStringFromLongTime( _validTime );
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
    public void setValidTime( long validTime )
    {
        _validTime = validTime;
    }
    public long getValidTime()
    {
        return _validTime;
    }
    public void setLowScreen( double lowScreen )
    {
        _lowScreen = lowScreen;
    }
    public double getLowScreen()
    {
        return _lowScreen;
    }
    
    public void setSigRate( double sigrate )
    {
        _sigRate = sigrate;
    }
    
    public double getSigRate()
    {
        return _sigRate;
    }
    
    public void setScreenRate( double screenRate )
    {
        _screenRate = screenRate;
    }

    public double getScreenRate()
    {
        return _screenRate;
    }
    
    public void setFis( double fis )
    {
        _fis = fis;
    }
    
    public double getFis()
    {
        return _fis;
    }
    
    public void setAction( double action )
    {
        _action = action;
    }
    
    public double getAction()
    {
        return _action;
    }
    
    public void setAlert( double alert )
    {
        _alert = alert;
    }
    
    public double getAlert()
    {
        return _alert;
    }
    public void setBank( double bank )
    {
        _bank = bank;
    }
    public double getBank()
    {
        return _bank;
    }
    public void setMinorFloodStage( double minorFloodStage )
    {
        _minorFloodStage = minorFloodStage;
    }
    public double getMinorFloodStage()
    {
        return _minorFloodStage;
    }
    public void setModerateFloodStage( double moderateFloodStage )
    {
        _moderateFloodStage = moderateFloodStage;
    }
    public double getModerateFloodStage()
    {
        return _moderateFloodStage;
    }
    public void setMajorFloodStage( double majorFloodStage )
    {
        _majorFloodStage = majorFloodStage;
    }
    public double getMajorFloodStage()
    {
        return _majorFloodStage;
    }
    public void setRecord( double record )
    {
        _record = record;
    }
    public double getRecord()
    {
        return _record;
    }
    public void setHighScreen( double highscreen )
    {
        _highScreen = highscreen;
    }
    public double getHighScreen()
    {
        return _highScreen;
    }
    public void setDamScreen( double damscreen )
    {
        _damScreen = damscreen;
    }
    public double getDamScreen()
    {
        return _damScreen;
    }
    public void setLowScreenF( double lowscreenf )
    {
        _lowScreenF = lowscreenf;
    }
    public double getLowScreenF()
    {
        return _lowScreenF;
    }
    public void setSigRateF( double sigratef )
    {
        _sigRateF = sigratef;
    }
    public double getSigRateF()
    {
        return _sigRateF;
    }
    public void setScreenRateF( double screenratef )
    {
        _screenRateF = screenratef;
    }
    public double getScreenRateF()
    {
        return _screenRateF;
    }
    public void setFisF( double fisf )
    {
        _fisf = fisf;
    }
    public double getFisF()
    {
        return _fisf;
    }
    public void setActionF( double actionf )
    {
        _actionF = actionf;
    }
    public double getActionF()
    {
        return _actionF;
    }
    public void setAlertF( double alertf )
    {
        _alertF = alertf;
    }
    public double getAlertF()
    {
        return _alertF;
    }
    public void setBankF( double bankf )
    {
        _bankF = bankf;
    }
    public double getBankF()
    {
        return _bankF;
    }
    public void setMinorFloodFlow( double minorFloodFlow )
    {
        _minorFloodFlow = minorFloodFlow;
    }
    public double getMinorFloodFlow()
    {
        return _minorFloodFlow;
    }
    public void setModerateFloodFlow( double moderateFloodFlow )
    {
        _moderateFloodFlow = moderateFloodFlow;
    }
    public double getModerateFloodFlow()
    {
        return _moderateFloodFlow;
    }

    public void setMajorFloodFlow( double majorFloodFlow )
    {
        _majorFloodFlow = majorFloodFlow;
    }

    public double getMajorFloodFlow()
    {
        return _majorFloodFlow;
    }

    public void setRecordF( double recordf )
    {
        _recordF = recordf;
    }

    public double getRecordF()
    {
        return _recordF;
    }

    public void setHighScreenF( double highscreenf )
    {
        _highScreenF = highscreenf;
    }

    public double getHighScreenF()
    {
        return _highScreenF;
    }

    public void setDamScreenF( double damscreenf )
    {
        _damScreenF = damscreenf;
    }

    public double getDamScreenF()
    {
        return _damScreenF;
    }

    public void setSigRateT( double sigratet )
    {
        _sigRateT = sigratet;
    }

    public double getSigRateT()
    {
        return _sigRateT;
    }

    public void setScreenRateT( double screenratet )
    {
        _screenRateT = screenratet;
    }

    public double getScreenRateT()
    {
        return _screenRateT;
    }

    public void setLowScreenQ( String lowscreenq )
    {
        _lowScreenQ = lowscreenq;
    }

    public String getLowScreenQ()
    {
        return _lowScreenQ;
    }

    public void setSigRateQ( String sigrateq )
    {
        _sigRateQ = sigrateq;
    }

    public String getSigRateQ()
    {
        return _sigRateQ;
    }

    public void setScreenRateQ( String screenrateq )
    {
        _screenRateQ = screenrateq;
    }

    public String getScreenRateQ()
    {
        return _screenRateQ;
    }

    public void setFisQ( String fisq )
    {
        _fisQ = fisq;
    }

    public String getFisQ()
    {
        return _fisQ;
    }

    public void setActionQ( String actionq )
    {
        _actionQ = actionq;
    }

    public String getActionQ()
    {
        return _actionQ;
    }
    public void setAlertQ( String alertq )
    {
        _alertQ = alertq;
    }
    public String getAlertQ()
    {
        return _alertQ;
    }
    public void setBankQ( String bankq )
    {
        _bankQ = bankq;
    }
    public String getBankQ()
    {
        return _bankQ;
    }
    public void setFloodQ( String floodq )
    {
        _floodQ = floodq;
    }
    public String getFloodQ()
    {
        return _floodQ;
    }
    public void setModFloodQ( String modfloodq )
    {
        _modFloodQ = modfloodq;
    }
    public String getModFloodQ()
    {
        return _modFloodQ;
    }
    public void setMajFloodQ( String majfloodq )
    {
        _majFloodQ = majfloodq;
    }
    public String getMajFloodQ()
    {
        return _majFloodQ;
    }
    public void setRecordQ( String recordq )
    {
        _recordQ = recordq;
    }
    public String getRecordQ()
    {
        return _recordQ;
    }
    public void setHighScreenQ( String highscreenq )
    {
        _highScreenQ = highscreenq;
    }
    public String getHighScreenQ()
    {
        return _highScreenQ;
    }
    public void setDamScreenQ( String damscreenq )
    {
        _damScreenQ = damscreenq;
    }
    public String getDamScreenQ()
    {
        return _damScreenQ;
    }
    public void setStream( String stream )
    {
        _stream = stream;
    }
    public String getStream()
    {
        return _stream;
    }
    public void setLatitude( double latitude )
    {
        _latitude = latitude;
    }
    public double getLatitude()
    {
        return _latitude;
    }
    public void setLongitude( double longitude )
    {
        _longitude = longitude;
    }
    public double getLongitude()
    {
        return _longitude;
    }
    public void setDa( double da )
    {
        _da = da;
    }
    public double getDa()
    {
        return _da;
    }
    public void setMile( double mile )
    {
        _mile = mile;
    }
    public double getMile()
    {
        return _mile;
    }
    public void setZd( double zd )
    {
        _zd = zd;
    }
    public double getZd()
    {
        return _zd;
    }
    public void setVdatum( String vdatum )
    {
        _vdatum = vdatum;
    }
    public String getVdatum()
    {
        return _vdatum;
    }
    public void setCb( double cb )
    {
        _cb = cb;
    }
    public double getCb()
    {
        return _cb;
    }
    public void setLevel( String level )
    {
        _level = level;
    }
    public String getLevel()
    {
        return _level;
    }
    public void setPool( double pool )
    {
        _pool = pool;
    }
    public double getPool()
    {
        return _pool;
    }
    public void setPor( String por )
    {
        _por = por;
    }
    public String getPor()
    {
        return _por;
    }
    public void setTide( String tide )
    {
        _tide = tide;
    }
    public String getTide()
    {
        return _tide;
    }
    public void setBackwater( String backwater )
    {
        _backwater = backwater;
    }
    public String getBackwater()
    {
        return _backwater;
    }
    public void setRrevise( long rrevise )
    {
        _rrevise = rrevise;
    }
    public long getRrevise()
    {
        return _rrevise;
    }
    public void setRsource( String rsource )
    {
        _rsource = rsource;
    }
    public String getRsource()
    {
        return _rsource;
    }
    public void setResponseTime( double responseTime )
    {
        _responseTime = responseTime;
    }
    public double getResponseTime()
    {
        return _responseTime;
    }
    public void setThresholdRunoff( double thresholdRunoff )
    {
        _thresholdRunoff = thresholdRunoff;
    }
    public double getThresholdRunoff()
    {
        return _thresholdRunoff;
    }
    public void setUhgdur( int uhgdur )
    {
        _uhgDur = uhgdur;
    }
    public int getUhgdur()
    {
        return _uhgDur;
    }
    public void setRemark( String remark )
    {
        _remark = remark;
    }
    public String getRemark()
    {
        return _remark;
    }
}
