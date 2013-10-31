package ohd.hseb.raxbase.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ohd.hseb.raxbase.RaxBaseDataMgr;
import ohd.hseb.raxbase.model.ShefDuration;
import ohd.hseb.raxbase.model.ShefExtremum;
import ohd.hseb.raxbase.model.ShefPE;
import ohd.hseb.raxbase.model.ShefProb;
import ohd.hseb.raxbase.model.ShefTS;
import ohd.hseb.util.gui.LabeledComboBox;

public class PEDTSEPManager
{
    private RaxBaseDataMgr _dataMgr = null;
    private LabeledComboBox _peLCB = null;
    private LabeledComboBox _durLCB = null;
    private LabeledComboBox _tsLCB = null;
    private LabeledComboBox _extremumLCB = null;
    private LabeledComboBox _probLCB = null;

    private List _peCBStringList = new ArrayList();
    private List _tsCBStringList = new ArrayList();
    private Map _peCBStringToPeMap = new HashMap();
    private Map _durCBStringToDurMap = new HashMap();
    private Map _durCBStringToIDurMap = new HashMap();
    private Map _probCBStringToProbMap = new HashMap();
    private Map _tsCBStringToTSMap = new HashMap();
    private Map _extremumCBStringToExtremumMap = new HashMap();

    public PEDTSEPManager( RaxBaseDataMgr dataMgr )
    {
        _dataMgr = dataMgr;
        initComboBoxes();
    }
    
    private void initComboBoxes()
    {
        List shefPeList = _dataMgr.getShefPeList();
        List shefDurationList = _dataMgr.getShefDurationList();
        List shefTypeSourceList = _dataMgr.getShefTSList();
        List shefExtremumList = _dataMgr.getShefExtremumList();
        List shefProbList = _dataMgr.getShefProbList();
        
        List shefPeStringList = new ArrayList();
        List shefDurationStringList = new ArrayList();
        List shefTypeSourceStringList = new ArrayList();
        List shefExtremumStringList = new ArrayList();
        List shefProbStringList = new ArrayList();
        
        for ( int i = 0; i < shefPeList.size(); i++ )
        {
            ShefPE shefPE = (ShefPE) shefPeList.get( i );
            shefPeStringList.add( getShefPEComboBoxString( shefPE ) );
            _peCBStringList.add( getShefPEComboBoxString( shefPE ) );
            _peCBStringToPeMap.put( getShefPEComboBoxString( shefPE ), shefPE.getPe() );
        }
        
        for ( int i = 0; i < shefDurationList.size(); i++ )
        {
            ShefDuration shefDur = (ShefDuration) shefDurationList.get( i );
            shefDurationStringList.add( getShefDurationComboBoxString( shefDur ) );
            _durCBStringToDurMap.put( getShefDurationComboBoxString( shefDur ), shefDur.getDuration() );
            _durCBStringToIDurMap.put( getShefDurationComboBoxString( shefDur ), shefDur.getIduration() );
        }
        
        for ( int i = 0; i < shefProbList.size(); i++ )
        {
            ShefProb shefProb = (ShefProb) shefProbList.get( i );
            shefProbStringList.add( getShefProbComboBoxString( shefProb ) );
            _probCBStringToProbMap.put( getShefProbComboBoxString( shefProb ), shefProb.getP() );
        }
        
        for ( int i = 0; i < shefTypeSourceList.size(); i++ )
        {
            ShefTS shefTS = (ShefTS) shefTypeSourceList.get( i );
            shefTypeSourceStringList.add( getShefTSString( shefTS ) );
            _tsCBStringList.add( getShefTSString( shefTS ) );
            _tsCBStringToTSMap.put( getShefTSString( shefTS ), shefTS.getTs() );
        }
        
        for ( int i = 0; i < shefExtremumList.size(); i++ )
        {
            ShefExtremum shefEx = (ShefExtremum) shefExtremumList.get( i );
            shefExtremumStringList.add( getShefExtremumComboBoxString(shefEx ) );
            _extremumCBStringToExtremumMap.put( getShefExtremumComboBoxString( shefEx ), shefEx.getExtremum() );
        }
        
        _peLCB = new LabeledComboBox( "PE:", shefPeStringList );
        _durLCB = new LabeledComboBox( "Duration:", shefDurationStringList );
        _tsLCB = new LabeledComboBox( "Type Source:", shefTypeSourceStringList );
        _extremumLCB = new LabeledComboBox( "Extremum:", shefExtremumStringList );
        _probLCB = new LabeledComboBox( "Probability:", shefProbStringList );
    }
    
    public String getShefExtremumComboBoxString( ShefExtremum shefEx )
    {
        String shefExtremumString = shefEx.getName() + " (" + shefEx.getExtremum() + ")";
        
        return shefExtremumString;
    }
    
    public String getShefTSString( ShefTS shefTS )
    {
        String shefTSString = shefTS.getName() + " (" + shefTS.getTs() + ")";
        
        return shefTSString;
    }
    
    public String getShefPEComboBoxString( ShefPE shefPE )
    {
        String shefPEString = shefPE.getPe() + " " + shefPE.getName();
        
        return shefPEString;
    }
    
    public String getShefDurationComboBoxString( ShefDuration shefDuration )
    {
        String shefDurationString = shefDuration.getName() + " ( " + shefDuration.getIduration() + "/" + shefDuration.getDuration() + " )";

        return shefDurationString;
    }

    public String getShefProbComboBoxString( ShefProb shefProb )
    {
        String shefProbString = "";
        
        if ( shefProb != null )
        {
            shefProbString = shefProb.getP() + "/" + shefProb.getProbability();
        }
        
        return shefProbString;
    }

    public LabeledComboBox getPeLCB()
    {
        return _peLCB;
    }

    public LabeledComboBox getDurLCB()
    {
        return _durLCB;
    }

    public LabeledComboBox getTsLCB()
    {
        return _tsLCB;
    }

    public LabeledComboBox getExtremumLCB()
    {
        return _extremumLCB;
    }

    public LabeledComboBox getProbLCB()
    {
        return _probLCB;
    }

    public Map getPeCBStringToPeMap()
    {
        return _peCBStringToPeMap;
    }

    public Map getDurCBStringToDurMap()
    {
        return _durCBStringToDurMap;
    }

    public Map getDurCBStringToIDurMap()
    {
        return _durCBStringToIDurMap;
    }
    public Map getProbCBStringToProbMap()
    {
        return _probCBStringToProbMap;
    }

    public Map getTsCBStringToTSMap()
    {
        return _tsCBStringToTSMap;
    }
    public Map getExtremumCBStringToExtremumMap()
    {
        return _extremumCBStringToExtremumMap;
    }

}
