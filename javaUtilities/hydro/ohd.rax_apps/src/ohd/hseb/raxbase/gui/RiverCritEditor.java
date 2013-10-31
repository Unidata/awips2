package ohd.hseb.raxbase.gui;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;

import ohd.hseb.db.DbTable;
import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.raxbase.RaxBaseDataMgr;
import ohd.hseb.raxbase.model.RaxRiverCrit;
import ohd.hseb.raxbase.util.DateManager;
import ohd.hseb.raxbase.util.HDateSuperChooser;
import ohd.hseb.rfc.util.graphics.HDateChooserOwner;
import ohd.hseb.util.StringDataConverter;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledTextField;

public class RiverCritEditor extends JDialog implements HDateChooserOwner
{
    private Container _dialogContentPane = getContentPane();
    
    private RaxBaseDataMgr _dataMgr = null;

    private HDateSuperChooser _hDateChooser = new HDateSuperChooser( false );
    
    private int _clickedDateField = -9999;

    private JPanel _floodCatPanel = new JPanel( new GridBagLayout() );
    private JPanel _procQCPanel = new JPanel( new GridBagLayout() );
    private JPanel _miscPanel = new JPanel( new GridBagLayout() );
    private JPanel _remarkPanel = new JPanel( new GridBagLayout() );
    private String _lid = null;
    private JComboBox _riverCritSelectionCB = new JComboBox();
    private List _raxRiverCritList = null;
    private Map _raxRiverCritToRiverCritSelectionCBMap = new HashMap();
    
    private StringDataConverter _converter = new StringDataConverter();
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _deleteButton = new JButton( "Delete" );
    
    private LabeledTextField _lidLTF = new LabeledTextField( "Lid:", "", "Location ID", 8 );
    private LabeledTextField _peLTF = new LabeledTextField( "PE:", "", "Physical Element", 4 );
    private LabeledTextField _validTimeLTF = new LabeledTextField( "Vdtime:", "", "Valid Time", 12 );
    private LabeledTextField _lowScreenLTF = new LabeledTextField( "LowScreen:", "", "Lower Screen Stage (ft):", 10 );
    private LabeledTextField _sigRateLTF = new LabeledTextField( "SigRate:", "", "Signifant Rise", 10 );
    private LabeledTextField _screenRateLTF = new LabeledTextField( "ScreenRate:", "", "Maximum Rise (rate of change) allowed for stage (ft)", 10 );
    private LabeledTextField _fisLTF = new LabeledTextField( "Fis:", "", "Forecast issuance stage (ft)", 10 );
    private LabeledTextField _actionLTF = new LabeledTextField( "Action:", "", "Action Stage (ft)", 10 );
    private LabeledTextField _alertLTF = new LabeledTextField( "Alert:", "", "Alert Stage (ft)", 10 );
    private LabeledTextField _bankLTF = new LabeledTextField( "Bank:", "", "Bankful Stage (ft)", 10 );
    private LabeledTextField _minorFloodStageLTF = new LabeledTextField( "Flood:", "", "Flood Stage (ft)", 10 );
    private LabeledTextField _moderateFloodStageLTF = new LabeledTextField( "ModFlood:", "", "Moderate Floodcat Stage (ft)", 10 );
    private LabeledTextField _majorFloodStageLTF = new LabeledTextField( "MajFlood:", "", "Major Floodcat Stage (ft)", 10 );
    private LabeledTextField _recordLTF = new LabeledTextField( "Record:", "", "Stage of Record (ft)", 10 );
    private LabeledTextField _highScreenLTF = new LabeledTextField( "HighScreen:", "", "Upper Limit Screen Stage (ft)", 10 );
    private LabeledTextField _damScreenLTF = new LabeledTextField( "DamScreen:", "", "Maximum Stage during Dam Break", 10 );
    private LabeledTextField _lowScreenFLTF = new LabeledTextField( "LowScreenF:", "", "Lower Screen Flow (cfs)", 10 );
    private LabeledTextField _sigRateFLTF = new LabeledTextField( "SigRateF:", "", "Significant Rise (rate of change) value for flow (cfs)", 10 );
    private LabeledTextField _screenRateFLTF = new LabeledTextField( "ScreenRateF:", "", "Maximum Rise (rate of change) allowed for flow (cfs)", 10 );
    private LabeledTextField _fisFLTF = new LabeledTextField( "FisF:", "", "Flow at forecast issuance stage (cfs)", 10 );
    private LabeledTextField _actionFLTF = new LabeledTextField( "ActionF:", "", "Flow at Action Stage (cfs)", 10 );
    private LabeledTextField _alertFLTF = new LabeledTextField( "AlertF:", "", "Flow for Alert Stage (cfs)", 10 );
    private LabeledTextField _bankFLTF = new LabeledTextField( "BankF:", "", "Flow at bankful Stage (cfs)", 10 );
    private LabeledTextField _minorFloodFlowLTF = new LabeledTextField( "FloodF:", "", "Flow at Flood Stage (cfs)", 10 );
    private LabeledTextField _moderateFloodFlowLTF = new LabeledTextField( "ModFloodF:", "", "Flow of Moderate floodcat stage (cfs)", 10 );
    private LabeledTextField _majorFloodFlowLTF = new LabeledTextField( "MajFloodF:", "", "Flow of Major floodcat stage (cfs)", 10 );
    private LabeledTextField _recordFLTF = new LabeledTextField( "RecordF:", "", "Flow of Record (cfs)", 10 );
    private LabeledTextField _highScreenFLTF = new LabeledTextField( "HighScreenF:", "", "Upper Limit Screen Flow (cfs)", 10 );
    private LabeledTextField _damScreenFLTF = new LabeledTextField( "DamScreenF:", "", "Maximum Flow during Dam Break", 10 );
    private LabeledTextField _sigRateTLTF = new LabeledTextField( "SigRateT:", "", "Time interval for sigrate/sgratef (hours or fractional hours)", 10 );
    private LabeledTextField _screenRateTLTF = new LabeledTextField( "ScreenRateT:", "", "Time interval for screenrate/screenratef (hours or fractional hours)", 10 );
    private LabeledTextField _lowScreenQLTF = new LabeledTextField( "LowScreenQ:", "", "<HTML>Source of lowscreen/lowscreenf:<BR><B>E</B>=e-19<BR><B>M</B>=memo" + 
                                                                    "<BR><B>P</B>=personal communication (i.e. phone)<BR><B>I</B>=internal (i.e. RFC determined)" +
                                                                    "<BR><B>U</B>=unknown</HTML>", 10 );
    private LabeledTextField _sigRateQLTF = new LabeledTextField( "SigRateQ:", "", "<HTML>Source for sigrate/sigratf, valid values:<BR><B>E</B>=e-19<BR><B>M</B>=memo" + 
                                                                    "<BR><B>P</B>=personal communication (i.e. phone)<BR><B>I</B>=internal (i.e. RFC determined)" +
                                                                    "<BR><B>U</B>=unknown</HTML>", 10 );
    private LabeledTextField _screenRateQLTF = new LabeledTextField( "ScreenRateQ:", "", "<HTML>Source for screenrate/screenratef:<BR><B>E</B>=e-19<BR><B>M</B>=memo" + 
                                                                    "<BR><B>P</B>=personal communication (i.e. phone)<BR><B>I</B>=internal (i.e. RFC determined)" +
                                                                    "<BR><B>U</B>=unknown</HTML>", 10 );
    private LabeledTextField _fisQLTF = new LabeledTextField( "FisQ:", "", "<HTML>Source of fis/fisf, valid values:<BR><B>E</B>=e-19<BR><B>M</B>=memo" + 
                                                                    "<BR><B>P</B>=personal communication (i.e. phone)<BR><B>I</B>=internal (i.e. RFC determined)" +
                                                                    "<BR><B>U</B>=unknown</HTML>", 10 );
    private LabeledTextField _actionQLTF = new LabeledTextField( "ActionQ:", "", "<HTML>Source of action/actionf, valid values:<BR><B>E</B>=e-19<BR><B>M</B>=memo" + 
                                                                    "<BR><B>P</B>=personal communication (i.e. phone)<BR><B>I</B>=internal (i.e. RFC determined)" +
                                                                    "<BR><B>U</B>=unknown</HTML>", 10 );
    private LabeledTextField _alertQLTF = new LabeledTextField( "AlertQ", "", "<HTML>Source of alert/alertf, valid values:<BR><B>E</B>=e-19<BR><B>M</B>=memo" + 
                                                                    "<BR><B>P</B>=personal communication (i.e. phone)<BR><B>I</B>=internal (i.e. RFC determined)" +
                                                                    "<BR><B>U</B>=unknown</HTML>", 10 );
    private LabeledTextField _bankQLTF = new LabeledTextField( "BankQ:", "", "<HTML>Source of bank/bankf, valid values:<BR><B>E</B>=e-19<BR><B>M</B>=memo" + 
                                                                    "<BR><B>P</B>=personal communication (i.e. phone)<BR><B>I</B>=internal (i.e. RFC determined)" +
                                                                    "<BR><B>U</B>=unknown</HTML>", 10 );
    private LabeledTextField _floodQLTF = new LabeledTextField( "FloodQ:", "", "<HTML>Source of flood/floodf, valid values:<BR><B>E</B>=e-19<BR><B>M</B>=memo" + 
                                                                    "<BR><B>P</B>=personal communication (i.e. phone)<BR><B>I</B>=internal (i.e. RFC determined)" +
                                                                    "<BR><B>U</B>=unknown</HTML>", 10 );
    private LabeledTextField _modFloodQLTF = new LabeledTextField( "ModFloodQ:", "", "<HTML>Source of modflood/modfloodq:<BR><B>E</B>=e-19<BR><B>M</B>=memo" + 
                                                                    "<BR><B>P</B>=personal communication (i.e. phone)<BR><B>I</B>=internal (i.e. RFC determined)" +
                                                                    "<BR><B>U</B>=unknown</HTML>", 10 );
    private LabeledTextField _majFloodQLTF = new LabeledTextField( "MajFloodQ:", "", "<HTML>Source of majflood/majfloodf:<BR><B>E</B>=e-19<BR><B>M</B>=memo" + 
                                                                    "<BR><B>P</B>=personal communication (i.e. phone)<BR><B>I</B>=internal (i.e. RFC determined)" +
                                                                    "<BR><B>U</B>=unknown</HTML>", 10 );
    private LabeledTextField _recordQLTF = new LabeledTextField( "RecordQ:", "", "<HTML>Source of record/recordf, valid values:<BR><B>E</B>=e-19<BR><B>M</B>=memo" + 
                                                                    "<BR><B>P</B>=personal communication (i.e. phone)<BR><B>I</B>=internal (i.e. RFC determined)" +
                                                                    "<BR><B>U</B>=unknown</HTML>", 10 );
    private LabeledTextField _highScreenQLTF = new LabeledTextField( "HighScreenQ:", "", "<HTML>Source of highscreen/highscreenf:<BR><B>E</B>=e-19<BR><B>M</B>=memo" + 
                                                                    "<BR><B>P</B>=personal communication (i.e. phone)<BR><B>I</B>=internal (i.e. RFC determined)" +
                                                                    "<BR><B>U</B>=unknown</HTML>", 10 );
    private LabeledTextField _damScreenQLTF = new LabeledTextField( "DamScreenQ:", "", "<HTML>Source of damscreen/damscreenf:<BR><B>E</B>=e-19<BR><B>M</B>=memo" + 
                                                                    "<BR><B>P</B>=personal communication (i.e. phone)<BR><B>I</B>=internal (i.e. RFC determined)" +
                                                                    "<BR><B>U</B>=unknown</HTML>", 10 );
    private LabeledTextField _streamLTF = new LabeledTextField( "Stream:", "", "Same of river/stream (mixed case)", 10 );
    private LabeledTextField _latitudeLTF = new LabeledTextField( "Latitude:", "", "Latitude (degrees decimal)", 10 );
    private LabeledTextField _longitudeLTF = new LabeledTextField( "Longitude:", "", "Longitude (degrees decimal)", 10 );
    private LabeledTextField _daLTF = new LabeledTextField( "Da:", "", "Drainage area (square miles)", 10 );
    private LabeledTextField _mileLTF = new LabeledTextField( "Mile:", "", "River Mile", 10 );
    private LabeledTextField _zdLTF = new LabeledTextField( "Zd:", "", "Elevation of the gage zero (i.e. when the gage reads zero) (ft MSL) (usually reported to the nearest 0.01 ft)", 10 );
    private LabeledTextField _vdatumLTF = new LabeledTextField( "VDatum:", "", "Reference value on which the gage zero (zd) is based. This generally is the National " +
                                                                               "Geodetic Vertical Datum of 1929 (NGVD 1929) or the North American Vertical Datum of 1988 " +
                                                                               "(NAVD 88). Not all locations use these datums (for example, river gages on the Ohio River " +
                                                                               "use NGVD 1913, also referred to as the Ohio River Datum).", 10 );
    private LabeledTextField _cbLTF = new LabeledTextField( "Cb:", "", "Gage is set for gage calibration purposes at a river station (ft)", 10 );
    private LabeledTextField _levelLTF = new LabeledTextField( "Level:", "", "", 10 );
    private LabeledTextField _poolLTF = new LabeledTextField( "Pool:", "", "Normal pool elevation (ft MSL) at a river station (desired elevation for water supply purposes, " +
                                                                           "is really the same as conserpool in the Reservoir table)", 10 );
    private LabeledTextField _porLTF = new LabeledTextField( "Por:", "", "Period of Record", 10 );
    private LabeledTextField _tideLTF = new LabeledTextField( "Tide:", "", "Indicator of tidal effects applicable to a river station, valid values: NONE, MINOR, MODERATE, MAJOR", 10 );
    private LabeledTextField _backwaterLTF = new LabeledTextField( "BackWater:", "", "Indicator of any backwater effects at a river gage site, valid values: NONE, MINOR, MODERATE, MAJOR", 10 );
    private LabeledTextField _rreviseLTF = new LabeledTextField( "RRevise:", "", "The date when E-19 data was last updated for a river station", 10 );
    private LabeledTextField _rsourceLTF = new LabeledTextField( "RSource:", "", "", 10 );
    private LabeledTextField _responseTimeLTF = new LabeledTextField( "ResponseTime:", "", "Typical or average response time (hours) for flooding to occur at this river station from " +
                                                                                           "a precipitation event", 10 );
    private LabeledTextField _thresholdRunoffLTF = new LabeledTextField( "ThresholdRunOff:", "", "Threshold runoff value (inches) for a site (used by the Site-Specific Model)", 10 );
    private LabeledTextField _uhgDurLTF = new LabeledTextField( "uhgDur:", "", "Unit hydrograph duration (hours)", 10 );
    
    private JLabel _remarkLabel = new JLabel( "Remark:" );
    private JTextArea _remarkTextArea = new JTextArea( 3, 80 );
    
    public RiverCritEditor( JFrame frame, RaxBaseDataMgr dataMgr, String lid, String title )
    {
        super( frame, "RiverCrit Editor - " + title, true );
        _dataMgr = dataMgr;
        _lid = lid;
    }
    
    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 1024, 768 ) );
        populateFields();
        initGUI();
    }

    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        setupRiverCritSelectionComboBox();
        initDataPanel();
        initButtonPanel();
        initFrameComponents();
        addListeners();
        populateFields();
        pack();
        setVisible( true );
    }
    
    private void setupRiverCritSelectionComboBox()
    {
        _raxRiverCritList = _dataMgr.setupAndGetRaxRiverCritList( _lid );
        RaxRiverCrit raxRiverCrit = null;
        
        if ( _raxRiverCritList.isEmpty() )
        {
            _riverCritSelectionCB.addItem( "No entries found in the database" );
        }
        else
        {
            for ( int i = 0; i < _raxRiverCritList.size(); i++ )
            {
                raxRiverCrit = (RaxRiverCrit) _raxRiverCritList.get( i );

                _riverCritSelectionCB.addItem( getLidPEVdateComboBoxString( raxRiverCrit ) );
                _raxRiverCritToRiverCritSelectionCBMap.put( raxRiverCrit, getLidPEVdateComboBoxString( raxRiverCrit ) );
                _raxRiverCritToRiverCritSelectionCBMap.put( getLidPEVdateComboBoxString( raxRiverCrit ), raxRiverCrit );
            }
        }
    }
    
    private String getLidPEVdateComboBoxString( RaxRiverCrit raxRiverCrit )
    {
        return ( "LID: " + raxRiverCrit.getLid() + " - PE: " + raxRiverCrit.getPe() + " - VDTIME: " + DbTimeHelper.getDateStringFromLongTime( raxRiverCrit.getValidTime() ) );
    }
    
    private RaxRiverCrit getRaxRiverCritFromLidPEVdateComboBoxString( String lidPeVdateString )
    {
        return (RaxRiverCrit) _raxRiverCritToRiverCritSelectionCBMap.get( lidPeVdateString );
    }
    
    private void initDataPanel()
    {
        initGUIItems();
        initFloodCatPanel();
        initProcQCPanel();
        initMiscPanel();
        initRemarkPanel();
    }
    
    private void initGUIItems()
    {
        _floodCatPanel.setBorder( BorderFactory.createTitledBorder( "Flood Category Data" ) );
        _procQCPanel.setBorder( BorderFactory.createTitledBorder( "Processors Quality Control" ) );
        _miscPanel.setBorder( BorderFactory.createTitledBorder( "Misc Data" ) );
        _hDateChooser.addOwner( this );
    }
    
    private void initFloodCatPanel()
    {
        _lidLTF.setEditTextField( false );
        _validTimeLTF.setEditTextField( false );
//                                                                                      X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _floodCatPanel, _lidLTF,                  0,   0,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _floodCatPanel, _peLTF,                   0,   1,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _floodCatPanel, _validTimeLTF,            0,   2,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _floodCatPanel, _fisLTF,                  0,   3,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _floodCatPanel, _fisFLTF,                 0,   4,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _floodCatPanel, _fisQLTF,                 0,   5,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _floodCatPanel, _actionLTF,               0,   6,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _floodCatPanel, _actionFLTF,              0,   7,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _floodCatPanel, _actionQLTF,              0,   8,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _floodCatPanel, _alertLTF,                0,   9,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _floodCatPanel, _alertFLTF,               0,  10,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _floodCatPanel, _alertQLTF,               0,  11,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _floodCatPanel, _bankLTF,                 0,  12,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _floodCatPanel, _bankFLTF,                0,  13,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _floodCatPanel, _bankQLTF,                0,  14,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _floodCatPanel, _minorFloodStageLTF,      0,  15,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _floodCatPanel, _minorFloodFlowLTF,       0,  16,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _floodCatPanel, _floodQLTF,               0,  17,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        
        ComponentHelper.addFrameComponent( _floodCatPanel, _moderateFloodStageLTF,   0,  18,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _floodCatPanel, _moderateFloodFlowLTF,    0,  19,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _floodCatPanel, _modFloodQLTF,            0,  20,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );

        ComponentHelper.addFrameComponent( _floodCatPanel, _majorFloodStageLTF,      0,  21,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _floodCatPanel, _majorFloodFlowLTF,       0,  22,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _floodCatPanel, _majFloodQLTF,            0,  23,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );

        ComponentHelper.addFrameComponent( _floodCatPanel, _recordLTF,               0,  24,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _floodCatPanel, _recordFLTF,              0,  25,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _floodCatPanel, _recordQLTF,              0,  26,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
    }
    
    private void initProcQCPanel()
    {
//                                                                                      X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _procQCPanel, _lowScreenLTF,            0,   0,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _procQCPanel, _lowScreenFLTF,           0,   1,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _procQCPanel, _lowScreenQLTF,           0,   2,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );

        ComponentHelper.addFrameComponent( _procQCPanel, _highScreenLTF,           0,   3,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _procQCPanel, _highScreenFLTF,          0,   4,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _procQCPanel, _highScreenQLTF,          0,   5,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        
        ComponentHelper.addFrameComponent( _procQCPanel, _damScreenLTF,            0,   6,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _procQCPanel, _damScreenFLTF,           0,   7,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _procQCPanel, _damScreenQLTF,           0,   8,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );

        ComponentHelper.addFrameComponent( _procQCPanel, _sigRateLTF,              0,   9,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _procQCPanel, _sigRateFLTF,             0,  10,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _procQCPanel, _sigRateQLTF,             0,  11,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _procQCPanel, _sigRateTLTF,             0,  12,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );

        ComponentHelper.addFrameComponent( _procQCPanel, _screenRateLTF,           0,  13,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _procQCPanel, _screenRateFLTF,          0,  14,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _procQCPanel, _screenRateQLTF,          0,  15,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _procQCPanel, _screenRateTLTF,          0,  16,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
    }
    
    private void initMiscPanel()
    {
        _rreviseLTF.setEditTextField( false );
//                                                                           X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _miscPanel, _streamLTF,           0,   0,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _miscPanel, _latitudeLTF,         0,   1,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _miscPanel, _longitudeLTF,        0,   2,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _miscPanel, _daLTF,               0,   3,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _miscPanel, _mileLTF,             0,   4,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _miscPanel, _zdLTF,               0,   5,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _miscPanel, _vdatumLTF,           0,   6,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _miscPanel, _cbLTF,               0,   7,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _miscPanel, _levelLTF,            0,   8,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _miscPanel, _poolLTF,             0,   9,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _miscPanel, _porLTF,              0,  10,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _miscPanel, _tideLTF,             0,  11,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _miscPanel, _backwaterLTF,        0,  12,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _miscPanel, _rreviseLTF,          0,  13,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _miscPanel, _rsourceLTF,          0,  14,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _miscPanel, _responseTimeLTF,     0,  15,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _miscPanel, _thresholdRunoffLTF,  0,  16,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _miscPanel, _uhgDurLTF,           0,  17,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
    }
    
    private void initRemarkPanel()
    {
        _remarkLabel.setToolTipText( "Stores any type of documentation or commentary in a free text format" );
        ComponentHelper.addFrameComponent( _remarkPanel, _remarkLabel,         0,   0,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _remarkPanel, _remarkTextArea,      0,   1,    3,     3, 1, 1, GridBagConstraints.BOTH );
        
//        ComponentHelper.addFrameComponent( _remarkPanel, _remarkLTF,         0,   0,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
    }
    
    private void initButtonPanel()
    {
//                                                                              X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _buttonPanel, _saveAndCloseButton,   0,   0,   1,    1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _saveButton,           1,   0,   1,    1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _closeButton,          2,   0,   1,    1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _deleteButton,         3,   0,   1,    1, 1, 1, GridBagConstraints.NONE );
    }

    private void initFrameComponents()
    {  
//                                                                                      X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _riverCritSelectionCB,   0,   0,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _dialogContentPane, _floodCatPanel,          0,   1,    1,     1, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _procQCPanel,            1,   1,    1,     1, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _miscPanel,              2,   1,    1,     1, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _remarkPanel,            0,   2,    3,     4, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _buttonPanel,            0,   6,    3,     1, 1, 1, GridBagConstraints.BOTH );
    }
    
    private void deleteRiverCrit()
    {
        String riverCritSelectionCBString = (String) _riverCritSelectionCB.getSelectedItem();
        RaxRiverCrit raxRiverCrit = getRaxRiverCritFromLidPEVdateComboBoxString( riverCritSelectionCBString );

        
        if ( raxRiverCrit != null )
        {
            if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + raxRiverCrit.keyString(), "Delete River Crit" ) )
            {
                _dataMgr.deleteRaxRiverCrit( raxRiverCrit );
                _riverCritSelectionCB.removeAllItems();
                setupRiverCritSelectionComboBox();
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select a crest", "Delete River Crit" );
        }
    }
    
    private boolean saveRiverCrit()
    {
        RaxRiverCrit raxRiverCrit = new RaxRiverCrit();
        boolean saved = false;
        
        raxRiverCrit.setLid( _lidLTF.getTextFieldText() );
        raxRiverCrit.setPe( _peLTF.getTextFieldText() );
        raxRiverCrit.setValidTime( _converter.getLongDateValue( _validTimeLTF.getTextFieldText() ) );
        raxRiverCrit.setFis( _converter.getDoubleValue( _fisLTF.getTextFieldText() ) );
        raxRiverCrit.setFisF( _converter.getDoubleValue( _fisFLTF.getTextFieldText() ) );
        raxRiverCrit.setFisQ( _fisQLTF.getTextFieldText());
        raxRiverCrit.setAction( _converter.getDoubleValue( _actionLTF.getTextFieldText() ) );
        raxRiverCrit.setActionF( _converter.getDoubleValue( _actionFLTF.getTextFieldText() ) );
        raxRiverCrit.setActionQ( _actionQLTF.getTextFieldText() );
        raxRiverCrit.setAlert( _converter.getDoubleValue( _alertLTF.getTextFieldText() ) );
        raxRiverCrit.setAlertF( _converter.getDoubleValue( _alertFLTF.getTextFieldText() ) );
        raxRiverCrit.setAlertQ( _alertQLTF.getTextFieldText() );
        raxRiverCrit.setBank( _converter.getDoubleValue( _bankLTF.getTextFieldText() ) );
        raxRiverCrit.setBankF( _converter.getDoubleValue( _bankFLTF.getTextFieldText() ) );
        raxRiverCrit.setBankQ( _bankQLTF.getTextFieldText() );
        raxRiverCrit.setMinorFloodStage( _converter.getDoubleValue( _minorFloodStageLTF.getTextFieldText() ) );
        raxRiverCrit.setMinorFloodFlow( _converter.getDoubleValue( _minorFloodFlowLTF.getTextFieldText() ) );
        raxRiverCrit.setFloodQ( _floodQLTF.getTextFieldText() );
        raxRiverCrit.setModerateFloodStage( _converter.getDoubleValue( _moderateFloodStageLTF.getTextFieldText() ) );
        raxRiverCrit.setModerateFloodFlow( _converter.getDoubleValue( _moderateFloodFlowLTF.getTextFieldText() ) );
        raxRiverCrit.setModFloodQ( _modFloodQLTF.getTextFieldText() );
        raxRiverCrit.setMajorFloodStage( _converter.getDoubleValue( _majorFloodStageLTF.getTextFieldText() ) );
        raxRiverCrit.setMajorFloodFlow( _converter.getDoubleValue( _majorFloodFlowLTF.getTextFieldText() ) );
        raxRiverCrit.setMajFloodQ( _majFloodQLTF.getTextFieldText() );
        raxRiverCrit.setRecord( _converter.getDoubleValue( _recordLTF.getTextFieldText() ) );
        raxRiverCrit.setRecordF( _converter.getDoubleValue( _recordFLTF.getTextFieldText() ) );
        raxRiverCrit.setRecordQ( _recordQLTF.getTextFieldText() );

        raxRiverCrit.setLowScreen( _converter.getDoubleValue( _lowScreenLTF.getTextFieldText() ) );
        raxRiverCrit.setLowScreenF( _converter.getDoubleValue( _lowScreenFLTF.getTextFieldText() ) );
        raxRiverCrit.setLowScreenQ( _lowScreenQLTF.getTextFieldText() );
        raxRiverCrit.setHighScreen( _converter.getDoubleValue( _highScreenLTF.getTextFieldText() ) );
        raxRiverCrit.setHighScreenF( _converter.getDoubleValue( _highScreenFLTF.getTextFieldText() ) );
        raxRiverCrit.setHighScreenQ( _highScreenQLTF.getTextFieldText() );
        raxRiverCrit.setDamScreen( _converter.getDoubleValue( _damScreenLTF.getTextFieldText() ) );
        raxRiverCrit.setDamScreenF( _converter.getDoubleValue( _damScreenFLTF.getTextFieldText() ) );
        raxRiverCrit.setDamScreenQ( _damScreenQLTF.getTextFieldText() );
        raxRiverCrit.setSigRate( _converter.getDoubleValue( _sigRateLTF.getTextFieldText() ) );
        raxRiverCrit.setSigRateF( _converter.getDoubleValue( _sigRateFLTF.getTextFieldText() ) );
        raxRiverCrit.setSigRateQ( _sigRateQLTF.getTextFieldText() );
        raxRiverCrit.setSigRateT( _converter.getDoubleValue( _sigRateTLTF.getTextFieldText() ) );
        raxRiverCrit.setScreenRate( _converter.getDoubleValue( _screenRateLTF.getTextFieldText() ) );
        raxRiverCrit.setScreenRateF( _converter.getDoubleValue( _screenRateFLTF.getTextFieldText() ) );
        raxRiverCrit.setScreenRateQ( _screenRateQLTF.getTextFieldText() );
        raxRiverCrit.setScreenRateT( _converter.getDoubleValue( _screenRateTLTF.getTextFieldText() ) );

        raxRiverCrit.setStream( _streamLTF.getTextFieldText() );
        raxRiverCrit.setLatitude( _converter.getDoubleValue( _latitudeLTF.getTextFieldText() ) );
        raxRiverCrit.setLongitude( _converter.getDoubleValue( _longitudeLTF.getTextFieldText() ) );
        raxRiverCrit.setDa( _converter.getDoubleValue( _daLTF.getTextFieldText() ) );
        raxRiverCrit.setMile( _converter.getDoubleValue( _mileLTF.getTextFieldText() ) );
        raxRiverCrit.setZd( _converter.getDoubleValue( _zdLTF.getTextFieldText() ) );
        raxRiverCrit.setVdatum( _vdatumLTF.getTextFieldText() );
        raxRiverCrit.setCb( _converter.getDoubleValue( _cbLTF.getTextFieldText() ) );
        raxRiverCrit.setLevel( _levelLTF.getTextFieldText() );
        raxRiverCrit.setPool( _converter.getDoubleValue( _poolLTF.getTextFieldText() ) );
        raxRiverCrit.setPor( _porLTF.getTextFieldText() );
        raxRiverCrit.setTide( _tideLTF.getTextFieldText() );
        raxRiverCrit.setBackwater( _backwaterLTF.getTextFieldText() );
        
        raxRiverCrit.setRrevise( _converter.getLongDateValue( _rreviseLTF.getTextFieldText() ) );
        raxRiverCrit.setRsource( _rsourceLTF.getTextFieldText() );
        raxRiverCrit.setResponseTime( _converter.getDoubleValue( _responseTimeLTF.getTextFieldText() ) );
        raxRiverCrit.setThresholdRunoff( _converter.getDoubleValue( _thresholdRunoffLTF.getTextFieldText() ) );
        raxRiverCrit.setUhgdur( _converter.getIntValue( _uhgDurLTF.getTextFieldText() ) );
        raxRiverCrit.setRemark( _remarkTextArea.getText() );
        
        saved = _dataMgr.saveRaxRiverCrit( raxRiverCrit );
        
        _riverCritSelectionCB.removeAllItems();
        setupRiverCritSelectionComboBox();
        
        return saved;
    }
    
    private void populateFields()
    {
        _lidLTF.setTextField( _lid );

        String riverCritSelectionCBString = (String) _riverCritSelectionCB.getSelectedItem();
        RaxRiverCrit raxRiverCrit = getRaxRiverCritFromLidPEVdateComboBoxString( riverCritSelectionCBString );
        if ( raxRiverCrit != null )
        {
            _peLTF.setTextField( raxRiverCrit.getPe() );
            setDateFields( _validTimeLTF, raxRiverCrit.getValidTime() );
            setNumberFields( _fisLTF, raxRiverCrit.getFis() );
            setNumberFields( _fisFLTF, raxRiverCrit.getFisF() );
            _fisQLTF.setTextField( raxRiverCrit.getFisQ() );
            setNumberFields( _actionLTF, raxRiverCrit.getAction() );
            setNumberFields( _actionFLTF, raxRiverCrit.getActionF() );
            _actionQLTF.setTextField( raxRiverCrit.getActionQ() );
            setNumberFields( _alertLTF, raxRiverCrit.getAlert() );
            setNumberFields( _alertFLTF, raxRiverCrit.getAlertF() );
            _alertQLTF.setTextField( raxRiverCrit.getAlertQ() );
            setNumberFields( _bankLTF, raxRiverCrit.getBank() );
            setNumberFields( _bankFLTF, raxRiverCrit.getBankF() );
            _bankQLTF.setTextField( raxRiverCrit.getBankQ() );
            setNumberFields( _minorFloodStageLTF, raxRiverCrit.getMinorFloodStage() );
            setNumberFields( _minorFloodFlowLTF, raxRiverCrit.getMinorFloodFlow() );
            _floodQLTF.setTextField( raxRiverCrit.getFloodQ() );
            setNumberFields( _moderateFloodStageLTF, raxRiverCrit.getModerateFloodStage() );
            setNumberFields( _moderateFloodFlowLTF, raxRiverCrit.getModerateFloodFlow() );
            _modFloodQLTF.setTextField( raxRiverCrit.getModFloodQ() );
            setNumberFields( _majorFloodStageLTF, raxRiverCrit.getMajorFloodStage() );
            setNumberFields( _majorFloodFlowLTF, raxRiverCrit.getMajorFloodFlow() );
            _majFloodQLTF.setTextField( raxRiverCrit.getMajFloodQ() );
            setNumberFields( _recordLTF, raxRiverCrit.getRecord() );
            setNumberFields( _recordFLTF, raxRiverCrit.getRecordF() );
            _recordQLTF.setTextField( raxRiverCrit.getRecordQ() );

            setNumberFields( _lowScreenLTF, raxRiverCrit.getLowScreen() );
            setNumberFields( _lowScreenFLTF, raxRiverCrit.getLowScreenF() );
            _lowScreenQLTF.setTextField( raxRiverCrit.getLowScreenQ() );
            setNumberFields( _highScreenLTF, raxRiverCrit.getHighScreen() );
            setNumberFields( _highScreenFLTF, raxRiverCrit.getHighScreenF() );
            _highScreenQLTF.setTextField( raxRiverCrit.getHighScreenQ() );
            setNumberFields( _damScreenLTF, raxRiverCrit.getDamScreen() );
            setNumberFields( _damScreenFLTF, raxRiverCrit.getDamScreenF() );
            _damScreenQLTF.setTextField( raxRiverCrit.getDamScreenQ() );
            setNumberFields( _sigRateLTF, raxRiverCrit.getSigRate() );
            setNumberFields( _sigRateFLTF, raxRiverCrit.getSigRateF() );
            _sigRateQLTF.setTextField( raxRiverCrit.getSigRateQ() );
            setNumberFields( _sigRateTLTF, raxRiverCrit.getSigRateT() );
            setNumberFields( _screenRateLTF, raxRiverCrit.getScreenRate() );
            setNumberFields( _screenRateFLTF, raxRiverCrit.getScreenRateF() );
            _screenRateQLTF.setTextField( raxRiverCrit.getScreenRateQ() );
            setNumberFields( _screenRateTLTF, raxRiverCrit.getScreenRateT() );

            _streamLTF.setTextField( raxRiverCrit.getStream() );
            setNumberFields( _latitudeLTF, raxRiverCrit.getLatitude() );
            setNumberFields( _longitudeLTF, raxRiverCrit.getLongitude() );
            setNumberFields( _daLTF, raxRiverCrit.getDa() );
            setNumberFields( _mileLTF, raxRiverCrit.getMile() );
            setNumberFields( _zdLTF, raxRiverCrit.getZd() );
            _vdatumLTF.setTextField( raxRiverCrit.getVdatum() );
            setNumberFields( _cbLTF, raxRiverCrit.getCb() );
            _levelLTF.setTextField( raxRiverCrit.getLevel() );
            setNumberFields( _poolLTF, raxRiverCrit.getPool() );
            _porLTF.setTextField( raxRiverCrit.getPor() );
            _tideLTF.setTextField( raxRiverCrit.getTide() );
            _backwaterLTF.setTextField( raxRiverCrit.getBackwater() );
            setDateFields( _rreviseLTF, raxRiverCrit.getRrevise() );
            _rsourceLTF.setTextField( raxRiverCrit.getRsource() );
            setNumberFields( _responseTimeLTF, raxRiverCrit.getResponseTime() );
            setNumberFields( _thresholdRunoffLTF, raxRiverCrit.getThresholdRunoff() );
            setNumberFields( _uhgDurLTF, raxRiverCrit.getUhgdur() );
            _remarkTextArea.setText( raxRiverCrit.getRemark() );
        }
    }
    
    private void launchDateWindow( int clickedDateField )
    {
        _clickedDateField = clickedDateField;
        
        GregorianCalendar calendar = new GregorianCalendar();
        long dayInMillis = 60*60*24*1000;
        long dateLong = 0;
        String dateString = null;
        
        switch ( _clickedDateField )
        {
            case 0: dateString = _validTimeLTF.getTextFieldText(); break;
            case 1: dateString = _rreviseLTF.getTextFieldText(); break;
        }
        
        if ( ! dateString.equalsIgnoreCase( ""  ) )
        {
            dateLong = DbTimeHelper.getLongTimeFromDateString( dateString ) + dayInMillis;
        }
        else
        {
            dateLong = System.currentTimeMillis();
        }
        
        calendar.setTimeInMillis( dateLong );
        _hDateChooser.setDate( calendar );
        _hDateChooser.setModal( true );
        _hDateChooser.setVisible( true );
    }

    public void dateChosen( JDialog jDialog )
    {
        if ( _clickedDateField != -9999 )
        {
            Calendar calendar = _hDateChooser.getDate();
            String dateString = null;
            
            if ( _hDateChooser.isClearDate() )
            {
                dateString = "";
            }
            else
            {
                dateString = DateManager.getDateStringFromCalendar( calendar );
            }
            
            switch ( _clickedDateField )
            {
                case 0: _validTimeLTF.setTextField( dateString ); break;
                case 1: _rreviseLTF.setTextField( dateString ); break;
            }
            _clickedDateField = -9999;
        }
    }

    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();
        DateFieldMouseListener vdatumMouseListener = new DateFieldMouseListener( 0 );
        DateFieldMouseListener rreviseMouseListener = new DateFieldMouseListener( 1 );

        _riverCritSelectionCB.addActionListener( new RiverCritSelectionComboBoxListener() );
        _validTimeLTF.addTextFieldMouseListener( vdatumMouseListener );
        _rreviseLTF.addTextFieldMouseListener( rreviseMouseListener );
        
        _closeButton.addActionListener( windowCloser );
        _saveButton.addActionListener( new SaveButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
        addWindowListener( windowCloser );
    }
    
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteRiverCrit();
        }
    }
    
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveRiverCrit();
        }
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveRiverCrit() )
            {
                closeWindow();
            }
        }
    }
    
    private class DateFieldMouseListener implements MouseListener
    {
        private int __clickedDateField = -9999;
        
        public DateFieldMouseListener( int clickedDateField )
        {
            __clickedDateField = clickedDateField;
        }

        public void mouseClicked( MouseEvent e )
        {
        }

        public void mouseEntered( MouseEvent e )
        {
        }

        public void mouseExited( MouseEvent e )
        {
        }

        public void mousePressed( MouseEvent e )
        {
            launchDateWindow( __clickedDateField );
        }

        public void mouseReleased( MouseEvent e )
        {
        }
    }
        
    private class RiverCritSelectionComboBoxListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            populateFields();
        }
    }
    
    private void setNumberFields( LabeledTextField LTF, double doubleValue )
    {
        if ( DbTable.isNull( doubleValue ) )
        {
            LTF.setTextField( "" );
        }
        else
        {
            LTF.setTextField( Double.toString( doubleValue ) );
        }
    }

    private void setNumberFields( LabeledTextField LTF, int intValue )
    {
        if ( DbTable.isNull( intValue ) )
        {
            LTF.setTextField( "" );
        }
        else
        {
            LTF.setTextField( Integer.toString( intValue ) );
        }
    }
    
    private void setDateFields( LabeledTextField LTF, long dateValue )
    {
        if ( DbTable.isNull( dateValue ) )
        {
            LTF.setTextField( "" );
        }
        else
        {
            LTF.setTextFieldDate( dateValue );
        }
    }
    
    private class WindowCloserListener extends WindowAdapter implements ActionListener
    {
        public void windowClosing ( WindowEvent e )
        {
            closeWindow();
        }
        
        public void actionPerformed( ActionEvent e )
        {
            closeWindow();
        }
    }

    private void closeWindow()
    /********************
        Purpose: Exits the program gracefully 
     *********************/
    {
        this.dispose();
    }


    public static void main( String args[] )
    {
        JFrame frame = new JFrame();
        frame.setSize( new Dimension( 1024, 768 ) );
        RaxBaseDataMgr dataMgr = new RaxBaseDataMgr( "jdbc:postgresql://ax2:5432/adb_ob72krf?user=pguser" );
        
        String title = "LUDN8";
        String name = dataMgr.getRaxLocation( "LUDN8" ).getName();
        
        if ( name != null )
        {
            title += " - " + name;
        }
        

        RiverCritEditor riverCritEditorGUI = new RiverCritEditor( frame, dataMgr, "LUDN8", name );
        riverCritEditorGUI.displayGUI();
    }
}
