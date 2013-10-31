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
import java.util.ArrayList;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.raxbase.RaxBaseDataMgr;
import ohd.hseb.raxbase.model.RaxLocation;
import ohd.hseb.raxbase.model.SlopeProfile;
import ohd.hseb.raxbase.util.DateManager;
import ohd.hseb.raxbase.util.HDateSuperChooser;
import ohd.hseb.rfc.util.graphics.HDateChooserOwner;
import ohd.hseb.util.StringDataConverter;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledComboBox;
import ohd.hseb.util.gui.LabeledTextField;

public class SlopeProfileEditor extends JDialog implements HDateChooserOwner
{
    private Container _dialogContentPane = getContentPane();
    
    private RaxBaseDataMgr _dataMgr = null;

    private HDateSuperChooser _hDateChooser = new HDateSuperChooser( false );
    private int _clickedDateField = -9999;

    private LabeledComboBox _selectionLCB = new LabeledComboBox();

    private JPanel _dataPanel = new JPanel( new GridBagLayout() );
    private JPanel _distancePanel = new JPanel( new GridBagLayout() );
    private JPanel _elevationPanel = new JPanel( new GridBagLayout() );
    
    private StringDataConverter _converter = new StringDataConverter();

    private Map _selectionStringToSlopeProfileMap = new HashMap();
    private String _lid = null;
    
    private List _slopeProfileList = null;
    private SlopeProfile _selectedSlopeProfile = null;
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _deleteButton = new JButton( "Delete" );

    private LabeledTextField _lidLTF = new LabeledTextField( "LID:", "", "Location ID", 8 );
    private LabeledTextField _markerLTF = new LabeledTextField( "Marker:", "", "Marker used to measure distance to water in slope profile (eg. marker a)", 2 );
    private LabeledTextField _beginDateLTF = new LabeledTextField( "BeginDate:", "", "Begin Date", 12 );
    private LabeledTextField _endDateLTF = new LabeledTextField( "EndDate:", "", "End Date", 12 );
    
    private LabeledTextField _distance01LTF = new LabeledTextField( "Distance01:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance02LTF = new LabeledTextField( "Distance02:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance03LTF = new LabeledTextField( "Distance03:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance04LTF = new LabeledTextField( "Distance04:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance05LTF = new LabeledTextField( "Distance05:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance06LTF = new LabeledTextField( "Distance06:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance07LTF = new LabeledTextField( "Distance07:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance08LTF = new LabeledTextField( "Distance08:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance09LTF = new LabeledTextField( "Distance09:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance10LTF = new LabeledTextField( "Distance10:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance11LTF = new LabeledTextField( "Distance11:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance12LTF = new LabeledTextField( "Distance12:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance13LTF = new LabeledTextField( "Distance13:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance14LTF = new LabeledTextField( "Distance14:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance15LTF = new LabeledTextField( "Distance15:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance16LTF = new LabeledTextField( "Distance16:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance17LTF = new LabeledTextField( "Distance17:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance18LTF = new LabeledTextField( "Distance18:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance19LTF = new LabeledTextField( "Distance19:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance20LTF = new LabeledTextField( "Distance20:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance21LTF = new LabeledTextField( "Distance21:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance22LTF = new LabeledTextField( "Distance22:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance23LTF = new LabeledTextField( "Distance23:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance24LTF = new LabeledTextField( "Distance24:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance25LTF = new LabeledTextField( "Distance25:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance26LTF = new LabeledTextField( "Distance26:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance27LTF = new LabeledTextField( "Distance27:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance28LTF = new LabeledTextField( "Distance28:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance29LTF = new LabeledTextField( "Distance29:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );
    private LabeledTextField _distance30LTF = new LabeledTextField( "Distance30:", "", "Slope distances (ft and tenths of ft) measured from specified marker to the waters edge", 8 );

    private LabeledTextField _elevation01LTF = new LabeledTextField( "Elevation01:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation02LTF = new LabeledTextField( "Elevation02:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation03LTF = new LabeledTextField( "Elevation03:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation04LTF = new LabeledTextField( "Elevation04:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation05LTF = new LabeledTextField( "Elevation05:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation06LTF = new LabeledTextField( "Elevation06:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation07LTF = new LabeledTextField( "Elevation07:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation08LTF = new LabeledTextField( "Elevation08:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation09LTF = new LabeledTextField( "Elevation09:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation10LTF = new LabeledTextField( "Elevation10:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation11LTF = new LabeledTextField( "Elevation11:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation12LTF = new LabeledTextField( "Elevation12:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation13LTF = new LabeledTextField( "Elevation13:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation14LTF = new LabeledTextField( "Elevation14:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation15LTF = new LabeledTextField( "Elevation15:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation16LTF = new LabeledTextField( "Elevation16:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation17LTF = new LabeledTextField( "Elevation17:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation18LTF = new LabeledTextField( "Elevation18:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation19LTF = new LabeledTextField( "Elevation19:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation20LTF = new LabeledTextField( "Elevation20:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation21LTF = new LabeledTextField( "Elevation21:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation22LTF = new LabeledTextField( "Elevation22:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation23LTF = new LabeledTextField( "Elevation23:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation24LTF = new LabeledTextField( "Elevation24:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation25LTF = new LabeledTextField( "Elevation25:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation26LTF = new LabeledTextField( "Elevation26:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation27LTF = new LabeledTextField( "Elevation27:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation28LTF = new LabeledTextField( "Elevation28:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation29LTF = new LabeledTextField( "Elevation29:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );
    private LabeledTextField _elevation30LTF = new LabeledTextField( "Elevation30:", "", "Stage height corresponding to slope distance (distance01 - distance 30) (ft)", 8 );

    
    public SlopeProfileEditor( JFrame frame, RaxBaseDataMgr dataMgr, String lid, String title )
    {
        super( frame, "Slope Profile Editor - " + title, true );
        _dataMgr = dataMgr;
        _lid = lid;
    }

    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 600, 900 ) );
        initGUI();
    }

    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        updateSlopeProfileList();
        
        updateSelectionComboBox();
        initDataPanel();
        initButtonPanel();
        initFrameComponents();
        addListeners();
        populateDataPanel( _selectedSlopeProfile );
        pack();
        setVisible( true );
    }
    
    private void initButtonPanel()
    {
        _buttonPanel.setBorder( BorderFactory.createTitledBorder( "Database Controls" ) );
        
//                                                                              X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _buttonPanel, _saveAndCloseButton,   0,   0,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _saveButton,           1,   0,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _deleteButton,         2,   0,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _closeButton,          1,   1,    1,     1, 1, 1, GridBagConstraints.NONE );
    }
    
    private void initDataPanel()
    {
        _dataPanel.setBorder( BorderFactory.createTitledBorder( "Selected Item" ) );
        
        initDistancePanel();
        initElevationPanel();
        _beginDateLTF.setEditTextField( false );
        _endDateLTF.setEditTextField( false );
        _lidLTF.setEditTextField( false );
        _lidLTF.setTextField( _lid );
//                                                                        X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _dataPanel, _lidLTF,           0,   0,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _markerLTF,        0,   1,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _beginDateLTF,     0,   2,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _endDateLTF,       0,   3,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _distancePanel,    0,   4,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _elevationPanel,   1,   4,    1,     1, 1, 1, GridBagConstraints.NONE );

    }
    
    private void initElevationPanel()
    {
        _elevationPanel.setBorder( BorderFactory.createTitledBorder( "Elevation Values" ) );
        
//                                                                                  X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation01LTF,        0,   0,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation02LTF,        0,   1,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation03LTF,        0,   2,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation04LTF,        0,   3,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation05LTF,        0,   4,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation06LTF,        0,   5,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation07LTF,        0,   6,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation08LTF,        0,   7,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation09LTF,        0,   8,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation10LTF,        0,   9,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation11LTF,        0,  10,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation12LTF,        0,  11,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation13LTF,        0,  12,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation14LTF,        0,  13,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation15LTF,        0,  14,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation16LTF,        0,  15,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation17LTF,        0,  16,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation18LTF,        0,  17,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation19LTF,        0,  18,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation20LTF,        0,  19,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation21LTF,        0,  20,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation22LTF,        0,  21,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation23LTF,        0,  22,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation24LTF,        0,  23,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation25LTF,        0,  24,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation26LTF,        0,  25,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation27LTF,        0,  26,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation28LTF,        0,  27,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation29LTF,        0,  28,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _elevationPanel, _elevation30LTF,        0,  29,    1,     1, 1, 1, GridBagConstraints.NONE );
    }

    private void initDistancePanel()
    {
        _distancePanel.setBorder( BorderFactory.createTitledBorder( "Distance Values" ) );
        
//                                                                            X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _distancePanel, _distance01LTF,        0,   0,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance02LTF,        0,   1,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance03LTF,        0,   2,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance04LTF,        0,   3,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance05LTF,        0,   4,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance06LTF,        0,   5,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance07LTF,        0,   6,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance08LTF,        0,   7,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance09LTF,        0,   8,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance10LTF,        0,   9,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance11LTF,        0,  10,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance12LTF,        0,  11,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance13LTF,        0,  12,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance14LTF,        0,  13,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance15LTF,        0,  14,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance16LTF,        0,  15,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance17LTF,        0,  16,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance18LTF,        0,  17,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance19LTF,        0,  18,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance20LTF,        0,  19,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance21LTF,        0,  20,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance22LTF,        0,  21,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance23LTF,        0,  22,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance24LTF,        0,  23,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance25LTF,        0,  24,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance26LTF,        0,  25,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance27LTF,        0,  26,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance28LTF,        0,  27,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance29LTF,        0,  28,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _distancePanel, _distance30LTF,        0,  29,    1,     1, 1, 1, GridBagConstraints.NONE );
    }

    private void initFrameComponents()
    {
//                                                                                 X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _selectionLCB,      0,   0,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _dialogContentPane, _dataPanel,         0,   1,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _dialogContentPane, _buttonPanel,       0,   2,    2,     1, 1, 1, GridBagConstraints.NONE );
    }

    private void updateSelectionComboBox()
    {
        SlopeProfile slopeProfile = null;
        List selectionComboBoxStringList = new ArrayList();
        String dateString = null;
        
        _selectionStringToSlopeProfileMap.clear();
        _selectionLCB.getComboBox().removeAllItems();
        
        for ( int i = 0; i < _slopeProfileList.size(); i++ )
        {
            slopeProfile = (SlopeProfile) _slopeProfileList.get( i );
            selectionComboBoxStringList.add( getSelectedSlopeProfileString( slopeProfile ) );
            _selectionStringToSlopeProfileMap.put( getSelectedSlopeProfileString( slopeProfile ), slopeProfile );
        }
        
        if ( selectionComboBoxStringList.isEmpty() )
        {
            selectionComboBoxStringList.add( "No entries found" );
        }
        _selectionLCB.setComboBoxFromStringArray( selectionComboBoxStringList.toArray() );
        _selectionLCB.setLabel( "Select Average Entry:" );
        _selectionLCB.setLabelPreferredSize( new Dimension( 150, 15 ) );
        _selectionLCB.setComboBoxPreferredSize( new Dimension( 375, 15 ) );

        String selectedItem = _selectionLCB.getSelectedCBItem();
        
        _selectedSlopeProfile = (SlopeProfile) _selectionStringToSlopeProfileMap.get( selectedItem );
    }

    private String getSelectedSlopeProfileString( SlopeProfile slopeProfile )
    {
        return "LID = " + slopeProfile.getLid() + " | Marker = " + slopeProfile.getMarker() + " | BeginDate = " + DbTimeHelper.getDateStringFromLongTime( slopeProfile.getBegdate() );
    }


    private void updateSlopeProfileList()
    {
        _slopeProfileList = _dataMgr.getSlopeProfileList( _lid );
    }
    
    private void populateDataPanel( SlopeProfile e )
    {
        if ( e != null )
        {
            _lidLTF.setTextField( e.getLid() );
            _markerLTF.setTextField( e.getMarker() );
            _beginDateLTF.setTextField( _converter.getDateStringFromDateLong( e.getBegdate() ) );
            _endDateLTF.setTextField( _converter.getDateStringFromDateLong( e.getEnddate() ) );

            _distance01LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance01() ) );
            _distance02LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance02() ) );
            _distance03LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance03() ) );
            _distance04LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance04() ) );
            _distance05LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance05() ) );
            _distance06LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance06() ) );
            _distance07LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance07() ) );
            _distance08LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance08() ) );
            _distance09LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance09() ) );
            _distance10LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance10() ) );
            _distance11LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance11() ) );
            _distance12LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance12() ) );
            _distance13LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance13() ) );
            _distance14LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance14() ) );
            _distance15LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance15() ) );
            _distance16LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance16() ) );
            _distance17LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance17() ) );
            _distance18LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance18() ) );
            _distance19LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance19() ) );
            _distance20LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance20() ) );
            _distance21LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance21() ) );
            _distance22LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance22() ) );
            _distance23LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance23() ) );
            _distance24LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance24() ) );
            _distance25LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance25() ) );
            _distance26LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance26() ) );
            _distance27LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance27() ) );
            _distance28LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance28() ) );
            _distance29LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance29() ) );
            _distance30LTF.setTextField( _converter.getFormattedFloatValue( e.getDistance30() ) );

            _elevation01LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation01() ) );
            _elevation02LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation02() ) );
            _elevation03LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation03() ) );
            _elevation04LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation04() ) );
            _elevation05LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation05() ) );
            _elevation06LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation06() ) );
            _elevation07LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation07() ) );
            _elevation08LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation08() ) );
            _elevation09LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation09() ) );
            _elevation10LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation10() ) );
            _elevation11LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation11() ) );
            _elevation12LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation12() ) );
            _elevation13LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation13() ) );
            _elevation14LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation14() ) );
            _elevation15LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation15() ) );
            _elevation16LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation16() ) );
            _elevation17LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation17() ) );
            _elevation18LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation18() ) );
            _elevation19LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation19() ) );
            _elevation20LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation20() ) );
            _elevation21LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation21() ) );
            _elevation22LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation22() ) );
            _elevation23LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation23() ) );
            _elevation24LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation24() ) );
            _elevation25LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation25() ) );
            _elevation26LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation26() ) );
            _elevation27LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation27() ) );
            _elevation28LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation28() ) );
            _elevation29LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation29() ) );
            _elevation30LTF.setTextField( _converter.getFormattedFloatValue( e.getElevation30() ) );
            
        }
        _hDateChooser.addOwner( this );

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
            case 0: dateString = _beginDateLTF.getTextFieldText(); break;
            case 1: dateString = _endDateLTF.getTextFieldText(); break;
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
                case 0: _beginDateLTF.setTextField( dateString ); break;
                case 1: _endDateLTF.setTextField( dateString ); break;
            }
            _clickedDateField = -9999;
        }
    }
    
    private void deleteSlopeProfile()
    {
        String selectedItem = _selectionLCB.getSelectedCBItem();
        
        SlopeProfile slopeProfile = (SlopeProfile) _selectionStringToSlopeProfileMap.get( selectedItem );

        if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + slopeProfile.keyString(), "Delete SlopeProfile" ) )
        {
            boolean success = _dataMgr.deleteSlopeProfileFromDatabase( slopeProfile );
            if ( ! success )
            {
                DialogHelper.displayErrorDialog( this, "Unable to delete SlopeProfile entry", "Delete SlopeProfile Error" );
            }
            updateSlopeProfileList();
            updateSelectionComboBox();
        }
    }
    
    private boolean saveSlopeProfile()
    {
        boolean saved = false;
        SlopeProfile slopeProfile = new SlopeProfile();
        
        slopeProfile.setLid( _lidLTF.getTextFieldText() );
        slopeProfile.setMarker( _markerLTF.getTextFieldText() );
        slopeProfile.setBegdate( _converter.getLongDateValue( _beginDateLTF.getTextFieldText() ) );
        slopeProfile.setEnddate( _converter.getLongDateValue( _endDateLTF.getTextFieldText() ) );

        slopeProfile.setDistance01( _converter.getFloatValue( _distance01LTF.getTextFieldText() ) );
        slopeProfile.setDistance02( _converter.getFloatValue( _distance02LTF.getTextFieldText() ) );
        slopeProfile.setDistance03( _converter.getFloatValue( _distance03LTF.getTextFieldText() ) );
        slopeProfile.setDistance04( _converter.getFloatValue( _distance04LTF.getTextFieldText() ) );
        slopeProfile.setDistance05( _converter.getFloatValue( _distance05LTF.getTextFieldText() ) );
        slopeProfile.setDistance06( _converter.getFloatValue( _distance06LTF.getTextFieldText() ) );
        slopeProfile.setDistance07( _converter.getFloatValue( _distance07LTF.getTextFieldText() ) );
        slopeProfile.setDistance08( _converter.getFloatValue( _distance08LTF.getTextFieldText() ) );
        slopeProfile.setDistance09( _converter.getFloatValue( _distance09LTF.getTextFieldText() ) );
        slopeProfile.setDistance10( _converter.getFloatValue( _distance10LTF.getTextFieldText() ) );
        slopeProfile.setDistance11( _converter.getFloatValue( _distance11LTF.getTextFieldText() ) );
        slopeProfile.setDistance12( _converter.getFloatValue( _distance12LTF.getTextFieldText() ) );
        slopeProfile.setDistance13( _converter.getFloatValue( _distance13LTF.getTextFieldText() ) );
        slopeProfile.setDistance14( _converter.getFloatValue( _distance14LTF.getTextFieldText() ) );
        slopeProfile.setDistance15( _converter.getFloatValue( _distance15LTF.getTextFieldText() ) );
        slopeProfile.setDistance16( _converter.getFloatValue( _distance16LTF.getTextFieldText() ) );
        slopeProfile.setDistance17( _converter.getFloatValue( _distance17LTF.getTextFieldText() ) );
        slopeProfile.setDistance18( _converter.getFloatValue( _distance18LTF.getTextFieldText() ) );
        slopeProfile.setDistance19( _converter.getFloatValue( _distance19LTF.getTextFieldText() ) );
        slopeProfile.setDistance20( _converter.getFloatValue( _distance20LTF.getTextFieldText() ) );
        slopeProfile.setDistance21( _converter.getFloatValue( _distance21LTF.getTextFieldText() ) );
        slopeProfile.setDistance22( _converter.getFloatValue( _distance22LTF.getTextFieldText() ) );
        slopeProfile.setDistance23( _converter.getFloatValue( _distance23LTF.getTextFieldText() ) );
        slopeProfile.setDistance24( _converter.getFloatValue( _distance24LTF.getTextFieldText() ) );
        slopeProfile.setDistance25( _converter.getFloatValue( _distance25LTF.getTextFieldText() ) );
        slopeProfile.setDistance26( _converter.getFloatValue( _distance26LTF.getTextFieldText() ) );
        slopeProfile.setDistance27( _converter.getFloatValue( _distance27LTF.getTextFieldText() ) );
        slopeProfile.setDistance28( _converter.getFloatValue( _distance28LTF.getTextFieldText() ) );
        slopeProfile.setDistance29( _converter.getFloatValue( _distance29LTF.getTextFieldText() ) );
        slopeProfile.setDistance30( _converter.getFloatValue( _distance30LTF.getTextFieldText() ) );

        slopeProfile.setElevation01( _converter.getFloatValue( _elevation01LTF.getTextFieldText() ) );
        slopeProfile.setElevation02( _converter.getFloatValue( _elevation02LTF.getTextFieldText() ) );
        slopeProfile.setElevation03( _converter.getFloatValue( _elevation03LTF.getTextFieldText() ) );
        slopeProfile.setElevation04( _converter.getFloatValue( _elevation04LTF.getTextFieldText() ) );
        slopeProfile.setElevation05( _converter.getFloatValue( _elevation05LTF.getTextFieldText() ) );
        slopeProfile.setElevation06( _converter.getFloatValue( _elevation06LTF.getTextFieldText() ) );
        slopeProfile.setElevation07( _converter.getFloatValue( _elevation07LTF.getTextFieldText() ) );
        slopeProfile.setElevation08( _converter.getFloatValue( _elevation08LTF.getTextFieldText() ) );
        slopeProfile.setElevation09( _converter.getFloatValue( _elevation09LTF.getTextFieldText() ) );
        slopeProfile.setElevation10( _converter.getFloatValue( _elevation10LTF.getTextFieldText() ) );
        slopeProfile.setElevation11( _converter.getFloatValue( _elevation11LTF.getTextFieldText() ) );
        slopeProfile.setElevation12( _converter.getFloatValue( _elevation12LTF.getTextFieldText() ) );
        slopeProfile.setElevation13( _converter.getFloatValue( _elevation13LTF.getTextFieldText() ) );
        slopeProfile.setElevation14( _converter.getFloatValue( _elevation14LTF.getTextFieldText() ) );
        slopeProfile.setElevation15( _converter.getFloatValue( _elevation15LTF.getTextFieldText() ) );
        slopeProfile.setElevation16( _converter.getFloatValue( _elevation16LTF.getTextFieldText() ) );
        slopeProfile.setElevation17( _converter.getFloatValue( _elevation17LTF.getTextFieldText() ) );
        slopeProfile.setElevation18( _converter.getFloatValue( _elevation18LTF.getTextFieldText() ) );
        slopeProfile.setElevation19( _converter.getFloatValue( _elevation19LTF.getTextFieldText() ) );
        slopeProfile.setElevation20( _converter.getFloatValue( _elevation20LTF.getTextFieldText() ) );
        slopeProfile.setElevation21( _converter.getFloatValue( _elevation21LTF.getTextFieldText() ) );
        slopeProfile.setElevation22( _converter.getFloatValue( _elevation22LTF.getTextFieldText() ) );
        slopeProfile.setElevation23( _converter.getFloatValue( _elevation23LTF.getTextFieldText() ) );
        slopeProfile.setElevation24( _converter.getFloatValue( _elevation24LTF.getTextFieldText() ) );
        slopeProfile.setElevation25( _converter.getFloatValue( _elevation25LTF.getTextFieldText() ) );
        slopeProfile.setElevation26( _converter.getFloatValue( _elevation26LTF.getTextFieldText() ) );
        slopeProfile.setElevation27( _converter.getFloatValue( _elevation27LTF.getTextFieldText() ) );
        slopeProfile.setElevation28( _converter.getFloatValue( _elevation28LTF.getTextFieldText() ) );
        slopeProfile.setElevation29( _converter.getFloatValue( _elevation29LTF.getTextFieldText() ) );
        slopeProfile.setElevation30( _converter.getFloatValue( _elevation30LTF.getTextFieldText() ) );

        saved = _dataMgr.saveSlopeProfile( slopeProfile );
        updateSlopeProfileList();
        updateSelectionComboBox();
        
        return saved;
    }
    

    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();
        DateFieldMouseListener beginDateMouseListener = new DateFieldMouseListener( 0 );
        DateFieldMouseListener endDateMouseListener = new DateFieldMouseListener( 1 );
        
        _selectionLCB.addComboBoxActionListener( new ItemSelectionListener() );

        _beginDateLTF.addTextFieldMouseListener( beginDateMouseListener );
        _endDateLTF.addTextFieldMouseListener( endDateMouseListener );
        _closeButton.addActionListener( windowCloser );
        _saveButton.addActionListener( new SaveButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveSlopeProfile() )
            {
                closeWindow();
            }
            else
            {
                DialogHelper.displayErrorDialog( SlopeProfileEditor.this, "Unable to save the SlopeProfile entry", "Error Saving SlopeProfile" );
            }
        }
    }
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteSlopeProfile();
        }
    }

    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveSlopeProfile();
        }
    }
    
    private class DateFieldMouseListener implements MouseListener
    {
        private int __clickedDateField = -9999;

        public DateFieldMouseListener( int clickedDateField )
        {
            __clickedDateField = clickedDateField;
        }
        public void mouseClicked( MouseEvent e ){}
        public void mouseEntered( MouseEvent e ){}
        public void mouseReleased( MouseEvent e ){}
        public void mouseExited( MouseEvent e ){}
        public void mousePressed( MouseEvent e )
        {
            launchDateWindow( __clickedDateField );
        }
    }

    private class ItemSelectionListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e)
        {
            String selectedItem = _selectionLCB.getSelectedCBItem();
            
            SlopeProfile slopeProfile = (SlopeProfile) _selectionStringToSlopeProfileMap.get( selectedItem );
            
            if ( slopeProfile != null )
            {
                populateDataPanel( slopeProfile );
            }
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

        RaxBaseDataMgr dataMgr = new RaxBaseDataMgr( "jdbc:postgresql://ax2:5432/adb_ob82krf?user=pguser" );
        String name = null;
        
        String title = "ATLA2";
        RaxLocation location = dataMgr.getRaxLocation( title );
        if ( location != null )
        {
            name = location.getName();
        }
        
        if ( name != null )
        {
            title += " - " + name;
        }
        

        SlopeProfileEditor slopeProfileEditor = new SlopeProfileEditor( frame, dataMgr, "ACTL2", title );
        slopeProfileEditor.displayGUI();
    }

}
