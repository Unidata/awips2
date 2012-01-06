package ohd.hseb.raxbase.gui;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.BorderFactory;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import ohd.hseb.db.DbTable;
import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.raxbase.RaxBaseDataMgr;
import ohd.hseb.raxbase.model.RaxIngestFilter;
import ohd.hseb.raxbase.model.ShefDuration;
import ohd.hseb.raxbase.model.ShefExtremum;
import ohd.hseb.raxbase.model.ShefPE;
import ohd.hseb.raxbase.model.ShefTS;
import ohd.hseb.raxbase.table.ArcBaseIngestFilterJTableRowData;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class IngestFilterEditor extends JDialog implements PropertyChangeListener
{
    private Container _dialogContentPane = getContentPane();

    private RaxBaseDataMgr _dataMgr = null;

    private Map _raxIFRowDataToRaxIFMap = new HashMap();
    
    private Map _peCBStringToPeMap = new HashMap();
    private Map _durCBStringToDurMap = new HashMap();
    private Map _durCBStringToIDurMap = new HashMap();
    private Map _tsCBStringToTSMap = new HashMap();
    private Map _extremumCBStringToExtremumMap = new HashMap();
    
    private JPanel _ingestFilterListPanel = new JPanel( new GridBagLayout() );
    
    private StatusBar _statusBar = new StatusBar();

//  JTable variables
    private JTableManager _IFETableManager = null;
    private List _IFEColumnDescriptorList = null;
    private List _allRowDataList = null;
    private List _filteredRowDataList = null; //Filtered row data entries
    private JScrollPane _tableScrollPane = null;
    
    private JPanel _dataInputPanel = new JPanel( new GridBagLayout() );
    private JLabel _lidLabel = new JLabel( "Location:" );
    private JTextField _lidTextField = new JTextField();
    private JLabel _peLabel = new JLabel( "Physical Element:" );
    private JComboBox _peComboBox = new JComboBox();
    private JLabel _durationLabel = new JLabel( "Duration:" );
    private JComboBox _durationComboBox = new JComboBox();
    private JLabel _typeSourceLabel = new JLabel( "Type Source:" );
    private JComboBox _typeSourceComboBox = new JComboBox();
    private JLabel _extremumLabel = new JLabel( "Extremum:" );
    private JComboBox _extremumComboBox = new JComboBox();
    private JLabel _rankLabel = new JLabel( "Type Source Rank:" );
    private JComboBox _rankComboBox = new JComboBox();
    private JLabel _masterLabel = new JLabel( "Master Switch:" );
    private JCheckBox _masterCheckBox = new JCheckBox();
    private JLabel _ofsLabel = new JLabel( "OFS Input Switch" );
    private JCheckBox _ofsCheckBox = new JCheckBox();
    private JLabel _mpeInputLabel = new JLabel( "MPE Input Switch:" );
    private JCheckBox _mpeInputCheckBox = new JCheckBox();
    private JLabel _detailLabel = new JLabel( "Detail:" );
    private JTextField _detailTextField = new JTextField();
    private JLabel _newReportLabel = new JLabel( "New Report:" );
    private JCheckBox _newReportCheckBox = new JCheckBox();
    private JLabel _activeLabel = new JLabel( "Active:" );
    private JCheckBox _activeCheckBox = new JCheckBox();
    private JLabel _obsTimeLabel = new JLabel( "ObsTime:" );
    private JTextField _obsTimeTextField = new JTextField();
    private JLabel _ownAgencyLabel = new JLabel( "Owner Agency:" );
    private JTextField _ownAgencyTextField = new JTextField();
    private JLabel _ownLocationLabel = new JLabel( "Owner Location:" );
    private JTextField _ownLocationTextField = new JTextField();
    
    private List _peCBStringList = new ArrayList();
    private List _tsCBStringList = new ArrayList();
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveButton = new JButton( "Save to Database" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _clearButton = new JButton( "Clear" );
    private JButton _deleteButton = new JButton( "Delete Highlighted Entry" );
    
    private JPanel _filterPanel = new JPanel( new GridBagLayout() );
    private JLabel _filterByLabel = new JLabel( "Filter By:" );
    private JCheckBox _lidFilterCheckBox = new JCheckBox();
    private JCheckBox _tsFilterCheckBox = new JCheckBox();
    private JCheckBox _peFilterCheckBox = new JCheckBox();
    private JCheckBox _switchesFilterCheckBox = new JCheckBox();
    private JLabel _lidFilterPanelLabel = new JLabel( "Location:" );
    private JLabel _peFilterPanelLabel = new JLabel( "Physical Element:" );
    private JLabel _tsFilterPanelLabel = new JLabel( "Type Source:" );
    private JLabel _switchesFilterPanelLabel = new JLabel( "Switches:" );
    private JTextField _lidFilterTextField = new JTextField();
    private JList _peJList = null;
    private JScrollPane _peScrollPane = new JScrollPane();
    private DefaultListModel _peListModel = new DefaultListModel();
    private JComboBox _tsFilterComboBox = new JComboBox();
    private JCheckBox _masterFilterCB = new JCheckBox();
    private JCheckBox _ofsFilterCB = new JCheckBox();
    private JCheckBox _mpeFilterCB = new JCheckBox();
    
    public IngestFilterEditor( JFrame frame, RaxBaseDataMgr dataMgr )
    {
        super( frame, "Ingest Filter Editor", true );
        _dataMgr = dataMgr;
    }
    
    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 1100, 825 ) );
        initData();
    }
    
    private void initData()
    {
        _dataMgr.initIngestFilterRowData( this );
    }
    
    public void propertyChange( PropertyChangeEvent evt )
    {
        initGUI();
    }

    public void initGUI()
    {
        setLayout( new GridBagLayout() );
        _raxIFRowDataToRaxIFMap = _dataMgr.getRaxIngestFilterRowDataToRaxIngestFilterMap();
        initJTable();
        initFrameComponenets();
        initComboBoxes();
        initDataInputPanel();
        initButtonPanel();
        initFilterPanel();
        _statusBar.setMessage( _allRowDataList.size() + " entries"  );
        addListeners();
        pack();
        setVisible( true );
    }
    
    
    private void initButtonPanel()
    {
        _buttonPanel.setBorder( BorderFactory.createTitledBorder( "IngestFilter Database Controls" ) );

//                                                                             X,    Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _buttonPanel, _saveButton,          0,    0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _closeButton,         1,    0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _clearButton,         2,    0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _deleteButton,        3,    0,    1,     1, GridBagConstraints.NONE );
    }
    
    private void initFilterPanel()
    {
        JLabel horizontalLabel = new JLabel( "---------------------------" );

        _filterPanel.setBorder( BorderFactory.createTitledBorder( "Filter Parameters" ) );

        _lidFilterCheckBox.setText( "Location" );
        _tsFilterCheckBox.setText( "Type Source" );
        _peFilterCheckBox.setText( "Physical Element" );
        _switchesFilterCheckBox.setText( "Switches" );
        _masterFilterCB.setText( "Master" );
        _ofsFilterCB.setText( "OFS" );
        _mpeFilterCB.setText( "MPE" );

        _lidFilterTextField.setColumns( 9 );
        
        setupPEJlist();
//        _peJList.setPreferredSize( new Dimension( 100, 100 )  );
        _peJList.setSelectionMode( ListSelectionModel.MULTIPLE_INTERVAL_SELECTION );
        _peScrollPane = new JScrollPane( _peJList );
        
        _tsFilterComboBox = new JComboBox( _tsCBStringList.toArray() );
        
        activateLIDFilter( false );
        activatePEFilter( false );
        activateTSFilter( false );
        activateSwitchesFilter( false );
        
        JPanel switchFilterPanel = new JPanel( new GridBagLayout() );
        
//                                                                                   X,    Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _filterPanel, _filterByLabel,             0,    0,    2,     2, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _filterPanel, _lidFilterCheckBox,         2,    0,    2,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _filterPanel, _tsFilterCheckBox,          4,    0,    2,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _filterPanel, _peFilterCheckBox,          2,    1,    2,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _filterPanel, _switchesFilterCheckBox,    4,    1,    2,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _filterPanel, horizontalLabel,            0,    2,    6,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _filterPanel, _lidFilterPanelLabel,       0,    3,    2,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _filterPanel, _lidFilterTextField,        2,    3,    2,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _filterPanel, _peFilterPanelLabel,        0,    4,    2,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _filterPanel, _peScrollPane,              2,    4,    2,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _filterPanel, _tsFilterPanelLabel,        0,    5,    2,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _filterPanel, _tsFilterComboBox,          2,    5,    2,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _filterPanel, switchFilterPanel,          0,    6,    4,     1, GridBagConstraints.NONE );

        ComponentHelper.addPanelComponent( switchFilterPanel, _switchesFilterPanelLabel,  0,    0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( switchFilterPanel, _masterFilterCB,            1,    0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( switchFilterPanel, _ofsFilterCB,               2,    0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( switchFilterPanel, _mpeFilterCB,               3,    0,    1,     1, GridBagConstraints.NONE );
        
    }
    
    private void activateLIDFilter( boolean active )
    {
        if ( active )
        {
            _lidFilterPanelLabel.setEnabled( true );
            _lidFilterTextField.setEnabled( true );
        }
        else
        {
            _lidFilterPanelLabel.setEnabled( false );
            _lidFilterTextField.setEnabled( false );
            _lidFilterTextField.setText( "" );
        }
    }
    
    private void activatePEFilter( boolean active )
    {
        if ( active )
        {
            _peFilterPanelLabel.setEnabled( true );
            _peScrollPane.setEnabled( true );
            _peJList.setEnabled( true );
        }
        else
        {
            _peFilterPanelLabel.setEnabled( false );
            _peScrollPane.setEnabled( false );
            _peJList.setEnabled( false );
        }
    }
    
    private void activateTSFilter( boolean active )
    {
        if ( active )
        {
            _tsFilterPanelLabel.setEnabled( true );
            _tsFilterComboBox.setEnabled( true );
        }
        else
        {
            _tsFilterPanelLabel.setEnabled( false );
            _tsFilterComboBox.setEnabled( false );
            _tsFilterComboBox.setSelectedIndex( 0 );
        }
    }
    
    private void activateSwitchesFilter( boolean active )
    {
        if ( active )
        {
            _switchesFilterPanelLabel.setEnabled( true );
            _masterFilterCB.setEnabled( true );
            _ofsFilterCB.setEnabled( true );
            _mpeFilterCB.setEnabled( true );
        }
        else
        {
            _switchesFilterPanelLabel.setEnabled( false );
            _masterFilterCB.setEnabled( false );
            _masterFilterCB.setSelected( false );
            _ofsFilterCB.setEnabled( false );
            _ofsFilterCB.setSelected( false );
            _mpeFilterCB.setEnabled( false );
            _mpeFilterCB.setSelected( false );
        }
    }
    
    private void setupPEJlist()
    {
        String peString = null;
        for ( int i = 0; i < _peCBStringList.size(); i++ )
        {
            peString = (String) _peCBStringList.get( i );
            _peListModel.addElement( peString );
        }
        
        _peJList = new JList( _peListModel );
    }
    
    private void initComboBoxes()
    {
        List shefPeList = _dataMgr.getShefPeList();
        List shefDurationList = _dataMgr.getShefDurationList();
        List shefTypeSourceList = _dataMgr.getShefTSList();
        List shefExtremumList = _dataMgr.getShefExtremumList();
        
        List shefPeStringList = new ArrayList();
        List shefDurationStringList = new ArrayList();
        List shefTypeSourceStringList = new ArrayList();
        List shefExtremumStringList = new ArrayList();
        String[] tsRankStringArray = { "", "1st", "2nd", "3rd", "4th", "5th" };
        
        shefPeStringList.add( "" );
        for ( int i = 0; i < shefPeList.size(); i++ )
        {
            ShefPE shefPE = (ShefPE) shefPeList.get( i );
            shefPeStringList.add( getShefPEComboBoxString( shefPE ) );
            _peCBStringList.add( getShefPEComboBoxString( shefPE ) );
            _peCBStringToPeMap.put( getShefPEComboBoxString( shefPE ), shefPE.getPe() );
        }
        
        shefDurationStringList.add( "" );
        for ( int i = 0; i < shefDurationList.size(); i++ )
        {
            ShefDuration shefDur = (ShefDuration) shefDurationList.get( i );
            shefDurationStringList.add( getShefDurationComboBoxString( shefDur ) );
            _durCBStringToDurMap.put( getShefDurationComboBoxString( shefDur ), shefDur.getDuration() );
            _durCBStringToIDurMap.put( getShefDurationComboBoxString( shefDur ), shefDur.getIduration() );
        }
        
        shefTypeSourceStringList.add( "" );
        for ( int i = 0; i < shefTypeSourceList.size(); i++ )
        {
            ShefTS shefTS = (ShefTS) shefTypeSourceList.get( i );
            shefTypeSourceStringList.add( getShefTSString( shefTS ) );
            _tsCBStringList.add( getShefTSString( shefTS ) );
            _tsCBStringToTSMap.put( getShefTSString( shefTS ), shefTS.getTs() );
        }
        
        shefExtremumStringList.add( "" );
        for ( int i = 0; i < shefExtremumList.size(); i++ )
        {
            ShefExtremum shefEx = (ShefExtremum) shefExtremumList.get( i );
            shefExtremumStringList.add( getShefExtremumComboBoxString(shefEx ) );
            _extremumCBStringToExtremumMap.put( getShefExtremumComboBoxString( shefEx ), shefEx.getExtremum() );
        }
        
        _peComboBox = new JComboBox( shefPeStringList.toArray() );
        _durationComboBox = new JComboBox( shefDurationStringList.toArray() );
        _typeSourceComboBox = new JComboBox( shefTypeSourceStringList.toArray() );
        _extremumComboBox = new JComboBox( shefExtremumStringList.toArray() );
        
        _rankComboBox = new JComboBox( tsRankStringArray );
    }
    
    private String getShefExtremumComboBoxString( ShefExtremum shefEx )
    {
        String shefExtremumString = shefEx.getName() + " (" + shefEx.getExtremum() + ")";
        
        return shefExtremumString;
    }
    
    private String getShefTSString( ShefTS shefTS )
    {
        String shefTSString = shefTS.getName() + " (" + shefTS.getTs() + ")";
        
        return shefTSString;
    }
    
    private String getShefPEComboBoxString( ShefPE shefPE )
    {
        String shefPEString = shefPE.getPe() + " " + shefPE.getName();
        
        return shefPEString;
    }
    
    private String getShefDurationComboBoxString( ShefDuration shefDuration )
    {
        String shefDurationString = shefDuration.getName() + " ( " + shefDuration.getIduration() + "/" + shefDuration.getDuration() + " )";

        return shefDurationString;
    }
    
    private void initDataInputPanel()
    {
        GridBagConstraints gbLabelConstraints = new GridBagConstraints();
        GridBagConstraints gbTFConstraints = new GridBagConstraints();
        _dataInputPanel.setBorder( BorderFactory.createTitledBorder( "Selected Item" ) );

        gbLabelConstraints.anchor = GridBagConstraints.EAST;
        gbLabelConstraints.weightx = 1;
        gbLabelConstraints.weighty = 0;
        
        gbTFConstraints.anchor = GridBagConstraints.WEST;
        gbTFConstraints.weightx = 1;
        gbTFConstraints.weighty = 0;

        _lidTextField.setColumns( 8 );
        _detailTextField.setColumns( 45 );
        _obsTimeTextField.setColumns( 20 );
        _ownAgencyTextField.setColumns( 7 );
        _ownLocationTextField.setColumns( 4 );
        
//                                                                                                  X,    Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _dataInputPanel, _lidLabel, gbLabelConstraints,          0,    0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _lidTextField, gbTFConstraints,         1,    0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _peLabel, gbLabelConstraints,           0,    1,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _peComboBox, gbTFConstraints,           1,    1,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _durationLabel, gbLabelConstraints,     0,    2,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _durationComboBox, gbTFConstraints,     1,    2,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _typeSourceLabel, gbLabelConstraints,   0,    3,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _typeSourceComboBox, gbTFConstraints,   1,    3,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _extremumLabel, gbLabelConstraints,     0,    4,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _extremumComboBox, gbTFConstraints,     1,    4,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _rankLabel, gbLabelConstraints,         0,    5,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _rankComboBox, gbTFConstraints,         1,    5,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _masterLabel, gbLabelConstraints,       0,    6,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _masterCheckBox, gbTFConstraints,       1,    6,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _ofsLabel, gbLabelConstraints,          0,    7,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _ofsCheckBox, gbTFConstraints,          1,    7,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _mpeInputLabel, gbLabelConstraints,     0,    8,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _mpeInputCheckBox, gbTFConstraints,     1,    8,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _detailLabel, gbLabelConstraints,       0,    9,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _detailTextField, gbTFConstraints,      1,    9,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _newReportLabel, gbLabelConstraints,    0,   10,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _newReportCheckBox, gbTFConstraints,    1,   10,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _activeLabel, gbLabelConstraints,       0,   11,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _activeCheckBox, gbTFConstraints,       1,   11,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _obsTimeLabel, gbLabelConstraints,      0,   12,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _obsTimeTextField, gbTFConstraints,     1,   12,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _ownAgencyLabel, gbLabelConstraints,    0,   13,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _ownAgencyTextField, gbTFConstraints,   1,   13,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _ownLocationLabel, gbLabelConstraints,  0,   14,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataInputPanel, _ownLocationTextField, gbTFConstraints, 1,   14,    1,     1, GridBagConstraints.NONE );
    }
    
    private void initFrameComponenets()
    {
        _dialogContentPane.setLayout( new GridBagLayout() );
        JPanel horizontalPanel = new JPanel();
        horizontalPanel.setPreferredSize(new Dimension( 50, 50 ) );
//                                                                                  X,    Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _filterPanel,        2,    0,    2,    10, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _dataInputPanel,     0,   11,    4,     1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _buttonPanel,        0,   12,    4,     1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, horizontalPanel,     0,   13,    4,     1, 1, 0, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _dialogContentPane, _statusBar,          0,   14,    4,     1, GridBagConstraints.BOTH );
    }

    /**
     * Initialize the ArcBase JTable
     *
     * @return None
     *
     */    
    private void initJTable() 
    {
        GridBagConstraints mainGbc = new GridBagConstraints();

        mainGbc.fill = GridBagConstraints.BOTH;
        mainGbc.weightx = 1;
        mainGbc.weighty = 1;

        _allRowDataList = _dataMgr.getIngestFilterRowDataList();
        setIFEColumnDescriptorList();
        _IFETableManager = new ComplexJTableManager( _IFEColumnDescriptorList, _allRowDataList, ListSelectionModel.SINGLE_SELECTION );
        String columnsSelected[] = _IFETableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _IFETableManager.setDisplayableColumns( columnsSelected, true, true );
        _IFETableManager.setPreferredSizeForJScrollPane( new Dimension( 550, 300 ) );
        _tableScrollPane = _IFETableManager.getJScrollPane();

        JPanel tableScrollPanePanel = new JPanel( new GridLayout( 1, 1 ) ); 
        tableScrollPanePanel.add( _tableScrollPane );
        tableScrollPanePanel.setPreferredSize( new Dimension( 555,310 ) );
        tableScrollPanePanel.setBorder( BorderFactory.createTitledBorder( "Ingest Filter Contents for Locations" ) );

//                                                                                           X,  Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, tableScrollPanePanel,         0,  0,    2,     10, 1, 1, GridBagConstraints.BOTH );

    }

    private void displayFilteredIngestFilterEntries()
    {
        _filteredRowDataList = filterRowData();

        _statusBar.setMessage( _filteredRowDataList.size() + " entries"  );
//        System.out.println( "FilteredRowDataList has " + _filteredRowDataList.size() + " records" );
        
        _IFETableManager.setChangedAllRowDataList( _filteredRowDataList );
        
        _IFETableManager.refreshDisplay();

    }
    
    
    private List filterRowData()
    {
        List filteredRowDataList = new ArrayList();

        if ( ( _lidFilterCheckBox.isSelected() ) ||
                ( _tsFilterCheckBox.isSelected() ) ||
                ( _peFilterCheckBox.isSelected() ) ||
                ( _switchesFilterCheckBox.isSelected() ) )
        {
            ArcBaseIngestFilterJTableRowData rowData = null;
            ArcBaseIngestFilterJTableRowData addRowData = null;
            
            for ( int i = 0; i < _allRowDataList.size(); i++ )
            {
                rowData = (ArcBaseIngestFilterJTableRowData) _allRowDataList.get( i );
                
                if ( ( passFilterByLid( rowData ) ) &&
                     ( passFilterByPE( rowData ) ) &&
                     ( passFilterByTS( rowData ) ) &&
                     ( passFilterBySwitches( rowData ) ) )
                {
                    filteredRowDataList.add( rowData );
                }
            }
        }
        else
        {
            return _allRowDataList;
        }

        return filteredRowDataList;
    }

    private boolean passFilterBySwitches( ArcBaseIngestFilterJTableRowData rowData )
    {
        boolean addRowData = false;

        if ( _switchesFilterCheckBox.isSelected() )
        {
            if ( ( passSwitchFilter( _masterFilterCB.isSelected(), rowData.getRaxIngestFilter().isIngest() ) ) &&
                    ( passSwitchFilter( _ofsFilterCB.isSelected(), rowData.getRaxIngestFilter().isOfsInput() ) ) &&
                    ( passSwitchFilter( _mpeFilterCB.isSelected(), rowData.getRaxIngestFilter().isMpeInput() ) ) )
            {
                addRowData = true;
            }
        }
        else
        {
            addRowData = true;
        }
        return addRowData;
    }
    
    private boolean passSwitchFilter( boolean isFilterOn, boolean enabled )
    {
        boolean passFilter = (isFilterOn == enabled);    //evil   
        return passFilter;
    }
    
    private boolean passFilterByTS( ArcBaseIngestFilterJTableRowData rowData )
    {
        boolean addRowData = false; 
        
        if ( _tsFilterCheckBox.isSelected() )
        {
            String tsCBString = (String) _tsFilterComboBox.getSelectedItem();

            if ( tsCBString.indexOf( "(" + rowData.getRaxIngestFilter().getTs() + ")"  ) > 0 )
            {
                addRowData = true;
            }
        }
        else
        {
            addRowData = true;
        }
        
        return addRowData;
    }
    
    private boolean passFilterByPE( ArcBaseIngestFilterJTableRowData rowData )
    {
        boolean addRowData = false;  //Only false if the checkbox is enabled and is not in the PE filter list
        Object[] peJListObjectArray = null;

        if ( ( _peFilterCheckBox.isSelected() ) && ( ! _peJList.isSelectionEmpty() ) )
        {
            peJListObjectArray = _peJList.getSelectedValues();
            Set selectedPESet = new HashSet();
            String peString = rowData.getRaxIngestFilter().getPe();
            
            for ( int i = 0; i < peJListObjectArray.length; i++ )
            {
                selectedPESet.add( (String) peJListObjectArray[ i ] );
            }
            
            for ( int i = 0; i < _peCBStringList.size(); i++ )
            {
                String peCBString = (String) _peCBStringList.get( i );
                if ( peCBString.startsWith( peString ) )
                {
                    if ( selectedPESet.contains( peCBString ) )
                    {
                        addRowData = true;
                    }
                }
            }
            
        }
        else
        {
            addRowData = true;
        }
        
        return addRowData;
    }
    
    private boolean passFilterByLid_Shorter( ArcBaseIngestFilterJTableRowData rowData )
    {
        boolean addRowData = true;  // Only false if the checkbox is enabled AND does not match the filter string
        
        boolean attemptFilter = _lidFilterCheckBox.isSelected();
        
        if (  attemptFilter )
        {
            String filterString = _lidFilterTextField.getText().trim();
            
            if ( ! rowData.getRaxIngestFilter().getLid().startsWith( filterString  ) )
            {
                addRowData = false;
            }
        }
    
        return addRowData;
    }
    
    
    private boolean passFilterByLid( ArcBaseIngestFilterJTableRowData rowData )
    {
        boolean addRowData = false;  // Only false if the checkbox is enabled AND does not match the filter string
        
        if ( _lidFilterCheckBox.isSelected() )
        {
            String filterString = _lidFilterTextField.getText().toUpperCase().trim();
            
            if ( rowData.getRaxIngestFilter().getLid().startsWith( filterString  ) )
            {
                addRowData = true;
            }
        }
        else
        {
            addRowData = true;
        }
        return addRowData;
    }
    
    private void clearDataInputPanel()
    {
        _lidTextField.setText( "" );
        _masterCheckBox.setSelected( false );
        _ofsCheckBox.setSelected( false);
        _mpeInputCheckBox.setSelected( false );
        _peComboBox.setSelectedIndex( 0 );
        _durationComboBox.setSelectedIndex( 0 );
        _typeSourceComboBox.setSelectedIndex( 0 );
        _extremumComboBox.setSelectedIndex( 0 );
        _newReportCheckBox.setSelected( false );
        _activeCheckBox.setSelected( false );
        _obsTimeTextField.setText( "" );
        _ownAgencyTextField.setText( "" );
        _ownLocationTextField.setText( "" );
        _rankComboBox.setSelectedIndex( 1 );
    }
    
    private void addRowDataToJTable( RaxIngestFilter raxIngestFilter )
    {
        String keyString = raxIngestFilter.getPrimaryKey();
        ArcBaseIngestFilterJTableRowData addRowData = new ArcBaseIngestFilterJTableRowData();
        String extractedKeyString = null;
        ArcBaseIngestFilterJTableRowData rowData = null;
        boolean found = false;
        
        addRowData.setRaxIngestFilter( raxIngestFilter );
        
        for ( int i = 0; i < _allRowDataList.size(); i++ )
        {
            rowData = (ArcBaseIngestFilterJTableRowData) _allRowDataList.get( i );
            
            extractedKeyString = rowData.getRaxIngestFilter().getPrimaryKey();
            
            if ( keyString.equalsIgnoreCase( extractedKeyString ) )
            {
                _allRowDataList.remove( rowData );
                _allRowDataList.add( addRowData );
                found = true;
                break;
            }
        }
        
        if ( ! found )
        {
            _allRowDataList.add( addRowData );
        }
        
        _raxIFRowDataToRaxIFMap.put( addRowData, raxIngestFilter );
        
        addRowData.addAllCellsToMap();
        _IFETableManager.setChangedAllRowDataList( _allRowDataList );
        _IFETableManager.sort( "Location", false, true );
        _IFETableManager.refreshDisplay();
    }
    
    private void removeRowDataFromJTable( RaxIngestFilter raxIngestFilter )
    {
        ArcBaseIngestFilterJTableRowData removeRowData = new ArcBaseIngestFilterJTableRowData();
        ArcBaseIngestFilterJTableRowData rowData = null;
        
        for ( int i = 0; i < _allRowDataList.size(); i++ )
        {
            rowData = (ArcBaseIngestFilterJTableRowData) _allRowDataList.get( i );
            if ( raxIngestFilter.getPrimaryKey().equalsIgnoreCase( rowData.getRaxIngestFilter().getPrimaryKey() ) )
            {
                _allRowDataList.remove( rowData );
                break;
            }
        }
        _IFETableManager.setChangedAllRowDataList( _allRowDataList );
        _IFETableManager.refreshDisplay();
    }
    
    private void deleteRaxIngestFilter()
    {
        RaxIngestFilter raxIngestFilter = null;
        List selectedRowsData = _IFETableManager.getSelectedRowsData();
        
        if ( ! selectedRowsData.isEmpty() )
        {
            ArcBaseIngestFilterJTableRowData rowData = (ArcBaseIngestFilterJTableRowData) selectedRowsData.get( 0 );
            raxIngestFilter = (RaxIngestFilter) _raxIFRowDataToRaxIFMap.get( rowData );
            if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + raxIngestFilter.keyString(), "Delete Ingest Filter Entry" ) )
            {
                _dataMgr.deleteRaxIngestFilterFromDatabase( raxIngestFilter );
                clearDataInputPanel();
                removeRowDataFromJTable( raxIngestFilter );
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select a crest", "Delete Crest" );
        }
    }
    
    
    private void saveRaxIngestFilter()
    {
        RaxIngestFilter raxIngestFilter = new RaxIngestFilter();
        
        String pe = (String) _peComboBox.getSelectedItem();
        String ts = (String) _typeSourceComboBox.getSelectedItem();
        String extremum = (String) _extremumComboBox.getSelectedItem();
        String duration = (String) _durationComboBox.getSelectedItem();
        short iDuration = (Short) _durCBStringToIDurMap.get( duration );
        short tsRank = (short) _rankComboBox.getSelectedIndex();
        
        raxIngestFilter.setLid( _lidTextField.getText() );
        raxIngestFilter.setPe( (String) _peCBStringToPeMap.get( pe ) );
        raxIngestFilter.setDur( (String) _durCBStringToDurMap.get( duration ) );
        raxIngestFilter.setIdur( iDuration );
        raxIngestFilter.setTs( (String ) _tsCBStringToTSMap.get( ts ) );
        raxIngestFilter.setExtremum( (String) _extremumCBStringToExtremumMap.get( extremum ) );
        raxIngestFilter.setTsRank( tsRank );
        raxIngestFilter.setDetailInfo( _detailTextField.getText() );
        raxIngestFilter.setIngest( _masterCheckBox.isSelected() );
        raxIngestFilter.setNewReport( _newReportCheckBox.isSelected() );
        raxIngestFilter.setActive( _activeCheckBox.isSelected() );
        raxIngestFilter.setOfsInput( _ofsCheckBox.isSelected() );

        long obsTime = 0;
        
        if ( _obsTimeTextField.getText().equalsIgnoreCase( "" ) )
        {
            obsTime = DbTimeHelper.getLongTimeFromTimeToSecondsString( null );
        }
        else
        {
            obsTime = DbTimeHelper.getLongTimeFromTimeToSecondsString( _obsTimeTextField.getText() );
        }
  
        raxIngestFilter.setObsTime( obsTime );
//        raxIngestFilter.setObsTime( DbTimeHelper.getLongTimeFromTimeToSecondsString(_obsTimeTextField.getText())  );
        raxIngestFilter.setOwnerAgency( _ownAgencyTextField.getText() );
        raxIngestFilter.setOwnerLocation( _ownLocationTextField.getText() );
        raxIngestFilter.setMpeInput( _mpeInputCheckBox.isSelected() );
        
        _dataMgr.saveRaxIngestFilter( raxIngestFilter );
        
        addRowDataToJTable( raxIngestFilter );
        System.out.println( raxIngestFilter );
    }
    
    private void populateDataInputPanel( RaxIngestFilter raxIF )
    {
        String obsTimeString = null;
        
        _lidTextField.setText( raxIF.getLid() );

        if ( raxIF.isIngest() )
        {
            _masterCheckBox.setSelected( true );
        }
        else
        {
            _masterCheckBox.setSelected( false );
        }
        
        if ( raxIF.isOfsInput() )
        {
            _ofsCheckBox.setSelected( true );
        }
        else
        {
            _ofsCheckBox.setSelected( false );
        }

        if ( raxIF.isMpeInput() )
        {
            _mpeInputCheckBox.setSelected( true );
        }
        else
        {
            _mpeInputCheckBox.setSelected( false );
        }
        
        ShefPE shefPE = (ShefPE) _dataMgr.getShefPEMap().get( raxIF.getPe() );
        if ( shefPE != null )
        {
            _peComboBox.setSelectedItem( getShefPEComboBoxString( shefPE ) );
        }
        else
        {
            _peComboBox.setSelectedItem( "" );
        }
        
        ShefDuration shefDur = (ShefDuration) _dataMgr.getShefDurationMap().get( raxIF.getDur() );
        if ( shefDur != null )
        {
            _durationComboBox.setSelectedItem( getShefDurationComboBoxString( shefDur ) );
        }
        else
        {
            _durationComboBox.setSelectedItem( "" );
        }
        
        ShefTS shefTS = (ShefTS) _dataMgr.getShefTSMap().get( raxIF.getTs() );
        if ( shefTS != null )
        {
            _typeSourceComboBox.setSelectedItem( getShefTSString( shefTS ) );
        }
        else
        {
            _typeSourceComboBox.setSelectedItem( "" );
        }
        
        ShefExtremum shefEx = (ShefExtremum) _dataMgr.getShefExtremumMap().get( raxIF.getExtremum() );
        if ( shefEx != null )
        {
            _extremumComboBox.setSelectedItem( getShefExtremumComboBoxString( shefEx ) );
        }
        else
        {
            _extremumComboBox.setSelectedItem( "" );
        }
        
        
        _detailTextField.setText( raxIF.getDetailInfo() );
        
        _newReportCheckBox.setSelected( raxIF.isNewReport() );
        
        _activeCheckBox.setSelected( raxIF.isActive() );
        
        if ( DbTable.isNull( raxIF.getObsTime() ) )
        { 
            obsTimeString = "";
        }
        else
        {
            obsTimeString = DbTimeHelper.getTimeToSecondsStringFromLongTime( raxIF.getObsTime() );
        }
        
        _obsTimeTextField.setText( obsTimeString );
        _ownAgencyTextField.setText( raxIF.getOwnerAgency() );
        _ownLocationTextField.setText( raxIF.getOwnerLocation() );
        
        short tsRank = raxIF.getTsRank();
        
        switch ( tsRank )
        {
            case 1: _rankComboBox.setSelectedIndex( 1 ); break;
            case 2: _rankComboBox.setSelectedIndex( 2 ); break;
            case 3: _rankComboBox.setSelectedIndex( 3 ); break;
            case 4: _rankComboBox.setSelectedIndex( 4 ); break;
            case 5: _rankComboBox.setSelectedIndex( 5 ); break;
            default: _rankComboBox.setSelectedIndex( 0 ); break;
        }
    }

    
    /**
     * Sets up the Columns for the arcbase jtable
     *
     */    
    private void setIFEColumnDescriptorList()
    {
        _IFEColumnDescriptorList = new ArrayList();

        _IFEColumnDescriptorList.add(new JTableColumnDescriptor( "Location", true, 75, "center" ) );
        _IFEColumnDescriptorList.add(new JTableColumnDescriptor( "PE", true, 50, "center" ) );
        _IFEColumnDescriptorList.add(new JTableColumnDescriptor( "Dur", true, 75, "center" ) );
        _IFEColumnDescriptorList.add(new JTableColumnDescriptor( "TypeSrc", true, 75, "center" ) );
        _IFEColumnDescriptorList.add(new JTableColumnDescriptor( "Ext", true, 50, "center" ) );
        _IFEColumnDescriptorList.add(new JTableColumnDescriptor( "Rank", true, 50, "center" ) );
        _IFEColumnDescriptorList.add(new JTableColumnDescriptor( "Master", true, 50, "center" ) );
        _IFEColumnDescriptorList.add(new JTableColumnDescriptor( "OFS", true, 50, "center" ) );
        _IFEColumnDescriptorList.add(new JTableColumnDescriptor( "MPE", true, 50, "center" ) );
    }

    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();
        
        addWindowListener( windowCloser );

        _IFETableManager.addTableListener( new IFETableListener() );
        
        _closeButton.addActionListener( windowCloser );
        _clearButton.addActionListener( new ClearButtonListener() );
        _saveButton.addActionListener( new SaveButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
        
        _lidFilterCheckBox.addActionListener( new LidFilterCheckBoxListener() );
        _tsFilterCheckBox.addActionListener( new TSFilterCheckBoxListener() );
        _peFilterCheckBox.addActionListener( new PEFilterCheckBoxListener() );
        _switchesFilterCheckBox.addActionListener( new SwitchesFilterCheckBoxListener() );

        _lidFilterTextField.addKeyListener( new LidFilterTextFieldListener() );
        _peJList.addListSelectionListener( new PEFilterJListListener() );
        _tsFilterComboBox.addActionListener( new TSFilterComboBoxListener() );
        _masterFilterCB.addActionListener( new Master_OFS_MPE_FilterCheckBoxListener() );
        _ofsFilterCB.addActionListener( new Master_OFS_MPE_FilterCheckBoxListener() );
        _mpeFilterCB.addActionListener( new Master_OFS_MPE_FilterCheckBoxListener() );
    }
    
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveRaxIngestFilter();
        }
    }
    
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteRaxIngestFilter();
        }
    }
    
    private class Master_OFS_MPE_FilterCheckBoxListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            displayFilteredIngestFilterEntries();
        }
    }
    
    private class PEFilterJListListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                displayFilteredIngestFilterEntries();
            }
        }
    }
    
    private class TSFilterComboBoxListener implements ActionListener
    {

        public void actionPerformed( ActionEvent e )
        {
            displayFilteredIngestFilterEntries();
        }
    }
    
    private class LidFilterTextFieldListener implements KeyListener
    {
        public void keyPressed( KeyEvent e )
        {
        }

        public void keyReleased( KeyEvent e )
        {
            displayFilteredIngestFilterEntries();
        }

        public void keyTyped( KeyEvent e )
        {
        }
    }

    private class ClearButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            clearDataInputPanel();
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

    private class TSFilterCheckBoxListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( _tsFilterCheckBox.isSelected() )
            {
                activateTSFilter( true );
            }
            else
            {
                activateTSFilter( false );
                displayFilteredIngestFilterEntries();
            }
        }
        
    }
    
    private class PEFilterCheckBoxListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( _peFilterCheckBox.isSelected() )
            {
                activatePEFilter( true );
            }
            else
            {
                activatePEFilter( false );
                _peJList.clearSelection();
                displayFilteredIngestFilterEntries();
            }
        }
    }
    
    private class SwitchesFilterCheckBoxListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( _switchesFilterCheckBox.isSelected() )
            {
                activateSwitchesFilter( true );
            }
            else
            {
                activateSwitchesFilter( false );
            }
            displayFilteredIngestFilterEntries();
        }
    }
    
    private class LidFilterCheckBoxListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( _lidFilterCheckBox.isSelected() )
            {
                activateLIDFilter( true );
            }
            else
            {
                activateLIDFilter( false );
                displayFilteredIngestFilterEntries();
            }
        }
    }
    
    private class IFETableListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                RaxIngestFilter raxIngestFilter = null;
                ArcBaseIngestFilterJTableRowData rowData = (ArcBaseIngestFilterJTableRowData) _IFETableManager.getSelectedRowsData().get( 0 );
                
                raxIngestFilter = (RaxIngestFilter) _raxIFRowDataToRaxIFMap.get( rowData );
                
                populateDataInputPanel( raxIngestFilter );
            }
        }
    }
    
    public static void main( String args[] )
    {
        JFrame frame = new JFrame();
        frame.setSize( new Dimension( 1024, 768 ) );
        RaxBaseDataMgr dataMgr = new RaxBaseDataMgr( "jdbc:postgresql://lx5:5432/adb_ob83raxtest?user=pguser" );
        IngestFilterEditor ingestFilterEditorGUI = new IngestFilterEditor( frame, dataMgr );
        ingestFilterEditorGUI.displayGUI();
    }

}