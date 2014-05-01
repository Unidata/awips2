package ohd.hseb.raxbase.gui;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
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
import ohd.hseb.raxbase.RaxBaseDataMgr;
import ohd.hseb.raxbase.model.RaxDataLimits;
import ohd.hseb.raxbase.model.RaxLocDataLimits;
import ohd.hseb.raxbase.model.ShefDuration;
import ohd.hseb.raxbase.model.ShefPE;
import ohd.hseb.raxbase.table.ArcBaseDataLimitsJTableRowData;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class DataLimitsEditor extends JDialog
{
    private Container _dialogContentPane = getContentPane();

    private RaxBaseDataMgr _dataMgr = null;
    
    private Map _raxDLRowDataToRaxDLMap = null;
    private Map _raxLocDLRowDataToRaxLocDLMap = null;
    
//  JTable variables
    private JTableManager _DLETableManager = null;
    private List _DLEColumnDescriptorList = null;
    private List _dataLimitsRowDataList = null;
    private List _locDataLimitsRowDataList = null;
    private List _filteredRowDataList = null;
    private JScrollPane _tableScrollPane = null;
    
    private JPanel _dataLimitsSelectionPanel = new JPanel( new GridBagLayout() );
    private JPanel _tableScrollPanePanel = new JPanel( new GridBagLayout() );
    private JLabel _listLabel = new JLabel( "List:" );
    private String[] _limitsLocLimitsStringArray = { "Default Limits", "Location Limits" };
    private JComboBox _limitsLocLimitsCB = new JComboBox( _limitsLocLimitsStringArray );
    private JCheckBox _lidFilterCB = new JCheckBox( "Location" );
    private JTextField _lidFilterTextBox = new JTextField();
    private JCheckBox _peFilterCB = new JCheckBox( "Physical Element" );
    private JList _peFilterJList = new JList();
    private JScrollPane _peFilterScrollPane = new JScrollPane();
    private DefaultListModel _peFilterListModel = new DefaultListModel();
    
    private List _peJLStringList= null;
    private Map _peJLStringToPeMap = new HashMap();
    private Map _durCBStringToDurMap = new HashMap();

    private JPanel _selectedItemPanel = new JPanel( new GridBagLayout() );
    private JLabel _lidLabel = new JLabel( "Location: " );
    private JTextField _lidTextField = new JTextField();
    private JLabel _durationLabel = new JLabel( "Duration: " );
    private JComboBox _durationComboBox = new JComboBox();
    private JLabel _startDateLabel = new JLabel( "Start MM-DD: " );
    private JTextField _startDateTextField = new JTextField();
    private JLabel _endDateLabel = new JLabel( "End MM-DD: " );
    private JTextField _endDateTextField = new JTextField();
    private JLabel _peLabel = new JLabel( "PE (Physical Element):" );
    private JList _peJList = new JList();
    private JScrollPane _peScrollPane = new JScrollPane();
    private DefaultListModel _peListModel = new DefaultListModel();
    private JLabel _minLabel = new JLabel( "  Minimum  " );
    private JLabel _maxLabel = new JLabel( "  Maximum  " );
    private JLabel _grossRangeLabel = new JLabel( "Gross Range: " );
    private JLabel _reasonRangeLabel = new JLabel( "Reasonable Range: " );
    private JLabel _rocLabel = new JLabel( "Rate of Change (ROC): " );
    private JLabel _unitsLabel = new JLabel( "Units/Hour" );
    private JLabel _alertAlarmLimitsLabel = new JLabel( "Alert/Alarm Limits" );
    private JLabel _limitsLabel = new JLabel( "Limit" );
    private JLabel _rocLimitLabel = new JLabel( "ROC" );
    private JLabel _alertLabel = new JLabel( "Alert" );
    private JLabel _alarmLabel = new JLabel( "Alarm" );
    private JTextField _grossRangeMinTF = new JTextField();
    private JTextField _grossRangeMaxTF = new JTextField();
    private JTextField _reasonRangeMinTF = new JTextField();
    private JTextField _reasonRangeMaxTF = new JTextField();
    private JTextField _rocTextField = new JTextField();
    private JTextField _alertTextField = new JTextField();
    private JTextField _alertRocTextField = new JTextField();
    private JTextField _alarmTextField = new JTextField();
    private JTextField _alarmRocTextField = new JTextField();
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _clearButton = new JButton( "Clear" );
    private JButton _deleteButton = new JButton( "Delete" );
    
    public DataLimitsEditor( JFrame frame, RaxBaseDataMgr dataMgr )
    {
        super( frame, "DataLimits Editor", true );
        _dataMgr = dataMgr;
        updateRowDataLists();
    }

    private void updateRowDataLists()
    {
        _dataLimitsRowDataList = _dataMgr.getDataLimitsRowDataList();
        _locDataLimitsRowDataList = _dataMgr.getLocDataLimitsRowDataList();
    }
    
    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 1100, 825 ) );
        initGUI();
    }

    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        initListSelectionPanel();
        initSelectedItemPanel();
        initButtonPanel();
        initFrameComponents();
        initFilters();
        initHashMaps();
        addListeners();
        pack();
        setVisible( true );
    }
    
    private void initFilters()
    {
        activateLidFilter( false );
        activatePEFilter( false );
        enableLidFilter( false );
    }
    
    private void initHashMaps()
    {
        _raxDLRowDataToRaxDLMap = _dataMgr.getRaxDataLimitsRowDataToRaxDataLimitsMap();
        _raxLocDLRowDataToRaxLocDLMap = _dataMgr.getRaxLocDataLimitsRowDataToRaxLocDataLimitsMap();
    }

    private void updateJTable( boolean refresh )
    {
        if ( refresh )
        {
            updateRowDataLists();
        }
        String dataLimitsString = (String) _limitsLocLimitsCB.getSelectedItem();
        
        if ( dataLimitsString.equalsIgnoreCase( "Location Limits" ) )
        {
            _DLETableManager.setChangedAllRowDataList( _locDataLimitsRowDataList );
            enableLidFilter( true );
        }
        else
        {
            _DLETableManager.setChangedAllRowDataList( _dataLimitsRowDataList );
            enableLidFilter( false );
        }
        _DLETableManager.refreshDisplay();

    }
    private void initJTable() 
    {
        _dataLimitsRowDataList = _dataMgr.getDataLimitsRowDataList();
        setLocDLEColumnDescriptorList();
        _DLETableManager = new ComplexJTableManager( _DLEColumnDescriptorList, _dataLimitsRowDataList, ListSelectionModel.SINGLE_SELECTION );
        enableLocationInColumnDescriptorList( false );
        String columnsSelected[] = _DLETableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _DLETableManager.setDisplayableColumns( columnsSelected, true, true );
        _tableScrollPane = _DLETableManager.getJScrollPane();
        _tableScrollPane.setPreferredSize( new Dimension( 1000, 300 ) );
//                                                                                     X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _tableScrollPanePanel, _tableScrollPane,    0,   1,    1,     1, 1, 1, GridBagConstraints.BOTH );
        
    }
    
    private void initListSelectionPanel()
    {
        GridBagConstraints gbLabelConstraints = new GridBagConstraints();
        GridBagConstraints gbTFConstraints = new GridBagConstraints();
        GridBagConstraints gbFilterConstraints = new GridBagConstraints();
        JPanel filterPanel = new JPanel( new GridBagLayout() );
        JPanel hPanel = new JPanel();
        JPanel vPanel = new JPanel();
        JPanel vPanel2 = new JPanel();

        gbLabelConstraints.anchor = GridBagConstraints.EAST;
        gbLabelConstraints.weightx = 1;
        gbLabelConstraints.weighty = 0;
        
        gbTFConstraints.anchor = GridBagConstraints.WEST;
        gbTFConstraints.weightx = 1;
        gbTFConstraints.weighty = 0;
        
        initJTable();  // Initializes the JTable
        
        _lidFilterTextBox.setColumns( 8 );
        
        setupPEJList( _peFilterListModel, _peFilterJList );  // Set up the PE Filter JList

        _peFilterJList.setSelectionMode( ListSelectionModel.SINGLE_SELECTION );
        _peFilterScrollPane = new JScrollPane( _peFilterJList );
        
        _peFilterScrollPane.setVerticalScrollBarPolicy( JScrollPane.VERTICAL_SCROLLBAR_ALWAYS );
//        _peFilterJList.setPreferredSize( new Dimension( 215, 100 ) );
//        _peFilterScrollPane.setPreferredSize( new Dimension( 225, 100 ) );
        
        _dataLimitsSelectionPanel.setBorder( BorderFactory.createTitledBorder( "Limits" ) );
//        _dataLimitsSelectionPanel.setPreferredSize( new Dimension( 1000, 600 ) );
        filterPanel.setBorder( BorderFactory.createTitledBorder( "Filters" ) );
//        filterPanel.setPreferredSize( new Dimension( 500, 500 ) );
//                                                                                                           X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _dataLimitsSelectionPanel, _listLabel, gbLabelConstraints,        0,   0,    1,      1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataLimitsSelectionPanel, _limitsLocLimitsCB, gbTFConstraints,   1,   0,    1,      1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataLimitsSelectionPanel, vPanel2,                               2,   0,    1,      1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( filterPanel, _lidFilterCB, gbLabelConstraints,                    1,   0,    1,      1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( filterPanel, _lidFilterTextBox, gbTFConstraints,                  2,   0,    1,      1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( filterPanel, _peFilterCB, gbLabelConstraints,                     1,   1,    1,      1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( filterPanel, _peFilterScrollPane, gbTFConstraints,                2,   1,    1,      1, GridBagConstraints.NONE );
        

        gbFilterConstraints.gridx = 3;
        gbFilterConstraints.gridy = 0;
        gbFilterConstraints.gridwidth = 1;
        gbFilterConstraints.gridheight = 1;
        gbFilterConstraints.weightx = 1;
        gbFilterConstraints.weighty = 1;
        gbFilterConstraints.fill = GridBagConstraints.BOTH;

        GridBagLayout layoutMgr = (GridBagLayout) _dataLimitsSelectionPanel.getLayout();
        layoutMgr.setConstraints( filterPanel, gbFilterConstraints );
        
        _dataLimitsSelectionPanel.add( filterPanel );

        ComponentHelper.addPanelComponent( _dataLimitsSelectionPanel, hPanel,                                0,   1,    4,      1, 1, 1, GridBagConstraints.BOTH );
        
        ComponentHelper.addPanelComponent( _dataLimitsSelectionPanel, _tableScrollPanePanel,                 0,   2,    10,      1, 1, 1, GridBagConstraints.BOTH );
    }
    
    
    private void initFrameComponents()
    {
//                                                                                         X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _dataLimitsSelectionPanel,  0,  0,    4,     20, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _selectedItemPanel,         0,  21,    4,     1, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _buttonPanel,               0,  22,    4,     1, 1, 1, GridBagConstraints.BOTH );
    }
    
    private void initSelectedItemPanel()
    {
        List shefDurList = _dataMgr.getShefDurationList();
        
        GridBagConstraints gbLabelConstraints = new GridBagConstraints();
        GridBagConstraints gbTFConstraints = new GridBagConstraints();
        List shefDurationStringList = new ArrayList();
        JPanel lidDurDatesPanel = new JPanel( new GridBagLayout() );
        JPanel minMaxRangePanel = new JPanel( new GridBagLayout() );
        JPanel vPanel1 = new JPanel();
//        JPanel vPanel2 = new JPanel();
        JPanel vPanel3 = new JPanel();
        JPanel vPanel4 = new JPanel();
        JPanel hPanel1 = new JPanel();
        JPanel hPanel2 = new JPanel();
        JPanel hPanel3 = new JPanel();
        JPanel hPanel4 = new JPanel();
        
        gbLabelConstraints.anchor = GridBagConstraints.EAST;
        gbLabelConstraints.weightx = 1;
        gbLabelConstraints.weighty = 0;
        
        gbTFConstraints.anchor = GridBagConstraints.WEST;
        gbTFConstraints.weightx = 1;
        gbTFConstraints.weighty = 0;

        _selectedItemPanel.setBorder( BorderFactory.createTitledBorder( "Limits for Selected Item" ) );
        minMaxRangePanel.setBorder( BorderFactory.createTitledBorder( "Quality Control Limits" ) );

        setupPEJList( _peListModel, _peJList );

        for ( int i = 0; i < shefDurList.size(); i++ )
        {
            ShefDuration shefDur = (ShefDuration) shefDurList.get( i );
            shefDurationStringList.add( getShefDurationComboBoxString( shefDur ) );
            _durCBStringToDurMap.put( getShefDurationComboBoxString( shefDur ), shefDur );
        }
        
        _durationComboBox = new JComboBox( shefDurationStringList.toArray() );

        _peJList.setSelectionMode( ListSelectionModel.SINGLE_SELECTION );
        _peScrollPane = new JScrollPane( _peJList );
        
        _lidTextField.setColumns( 7 );
        _startDateTextField.setColumns( 7 );
        _endDateTextField.setColumns( 7 );
        
        _peScrollPane.setPreferredSize( new Dimension( 125, 100 ) );

        
//                                                                                                 X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( lidDurDatesPanel, _lidLabel, gbLabelConstraints,        1,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( lidDurDatesPanel, _lidTextField, gbTFConstraints,       2,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( lidDurDatesPanel, hPanel1,                              1,  1,    2,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( lidDurDatesPanel, _durationLabel, gbLabelConstraints,   1,  2,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( lidDurDatesPanel, _durationComboBox, gbTFConstraints,   2,  2,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( lidDurDatesPanel, hPanel2,                              1,  3,    2,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( lidDurDatesPanel, _startDateLabel, gbLabelConstraints,  1,  4,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( lidDurDatesPanel, _startDateTextField, gbTFConstraints, 2,  4,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( lidDurDatesPanel, hPanel3,                              1,  5,    2,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( lidDurDatesPanel, _endDateLabel, gbLabelConstraints,    1,  6,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( lidDurDatesPanel, _endDateTextField, gbTFConstraints,   2,  6,    1,     1, GridBagConstraints.NONE );
        
//                                                                                                  X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( minMaxRangePanel, vPanel3,                               1,  0,    1,     4, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addPanelComponent( minMaxRangePanel, _minLabel,                             2,  0,    1,     1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addPanelComponent( minMaxRangePanel, vPanel4,                               3,  0,    1,     4, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addPanelComponent( minMaxRangePanel, _maxLabel,                             4,  0,    1,     1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addPanelComponent( minMaxRangePanel, _grossRangeLabel, gbLabelConstraints,  0,  1,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( minMaxRangePanel, _grossRangeMinTF,  gbTFConstraints,    2,  1,    1,     1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addPanelComponent( minMaxRangePanel, _grossRangeMaxTF,                      4,  1,    1,     1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addPanelComponent( minMaxRangePanel, _reasonRangeLabel, gbLabelConstraints, 0,  2,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( minMaxRangePanel, _reasonRangeMinTF, gbTFConstraints,    2,  2,    1,     1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addPanelComponent( minMaxRangePanel, _reasonRangeMaxTF,                     4,  2,    1,     1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addPanelComponent( minMaxRangePanel, _rocLabel, gbLabelConstraints,         0,  3,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( minMaxRangePanel, _rocTextField, gbTFConstraints,        2,  3,    1,     1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addPanelComponent( minMaxRangePanel, _unitsLabel,                           4,  3,    1,     1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addPanelComponent( minMaxRangePanel, hPanel4,                               0,  4,    5,     2, GridBagConstraints.HORIZONTAL );

        ComponentHelper.addPanelComponent( minMaxRangePanel, _alertAlarmLimitsLabel,                0,  6,    1,     1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addPanelComponent( minMaxRangePanel, _limitsLabel,                          2,  6,    1,     1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addPanelComponent( minMaxRangePanel, _rocLimitLabel,                        4,  6,    1,     1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addPanelComponent( minMaxRangePanel, _alertLabel, gbLabelConstraints,       0,  7,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( minMaxRangePanel, _alertTextField, gbTFConstraints,      2,  7,    1,     1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addPanelComponent( minMaxRangePanel, _alertRocTextField,                    4,  7,    1,     1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addPanelComponent( minMaxRangePanel, _alarmLabel, gbLabelConstraints,       0,  8,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( minMaxRangePanel, _alarmTextField,gbTFConstraints,       2,  8,    1,     1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addPanelComponent( minMaxRangePanel, _alarmRocTextField,                    4,  8,    1,     1, GridBagConstraints.HORIZONTAL );

        ComponentHelper.addPanelComponent( _selectedItemPanel, lidDurDatesPanel,       0,  0,    1,     2, GridBagConstraints.BOTH );
        ComponentHelper.addPanelComponent( _selectedItemPanel, vPanel1,                2,  0,    1,     2, GridBagConstraints.BOTH );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _peLabel,               3,  0,    1,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _peScrollPane,          3,  1,    1,     1, 1, 4, GridBagConstraints.BOTH );
        ComponentHelper.addPanelComponent( _selectedItemPanel, minMaxRangePanel,       4,  0,    1,     2, 1, 1, GridBagConstraints.BOTH );
        
    }
    
    private void initButtonPanel()
    {
//                                                                              X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _buttonPanel, _saveAndCloseButton,   0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _saveButton,           1,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _closeButton,          2,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _clearButton,            3,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _deleteButton,         4,  0,    1,     1, GridBagConstraints.NONE );
    }
    
    private void enableLocationInColumnDescriptorList( boolean enableLocation )
    {
        if ( _DLEColumnDescriptorList != null )
        {
            JTableColumnDescriptor colDesc = null;
            
            for ( int i = 0; i < _DLEColumnDescriptorList.size(); i++ )
            {
                colDesc = (JTableColumnDescriptor) _DLEColumnDescriptorList.get( i );
                if ( colDesc.getColumnName().equalsIgnoreCase( "Location" ) )
                {
                    colDesc.setDisplay( enableLocation );
                    break;
                }
            }
        }
    }
    
/*    
    private void setDLEColumnDescriptorList()
    {
        _DLEColumnDescriptorList = new ArrayList();

        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "PE", true, 30, "center" ) );
        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "Dur", true, 75, "center" ) );
        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "Start", true, 50, "center" ) );
        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "End", true, 50, "center" ) );
        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "Gross Min", true, 100, "center" ) );
        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "Gross Max", true, 100, "center" ) );
        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "<HTML>Reasonable<BR>Min</HTML>", true, 100, "center" ) );
        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "<HTML>Reasonable<BR>Max</HTML>", true, 100, "center" ) );
        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "<HTML>Rate of<BR>Change</HTML>", true, 75, "center" ) );
        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "Alert Limit", true, 100, "center" ) );
        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "<HTML>Alert Limit<BR>ROC</HTML>", true, 100, "center" ) );
        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "Alarm Limit", true, 100, "center" ) );
        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "<HTML>Alarm Limit<BR>ROC</HTML>", true, 100, "center" ) );
    }
*/
    
    private void setLocDLEColumnDescriptorList()
    {
        _DLEColumnDescriptorList = new ArrayList();

        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "Location", true, 75, "center" ) );
        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "PE", true, 30, "center" ) );
        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "Dur", true, 75, "center" ) );
        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "Start", true, 50, "center" ) );
        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "End", true, 50, "center" ) );
        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "Gross Min", true, 100, "center" ) );
        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "Gross Max", true, 100, "center" ) );
        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "<HTML>Reasonable<BR>Min</HTML>", true, 100, "center" ) );
        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "<HTML>Reasonable<BR>Max</HTML>", true, 100, "center" ) );
        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "<HTML>Rate of<BR>Change</HTML>", true, 75, "center" ) );
        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "Alert Limit", true, 100, "center" ) );
        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "<HTML>Alert Limit<BR>ROC</HTML>", true, 100, "center" ) );
        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "Alarm Limit", true, 100, "center" ) );
        _DLEColumnDescriptorList.add(new JTableColumnDescriptor( "<HTML>Alarm Limit<BR>ROC</HTML>", true, 100, "center" ) );
    }

    private String getShefPEJListString( ShefPE shefPE )
    {
        String shefPEString = shefPE.getPe() + " " + shefPE.getName();
        
        return shefPEString;
    }
    
    private String getShefDurationComboBoxString( ShefDuration shefDuration )
    {
        String shefDurationString = shefDuration.getName() + " ( " + shefDuration.getIduration() + "/" + shefDuration.getDuration() + " )";

        return shefDurationString;
    }
 
    private void setupPEJList( DefaultListModel listModel, JList peJList )
    {
        List shefPeList = _dataMgr.getShefPeList();
        String peString = null;

        if ( _peJLStringList == null )
        {
            _peJLStringList = new ArrayList();
            for ( int i = 0; i < shefPeList.size(); i++ )
            {
                ShefPE shefPE = (ShefPE) shefPeList.get( i );
                _peJLStringList.add( getShefPEJListString( shefPE ) );
                _peJLStringToPeMap.put( getShefPEJListString( shefPE ), shefPE );
            }
        }
        
        for ( int i = 0; i < _peJLStringList.size(); i++ )
        {
            peString = (String) _peJLStringList.get( i );
            listModel.addElement( peString );
        }
        
        peJList.setModel( listModel );
    }

    private void activateLidFilter( boolean enable )
    {
        if ( enable )
        {
            _lidFilterTextBox.setEnabled( true );
        }
        else 
        {
            _lidFilterTextBox.setEnabled( false );
        }
    }
    
    private void activatePEFilter( boolean enable )
    {
        if ( enable )
        {
            _peFilterJList.setEnabled( true );
            _peFilterScrollPane.setEnabled( true );
        }
        else
        {
            _peFilterJList.setEnabled( false );
            _peFilterScrollPane.setEnabled( false );
        }
    }
    
    private void enableLidFilter( boolean enable )
    {
        if ( enable )
        {
            _lidFilterCB.setVisible( true );
            _lidFilterTextBox.setVisible( true );
            _lidTextField.setVisible( true );
            _lidLabel.setVisible( true );
        }
        else
        {
            _lidFilterCB.setVisible( false );
            _lidFilterTextBox.setVisible( false );
            _lidTextField.setVisible( false );
            _lidLabel.setVisible( false );
        }
    }
    
    private void displayFilteredDataLimitsEntries()
    {
        _filteredRowDataList = filterRowData();
        
        clearSelectedItemTextFields();

//        _statusBar.setMessage( _filteredRowDataList.size() + " entries"  );
//        System.out.println( "FilteredRowDataList has " + _filteredRowDataList.size() + " records" );
        
        _DLETableManager.setChangedAllRowDataList( _filteredRowDataList );
        
        _DLETableManager.refreshDisplay();
    }

    private List getLimitsLocLimitsCBRowDataList()
    {
        String dataLimitsString = (String) _limitsLocLimitsCB.getSelectedItem();
        List rowDataList = null;
        
        if ( dataLimitsString.equalsIgnoreCase( "Location Limits" ) )
        {
            rowDataList = _locDataLimitsRowDataList;
        }
        else
        {
            rowDataList = _dataLimitsRowDataList;
        }
        
        return rowDataList;
    }

    private List filterRowData()
    {
        List filteredRowDataList = new ArrayList();
        List rowDataList = getLimitsLocLimitsCBRowDataList();

        if ( ( _lidFilterCB.isSelected() ) ||
                ( _peFilterCB.isSelected() ) )
        {
            ArcBaseDataLimitsJTableRowData rowData = null;
            ArcBaseDataLimitsJTableRowData addRowData = null;
            
            for ( int i = 0; i < rowDataList.size(); i++ )
            {
                rowData = (ArcBaseDataLimitsJTableRowData) rowDataList.get( i );
                
                if ( ( passFilterByLid( rowData ) ) &&
                     ( passFilterByPE( rowData ) ) )
                {
                    filteredRowDataList.add( rowData );
                }
            }
        }
        else
        {
            return rowDataList;
        }
        return filteredRowDataList;
    }

    private boolean passFilterByLid( ArcBaseDataLimitsJTableRowData rowData )
    {
        boolean addRowData = false;  // Only false if the checkbox is enabled AND does not match the filter string
        
        if ( _lidFilterCB.isSelected() )
        {
            String rowDataLid = rowData.getLid().toLowerCase();
            
            String filterString = _lidFilterTextBox.getText().trim().toLowerCase();
            
            if ( rowDataLid.startsWith( filterString  ) )
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

    private boolean passFilterByPE( ArcBaseDataLimitsJTableRowData rowData )
    {
        boolean addRowData = false;  //Only false if the checkbox is enabled and is not in the PE filter list
        Object[] peJListObjectArray = null;

        if ( _peFilterCB.isSelected() )
        {
            peJListObjectArray = _peFilterJList.getSelectedValues();
            Set selectedPESet = new HashSet();
            String peString = rowData.getPe();
            
            for ( int i = 0; i < peJListObjectArray.length; i++ )
            {
                selectedPESet.add( (String) peJListObjectArray[ i ] );
            }
            
            for ( int i = 0; i < _peJLStringList.size(); i++ )
            {
                String peCBString = (String) _peJLStringList.get( i );
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
    
    private void clearSelectedItemTextFields()
    {
        _lidTextField.setText( "" );
        _startDateTextField.setText( "" );
        _endDateTextField.setText( "" );
        _grossRangeMinTF.setText( "" );
        _grossRangeMaxTF.setText( "" );
        _reasonRangeMinTF.setText( "" );
        _reasonRangeMaxTF.setText( "" );
        _rocTextField.setText( "" );
        _alertTextField.setText( "" );
        _alertRocTextField.setText( "" );
        _alarmTextField.setText( "" );
        _alarmRocTextField.setText( "" );
        _peJList.setSelectedIndex( 0 );
        _durationComboBox.setSelectedIndex( 0 );
    }

    private boolean saveToDatabase()
    {
        String dataLimitsString = (String) _limitsLocLimitsCB.getSelectedItem();
        boolean saved = false;
        
        if ( dataLimitsString.equalsIgnoreCase( "Location Limits" ) )
        {
            RaxLocDataLimits raxLocDataLimits = new RaxLocDataLimits();
            ShefPE shefPe = (ShefPE) _peJLStringToPeMap.get( (String) _peJList.getSelectedValue() );
            ShefDuration shefDur = (ShefDuration) _durCBStringToDurMap.get( (String) _durationComboBox.getSelectedItem() ); 
            
            raxLocDataLimits.setLid( _lidTextField.getText().toUpperCase() );
            raxLocDataLimits.setPe( shefPe.getPe() );
            raxLocDataLimits.setDur( shefDur.getDuration() );
            raxLocDataLimits.setIdur( shefDur.getIduration() );
            raxLocDataLimits.setMonthDayStart( _startDateTextField.getText() );
            raxLocDataLimits.setMonthDayEnd( _endDateTextField.getText() );
            raxLocDataLimits.setGrossRangeMin( getDoubleValue( _grossRangeMinTF.getText() ) );
            raxLocDataLimits.setGrossRangeMax( getDoubleValue( _grossRangeMaxTF.getText() ) );
            raxLocDataLimits.setReasonRangeMin( getDoubleValue( _reasonRangeMinTF.getText() ) );
            raxLocDataLimits.setReasonRangeMax( getDoubleValue( _reasonRangeMaxTF.getText() ) );
            raxLocDataLimits.setRoc( getDoubleValue( _rocTextField.getText() ) );
            raxLocDataLimits.setAlertLimit( getDoubleValue( _alertTextField.getText() ) );
            raxLocDataLimits.setAlertRocLimit( getDoubleValue( _alertRocTextField.getText() ) );
            raxLocDataLimits.setAlarmLimit( getDoubleValue( _alarmTextField.getText() ) );
            raxLocDataLimits.setAlarmRocLimit( getDoubleValue( _alarmRocTextField.getText() ) );

            saved = _dataMgr.saveRaxLocDataLimits( raxLocDataLimits );
            
            updateJTable( true );
        }
        else
        {
            RaxDataLimits raxDataLimits = new RaxDataLimits();
            ShefPE shefPe = (ShefPE) _peJLStringToPeMap.get( (String) _peJList.getSelectedValue() );
            ShefDuration shefDur = (ShefDuration) _durCBStringToDurMap.get( (String) _durationComboBox.getSelectedItem() ); 
            
            raxDataLimits.setPe( shefPe.getPe() );
            raxDataLimits.setDur( shefDur.getDuration() );
            raxDataLimits.setIdur( shefDur.getIduration() );
            raxDataLimits.setMonthDayStart( _startDateTextField.getText() );
            raxDataLimits.setMonthDayEnd( _endDateTextField.getText() );
            raxDataLimits.setGrossRangeMin( getDoubleValue( _grossRangeMinTF.getText() ) );
            raxDataLimits.setGrossRangeMax( getDoubleValue( _grossRangeMaxTF.getText() ) );
            raxDataLimits.setReasonRangeMin( getDoubleValue( _reasonRangeMinTF.getText() ) );
            raxDataLimits.setReasonRangeMax( getDoubleValue( _reasonRangeMaxTF.getText() ) );
            raxDataLimits.setRoc( getDoubleValue( _rocTextField.getText() ) );
            raxDataLimits.setAlertLimit( getDoubleValue( _alertTextField.getText() ) );
            raxDataLimits.setAlertRocLimit( getDoubleValue( _alertRocTextField.getText() ) );
            raxDataLimits.setAlarmLimit( getDoubleValue( _alarmTextField.getText() ) );
            raxDataLimits.setAlarmRocLimit( getDoubleValue( _alarmRocTextField.getText() ) );

            saved = _dataMgr.saveRaxDataLimits( raxDataLimits );

            updateJTable( true );
        }

        return saved;
    }
    
    private double getDoubleValue( String value )
    {
        double returnValue = DbTable.getNullDouble();
        
        if ( ! value.equalsIgnoreCase( "" ) )
        {
            returnValue = Double.parseDouble( value );
        }
        
        return returnValue;
    }
    
    private void populateDataInputPanel( RaxLocDataLimits raxLocDataLimits, boolean local )
    {
        if ( local )
        {
            _lidTextField.setText( raxLocDataLimits.getLid() );
        }
        _startDateTextField.setText( raxLocDataLimits.getMonthDayStart() );
        _endDateTextField.setText( raxLocDataLimits.getMonthDayEnd() );
        setNumberFields( _grossRangeMinTF, raxLocDataLimits.getGrossRangeMin() );
        setNumberFields( _grossRangeMaxTF, raxLocDataLimits.getGrossRangeMax() );
        setNumberFields( _reasonRangeMinTF, raxLocDataLimits.getReasonRangeMin() );
        setNumberFields( _reasonRangeMaxTF, raxLocDataLimits.getReasonRangeMax() );
        setNumberFields( _rocTextField, raxLocDataLimits.getRoc() );
        setNumberFields( _alertTextField, raxLocDataLimits.getAlertLimit() );
        setNumberFields( _alertRocTextField, raxLocDataLimits.getAlertRocLimit() );
        setNumberFields( _alarmTextField, raxLocDataLimits.getAlarmLimit() );
        setNumberFields( _alarmRocTextField, raxLocDataLimits.getAlarmRocLimit() );
                
        ShefPE shefPE = (ShefPE) _dataMgr.getShefPEMap().get( raxLocDataLimits.getPe() );
        if ( shefPE != null )
        {
            _peJList.setSelectedValue( getShefPEJListString( shefPE ), true );
        }
        else
        {
            _peJList.clearSelection();
        }
        
        ShefDuration shefDur = (ShefDuration) _dataMgr.getShefDurationMap().get( raxLocDataLimits.getDur() );
        if ( shefDur != null )
        {
            _durationComboBox.setSelectedItem( getShefDurationComboBoxString( shefDur ) );
        }
        else
        {
            _durationComboBox.setSelectedItem( "" );
        }
        
    }
    
    private void setNumberFields( JTextField txtField, double doubleValue )
    {
        if ( DbTable.isNull( doubleValue ) )
        {
            txtField.setText( "" );
        }
        else
        {
            txtField.setText( Double.toString( doubleValue ) );
        }
    }
    
    private void deleteDataLimits()
    {
        String dataLimitsString = (String) _limitsLocLimitsCB.getSelectedItem();
        boolean delete = false;
        
        if ( dataLimitsString.equalsIgnoreCase( "Location Limits" ) )
        {
            RaxLocDataLimits raxLocDataLimits = null;
            List selectedRowsData = _DLETableManager.getSelectedRowsData();
            
            if ( ! selectedRowsData.isEmpty() )
            {
                ArcBaseDataLimitsJTableRowData rowData = (ArcBaseDataLimitsJTableRowData) selectedRowsData.get( 0 );
                raxLocDataLimits = (RaxLocDataLimits) _raxLocDLRowDataToRaxLocDLMap.get( rowData );
                if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + raxLocDataLimits.keyString(), "Delete LocDataLimit" ) )
                {
                    _dataMgr.deleteRaxLocDataLimitsFromDatabase( raxLocDataLimits );
                    clearSelectedItemTextFields();
                    updateJTable( true );
                }
            }
            else
            {
                DialogHelper.displayErrorDialog( this, "You must select a Local Data Limit", "Delete LocDataLimit" );
            }
        }
        else
        {
            RaxDataLimits raxDataLimits = null;
            List selectedRowsData = _DLETableManager.getSelectedRowsData();
            
            if ( ! selectedRowsData.isEmpty() )
            {
                ArcBaseDataLimitsJTableRowData rowData = (ArcBaseDataLimitsJTableRowData) selectedRowsData.get( 0 );
                raxDataLimits = (RaxDataLimits) _raxDLRowDataToRaxDLMap.get( rowData );
                if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + raxDataLimits.keyString(), "Delete DataLimit" ) )
                {
                    _dataMgr.deleteRaxDataLimitsFromDatabase( raxDataLimits );
                    clearSelectedItemTextFields();
                    updateJTable( true );
                }
            }
            else
            {
                DialogHelper.displayErrorDialog( this, "You must select a Data Limit", "Delete DataLimit" );
            }
        }
    }

    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();

        _limitsLocLimitsCB.addActionListener( new LimitsLocLimitsComboBoxListener() );
        _lidFilterCB.addActionListener( new LidFilterCheckBoxListener() );
        _peFilterCB.addActionListener( new PEFilterCheckBoxListener() );
        _lidFilterTextBox.addKeyListener( new LidFilterTextFieldListener() );
        _peFilterJList.addListSelectionListener( new PEFilterJListListener() );
        
        _DLETableManager.addTableListener( new DLETableListener() );
        
        _closeButton.addActionListener( windowCloser );
        _clearButton.addActionListener( new ClearButtonListener() );
        _saveButton.addActionListener( new SaveButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
        
        addWindowListener( windowCloser );
 
    }

    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteDataLimits();
        }
    }
    
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveToDatabase();
        }
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveToDatabase() )   // If the save is successful, close the window
            {
                closeWindow();
            }
        }
    }
    
    private class ClearButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            clearSelectedItemTextFields();
        }
    }
    
    private class PEFilterJListListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                displayFilteredDataLimitsEntries();
            }
        }
    }
    
    private class DLETableListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                String dataLimitsString = (String) _limitsLocLimitsCB.getSelectedItem();
                List rowDataList = null;
                
                if ( dataLimitsString.equalsIgnoreCase( "Location Limits" ) )
                {
                    RaxLocDataLimits raxLocDataLimits = null;

                    ArcBaseDataLimitsJTableRowData rowData = (ArcBaseDataLimitsJTableRowData) _DLETableManager.getSelectedRowsData().get( 0 );
                    
                    raxLocDataLimits = (RaxLocDataLimits) _raxLocDLRowDataToRaxLocDLMap.get( rowData );
                    
                    populateDataInputPanel( raxLocDataLimits, true );
                }
                else
                {
                    RaxLocDataLimits raxLocDataLimits = null;
                    RaxDataLimits raxDataLimits = null;
                    
                    ArcBaseDataLimitsJTableRowData rowData = (ArcBaseDataLimitsJTableRowData) _DLETableManager.getSelectedRowsData().get( 0 );
                    
                    raxDataLimits = (RaxDataLimits) _raxDLRowDataToRaxDLMap.get( rowData );
                    raxLocDataLimits = new RaxLocDataLimits( raxDataLimits );
                    
                    populateDataInputPanel( raxLocDataLimits, false );
                }

            }
        }
    }

    private class PEFilterCheckBoxListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            boolean enabled = _peFilterCB.isSelected();
            if ( enabled )
            {
                activatePEFilter( true );
            }
            else
            {
                activatePEFilter( false );
                _peFilterJList.clearSelection();
                displayFilteredDataLimitsEntries();
            }
        }
    }
    
    private class LidFilterCheckBoxListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            boolean enabled = _lidFilterCB.isSelected();
            if ( enabled )
            {
                activateLidFilter( true );
            }
            else
            {
                activateLidFilter( false );
                _lidFilterTextBox.setText( "" );
                displayFilteredDataLimitsEntries();
            }
        }
    }
    private class LimitsLocLimitsComboBoxListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            activateLidFilter( false );
            activatePEFilter( false );
            _lidFilterCB.setSelected( false );
            _peFilterCB.setSelected( false );
            _lidFilterTextBox.setText( "" );
            boolean refresh = false;
            
//            updateJTable( refresh );

            String dataLimitsString = (String) _limitsLocLimitsCB.getSelectedItem();
            
            if ( dataLimitsString.equalsIgnoreCase( "Location Limits" ) )
            {
                enableLocationInColumnDescriptorList( true );
                _DLETableManager.setChangedAllRowDataList( _locDataLimitsRowDataList );
                enableLidFilter( true );
                String columnsSelected[] = getFixedColumnOrderStringArray( _DLETableManager.getColumnNamesThatAreCurrentlyDisplayed() );
                _DLETableManager.setColumnsForDisplay( columnsSelected );
            }
            else
            {
                enableLocationInColumnDescriptorList( false );
                _DLETableManager.setChangedAllRowDataList( _dataLimitsRowDataList );
                enableLidFilter( false );
            }
            _DLETableManager.refreshDisplay();
        }
    }

    private String[] getFixedColumnOrderStringArray( String[] columnStringArray )
    {
        String[] columnsSelected = new String[ columnStringArray.length ];
        
        columnsSelected[ 0 ] = "Location";
        
        for ( int i = 1; i < columnStringArray.length; i++ )
        {
            columnsSelected[ i ] = columnStringArray[ i - 1 ];
        }
        
        return columnsSelected;
    }
    
    
    private String[] getColumnDescriptorStringArray()
    {
        String[] columnDescriptorArray = null;
        
        if ( _DLEColumnDescriptorList != null )
        {
            columnDescriptorArray = new String[ _DLEColumnDescriptorList.size() ];
            JTableColumnDescriptor colDesc = null;

            for ( int i = 0; i < _DLEColumnDescriptorList.size(); i++ )
            {
                colDesc = (JTableColumnDescriptor) _DLEColumnDescriptorList.get( i );
                columnDescriptorArray[ i ] = colDesc.getColumnName();
            }
        }
        return columnDescriptorArray;
    }
    
    
    private class LidFilterTextFieldListener implements KeyListener
    {
        public void keyPressed( KeyEvent e )
        {
        }

        public void keyReleased( KeyEvent e )
        {
            if ( e.getKeyCode() != 16 ) // Shift key
            {
                displayFilteredDataLimitsEntries();
            }
        }

        public void keyTyped( KeyEvent e )
        {
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
        RaxBaseDataMgr dataMgr = new RaxBaseDataMgr( "jdbc:postgresql://ax2:5432/adb_ob82krf?user=pguser", "jdbc:postgresql://lx5:5432/hd_ob83raxtest?user=pguser adb_ob83raxtest", "/fs/home/gsood/" );
        DataLimitsEditor dataLimitsEditorGUI = new DataLimitsEditor( frame, dataMgr );
        dataLimitsEditorGUI.displayGUI();
    }
}
