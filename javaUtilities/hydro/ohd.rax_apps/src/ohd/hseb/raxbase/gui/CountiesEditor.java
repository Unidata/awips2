package ohd.hseb.raxbase.gui;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import ohd.hseb.raxbase.RaxBaseDataMgr;
import ohd.hseb.raxbase.model.Counties;
import ohd.hseb.raxbase.table.ArcBaseCountiesJTableRowData;
import ohd.hseb.raxdb.generated.StateRecord;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledComboBox;
import ohd.hseb.util.gui.LabeledTextField;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class CountiesEditor extends JDialog
{
    private Container _dialogContentPane = getContentPane();
    
    private RaxBaseDataMgr _dataMgr = null;
    
//  JTable variables
    private JTableManager _countiesTableManager = null;
    private List _countiesColumnDescriptorList = null;
    private List _countiesRowDataList = null;
    private JScrollPane _tableScrollPane = null;
    private JPanel _countiesSelectionPanel = new JPanel( new GridBagLayout() );

    private JPanel _selectedItemPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _countyLTF = new LabeledTextField( "County:  ", "", "County", 23 );
    private LabeledComboBox _stateCountryFipsLCB = new LabeledComboBox( "State/CountryFips:" );
    private LabeledTextField _countyFipsLTF = new LabeledTextField( "CountyFips:  ", "", "3 char county FIPS code", 4 );
    private LabeledComboBox _wfoLCB = new LabeledComboBox( "Wfo:" );
    private LabeledTextField _zonLTF = new LabeledTextField( "Zon:  ", "", "NWS zone code", 5 );
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _deleteButton = new JButton( "Delete" );

    private Map _countiesRowDataToCountiesMap = null;

    public CountiesEditor( JFrame frame, RaxBaseDataMgr dataMgr )
    {
        super( frame, "Counties Editor", true );
        _dataMgr = dataMgr;
    }
    
    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 520, 500 ) );
        initGUI();
    }
    
    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        initComboBoxes();
        initCountiesSelectionPanel();
        initSelectedItemPanel();
        initButtonPanel();
        initFrameComponents();
        initHashMaps();
        addListeners();
        pack();
        setVisible( true );
    }
    
    private void initCountiesSelectionPanel()
    {
        initJTable();
    }
    
    private void initFrameComponents()
    {
//                                                                                         X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _countiesSelectionPanel,     0,  0,     4,     20, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _selectedItemPanel,         0,  21,    4,     1, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _buttonPanel,               0,  22,    4,     1, 1, 1, GridBagConstraints.BOTH );

    }
    private void initButtonPanel()
    {
//                                                                              X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _buttonPanel, _saveAndCloseButton,   0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _saveButton,           1,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _deleteButton,         2,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _closeButton,          1,  1,    1,     1, GridBagConstraints.NONE );
    }

    private void initSelectedItemPanel()
    {
        _selectedItemPanel.setBorder( BorderFactory.createTitledBorder( "Info for Selected County" ) );
        JPanel vPanel = new JPanel();
        
//                                                                                    X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _selectedItemPanel, _countyLTF,            0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _stateCountryFipsLCB,  0,  2,    1,     1, GridBagConstraints.NONE );
        
        ComponentHelper.addPanelComponent( _selectedItemPanel, _countyFipsLTF,        0,  3,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _wfoLCB,               0,  4,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _zonLTF,               0,  5,    1,     1, GridBagConstraints.NONE );
    }


    private void initHashMaps()
    {
        _countiesRowDataToCountiesMap = _dataMgr.getCountiesRowDataToCountiesMap();
    }

    private void initComboBoxes()
    {
        _stateCountryFipsLCB.setComboBox( _dataMgr.getStateCountryFipsList() );
        _wfoLCB.setComboBox( _dataMgr.getWfoList() );
    }
    
    private void initJTable() 
    {
        _countiesRowDataList = _dataMgr.getCountiesRowDataList();
        setCountiesEColumnDescriptorList();
        _countiesTableManager = new ComplexJTableManager( _countiesColumnDescriptorList, _countiesRowDataList, ListSelectionModel.SINGLE_SELECTION );
        String columnsSelected[] = _countiesTableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _countiesTableManager.setDisplayableColumns( columnsSelected, true, true );
        _tableScrollPane = _countiesTableManager.getJScrollPane();
        _tableScrollPane.setPreferredSize( new Dimension( 300, 100 ) );
//                                                                                    X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _countiesSelectionPanel, _tableScrollPane,    0,   1,    1,     1, 1, 1, GridBagConstraints.BOTH );
    }

    private void updateJTable()
    {
        _countiesRowDataList = _dataMgr.getCountiesRowDataList();
        _countiesTableManager.setChangedAllRowDataList( _countiesRowDataList );
        _countiesTableManager.refreshDisplay();
    }

    private void setCountiesEColumnDescriptorList()
    {
        _countiesColumnDescriptorList = new ArrayList();

        _countiesColumnDescriptorList.add(new JTableColumnDescriptor( "County", true, 150, "center" ) );
        _countiesColumnDescriptorList.add(new JTableColumnDescriptor( "State", true, 60, "center" ) );
        _countiesColumnDescriptorList.add(new JTableColumnDescriptor( "CountryFips", true, 80, "center" ) );
        _countiesColumnDescriptorList.add(new JTableColumnDescriptor( "CountyFips", true, 80, "center" ) );
        _countiesColumnDescriptorList.add(new JTableColumnDescriptor( "WFO", true, 60, "center" ) );
        _countiesColumnDescriptorList.add(new JTableColumnDescriptor( "ZON", true, 60, "center" ) );
    }
    
    private void populateDataInputPanel( Counties counties )
    {
        if ( counties != null )
        {
            _countyLTF.setTextField( counties.getCounty() );
            _stateCountryFipsLCB.setSelectedItem( _dataMgr.getStateCountryFipsString( counties ) );
            _countyFipsLTF.setTextField( counties.getCountyFips() );
            _wfoLCB.setSelectedItem( counties.getWfo() );
            _zonLTF.setTextField( counties.getZon() );
        }
    }
    
    private void clearDataInputPanel()
    {
        _countyLTF.setTextField( "" );
        _stateCountryFipsLCB.setSelectedIndex( 0 );
        _countyFipsLTF.setTextField( "" );
        _wfoLCB.setSelectedIndex( 0 );
        _zonLTF.setTextField( "" );
    }
    
    private boolean saveCounties()
    {
        boolean saved = false;
        Map stateCountryFipsStringToStateRecordMap = _dataMgr.getStateCountryFipsStringToStateMap();
        String stateCountryFipsSelectedItem = _stateCountryFipsLCB.getSelectedCBItem();
        
        StateRecord record = (StateRecord) stateCountryFipsStringToStateRecordMap.get( stateCountryFipsSelectedItem );
        Counties counties = new Counties();

        counties.setCounty( _countyLTF.getTextFieldText() );

        counties.setState( record.getState() );
        counties.setCountryFips( record.getCountryfips() );
        
        
        counties.setCountyFips( _countyFipsLTF.getTextFieldText() );
        counties.setWfo( _wfoLCB.getSelectedCBItem() );
        counties.setZon( _zonLTF.getTextFieldText() );
        
        saved = _dataMgr.saveCounties( counties );
        
        updateJTable();

        return saved;
    }
    
    private void deleteCounties()
    {
        Counties counties = null;
        List selectedRowsData = _countiesTableManager.getSelectedRowsData();
        
        if ( ! selectedRowsData.isEmpty() )
        {
            ArcBaseCountiesJTableRowData rowData = (ArcBaseCountiesJTableRowData) selectedRowsData.get( 0 );
            counties = (Counties) _countiesRowDataToCountiesMap.get( rowData );
            if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + counties.keyString(), "Delete County" ) )
            {
                _dataMgr.deleteCountiesFromDataBase( counties );
                clearDataInputPanel();
                updateJTable();
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select a County", "Delete County" );
        }
    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();

        _closeButton.addActionListener( windowCloser );
        addWindowListener( windowCloser );
        _countiesTableManager.addTableListener( new MCETableListener() );
        
        _saveButton.addActionListener( new SaveButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
    }
    
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveCounties();
        }
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveCounties() )   // If the save is successful, close the window
            {
                closeWindow();
            }
        }
    }
    
    
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteCounties();
        }
    }
    
    private class MCETableListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                Counties counties = null;

                ArcBaseCountiesJTableRowData rowData = (ArcBaseCountiesJTableRowData) _countiesTableManager.getSelectedRowsData().get( 0 );
                counties = (Counties) _countiesRowDataToCountiesMap.get( rowData );
                populateDataInputPanel( counties );
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
        RaxBaseDataMgr dataMgr = new RaxBaseDataMgr( "jdbc:postgresql://ax2:5432/adb_ob82krf?user=pguser", "", "/fs/home/gsood/" );

        CountiesEditor countiesEditor = new CountiesEditor( frame, dataMgr );
        countiesEditor.displayGUI();
    }
}
