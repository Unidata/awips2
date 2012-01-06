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
import ohd.hseb.raxbase.model.State;
import ohd.hseb.raxbase.table.ArcBaseStateJTableRowData;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledTextField;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class StateEditor extends JDialog
{
    private Container _dialogContentPane = getContentPane();
    
    private RaxBaseDataMgr _dataMgr = null;
    
//  JTable variables
    private JTableManager _stateTableManager = null;
    private List _stateColumnDescriptorList = null;
    private List _stateRowDataList = null;
    private JScrollPane _tableScrollPane = null;
    private JPanel _stateSelectionPanel = new JPanel( new GridBagLayout() );

    private JPanel _selectedItemPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _stateLTF = new LabeledTextField( "State:  ", "", "State", 3 );
    private LabeledTextField _countryFipsLTF = new LabeledTextField( "CountryFips:  ", "", "2 char country FIPS code (for \"counties\" table the default value is \"US\") (upper case)", 3 );
    private LabeledTextField _nameLTF = new LabeledTextField( "Name:  ", "", "Name of state (mixed case)", 20 );
    private LabeledTextField _ncdcLTF = new LabeledTextField( "NCDC:  ", "", "NCDC number for state", 3 );
    private LabeledTextField _stateFipsLTF = new LabeledTextField( "StateFips:  ", "", "State FIPS code", 3 );
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _deleteButton = new JButton( "Delete" );

    private Map _stateRowDataToStateMap = null;

    public StateEditor( JFrame frame, RaxBaseDataMgr dataMgr )
    {
        super( frame, "State Editor", true );
        _dataMgr = dataMgr;
    }
    
    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 370, 500 ) );
        initGUI();
    }
    
    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        initStateSelectionPanel();
        initSelectedItemPanel();
        initButtonPanel();
        initFrameComponents();
        initHashMaps();
        addListeners();
        pack();
        setVisible( true );
    }
    
    private void initStateSelectionPanel()
    {
        initJTable();
    }
    
    private void initFrameComponents()
    {
//                                                                                         X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _stateSelectionPanel,     0,  0,     4,     20, 1, 1, GridBagConstraints.BOTH );
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
        _selectedItemPanel.setBorder( BorderFactory.createTitledBorder( "Info for Selected State" ) );
        JPanel vPanel = new JPanel();
        
//                                                                                X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _selectedItemPanel, _stateLTF,         0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _countryFipsLTF,   0,  1,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _nameLTF,          0,  2,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _ncdcLTF,          0,  3,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _stateFipsLTF,     0,  4,    1,     1, GridBagConstraints.NONE );
    }


    private void initHashMaps()
    {
        _stateRowDataToStateMap = _dataMgr.getStateRowDataToStateMap();
    }

    private void initJTable() 
    {
        _stateRowDataList = _dataMgr.getStateRowDataList();
        setStateEColumnDescriptorList();
        _stateTableManager = new ComplexJTableManager( _stateColumnDescriptorList, _stateRowDataList, ListSelectionModel.SINGLE_SELECTION );
        String columnsSelected[] = _stateTableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _stateTableManager.setDisplayableColumns( columnsSelected, true, true );
        _tableScrollPane = _stateTableManager.getJScrollPane();
        _tableScrollPane.setPreferredSize( new Dimension( 300, 100 ) );
//                                                                                    X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _stateSelectionPanel, _tableScrollPane,    0,   1,    1,     1, 1, 1, GridBagConstraints.BOTH );
    }

    private void updateJTable()
    {
        _stateRowDataList = _dataMgr.getStateRowDataList();
        _stateTableManager.setChangedAllRowDataList( _stateRowDataList );
        _stateTableManager.refreshDisplay();
    }

    private void setStateEColumnDescriptorList()
    {
        _stateColumnDescriptorList = new ArrayList();

        _stateColumnDescriptorList.add(new JTableColumnDescriptor( "State", true, 50, "center" ) );
        _stateColumnDescriptorList.add(new JTableColumnDescriptor( "CountryFips", true, 60, "center" ) );
        _stateColumnDescriptorList.add(new JTableColumnDescriptor( "Name", true, 120, "center" ) );
        _stateColumnDescriptorList.add(new JTableColumnDescriptor( "Ncdc", true, 50, "center" ) );
        _stateColumnDescriptorList.add(new JTableColumnDescriptor( "StateFips", true, 60, "center" ) );
    }
    
    private void populateDataInputPanel( State state )
    {
        if ( state != null )
        {
            _stateLTF.setTextField( state.getState() );
            _countryFipsLTF.setTextField( state.getCountryFips() );
            _nameLTF.setTextField( state.getName() );
            _ncdcLTF.setTextField( state.getNcdc() );
            _stateFipsLTF.setTextField( state.getStateFips() );
        }
    }
    
    private void clearDataInputPanel()
    {
        _stateLTF.setTextField( "" );
        _countryFipsLTF.setTextField( "" );
        _nameLTF.setTextField( "" );
        _ncdcLTF.setTextField( "" );
        _stateFipsLTF.setTextField( "" );
    }
    
    private boolean saveState()
    {
        boolean saved = false;
        
        State state = new State();

        state.setState( _stateLTF.getTextFieldText() );
        state.setCountryFips( _countryFipsLTF.getTextFieldText() );
        state.setName( _nameLTF.getTextFieldText() );
        state.setNcdc( _ncdcLTF.getTextFieldText() );
        state.setStateFips( _stateFipsLTF.getTextFieldText() );
        
        saved = _dataMgr.saveState( state );
        
        updateJTable();

        return saved;
    }
    
    private void deleteState()
    {
        State state = null;
        List selectedRowsData = _stateTableManager.getSelectedRowsData();
        
        if ( ! selectedRowsData.isEmpty() )
        {
            ArcBaseStateJTableRowData rowData = (ArcBaseStateJTableRowData) selectedRowsData.get( 0 );
            state = (State) _stateRowDataToStateMap.get( rowData );
            if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + state.keyString(), "Delete State" ) )
            {
                _dataMgr.deleteStateFromDataBase( state );
                clearDataInputPanel();
                updateJTable();
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select a State", "Delete State" );
        }
    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();

        _closeButton.addActionListener( windowCloser );
        addWindowListener( windowCloser );
        _stateTableManager.addTableListener( new MCETableListener() );
        
        _saveButton.addActionListener( new SaveButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
    }
    
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveState();
        }
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveState() )   // If the save is successful, close the window
            {
                closeWindow();
            }
        }
    }
    
    
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteState();
        }
    }
    
    private class MCETableListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                State state = null;

                ArcBaseStateJTableRowData rowData = (ArcBaseStateJTableRowData) _stateTableManager.getSelectedRowsData().get( 0 );
                state = (State) _stateRowDataToStateMap.get( rowData );
                populateDataInputPanel( state );
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
        RaxBaseDataMgr dataMgr = new RaxBaseDataMgr( "jdbc:postgresql://ax2:5432/adb_ob72krf?user=pguser" );

        StateEditor stateEditor = new StateEditor( frame, dataMgr );
        stateEditor.displayGUI();
    }
}
