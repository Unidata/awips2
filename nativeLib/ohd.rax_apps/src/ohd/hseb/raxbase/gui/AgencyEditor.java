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
import ohd.hseb.raxbase.model.Agency;
import ohd.hseb.raxbase.table.ArcBaseAgencyJTableRowData;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledTextField;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class AgencyEditor extends JDialog
{
    private Container _dialogContentPane = getContentPane();
    
    private RaxBaseDataMgr _dataMgr = null;
    
//  JTable variables
    private JTableManager _agencyTableManager = null;
    private List _agencyColumnDescriptorList = null;
    private List _agencyRowDataList = null;
    private JScrollPane _tableScrollPane = null;
    private JPanel _agencySelectionPanel = new JPanel( new GridBagLayout() );

    private JPanel _selectedItemPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _agCodeLTF = new LabeledTextField( "AgencyCode:", "", "Abbreviation for agency", 7 );
    private LabeledTextField _agLocLTF = new LabeledTextField( "AgencyLoc:", "", "Location of agency", 4 );
    private LabeledTextField _descLTF = new LabeledTextField( "Desc:", "", "Description (mixed case)", 21 );
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _deleteButton = new JButton( "Delete" );

    private Map _agencyRowDataToAgencyMap = null;

    public AgencyEditor( JFrame frame, RaxBaseDataMgr dataMgr )
    {
        super( frame, "Agency Editor", true );
        _dataMgr = dataMgr;
    }
    
    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 340, 400 ) );
        initGUI();
    }
    
    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        initAgencySelectionPanel();
        initSelectedItemPanel();
        initButtonPanel();
        initFrameComponents();
        initHashMaps();
        addListeners();
        pack();
        setVisible( true );
    }
    
    private void initAgencySelectionPanel()
    {
        initJTable();
    }
    
    private void initFrameComponents()
    {
//                                                                                         X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _agencySelectionPanel,     0,  0,     4,     20, 1, 1, GridBagConstraints.BOTH );
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
        _selectedItemPanel.setBorder( BorderFactory.createTitledBorder( "Info for Selected ShefTS" ) );
        JPanel vPanel = new JPanel();
        
//                                                                           X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _selectedItemPanel, _agCodeLTF,   0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _agLocLTF,    0,  1,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _descLTF,     0,  2,    1,     1, GridBagConstraints.NONE );
    }


    private void initHashMaps()
    {
        _agencyRowDataToAgencyMap = _dataMgr.getAgencyRowDataToAgencyMap();
    }

    private void initJTable() 
    {
        _agencyRowDataList = _dataMgr.getAgencyRowDataList();
        setCEColumnDescriptorList();
        _agencyTableManager = new ComplexJTableManager( _agencyColumnDescriptorList, _agencyRowDataList, ListSelectionModel.SINGLE_SELECTION );
        String columnsSelected[] = _agencyTableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _agencyTableManager.setDisplayableColumns( columnsSelected, true, true );
        _tableScrollPane = _agencyTableManager.getJScrollPane();
        _tableScrollPane.setPreferredSize( new Dimension( 100, 100 ) );
//                                                                                    X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _agencySelectionPanel, _tableScrollPane,    0,   1,    1,     1, 1, 1, GridBagConstraints.BOTH );
    }

    private void updateJTable()
    {
        _agencyRowDataList = _dataMgr.getAgencyRowDataList();
        _agencyTableManager.setChangedAllRowDataList( _agencyRowDataList );
        _agencyTableManager.refreshDisplay();
    }

    private void setCEColumnDescriptorList()
    {
        _agencyColumnDescriptorList = new ArrayList();

        _agencyColumnDescriptorList.add(new JTableColumnDescriptor( "AgCode", true, 50, "center" ) );
        _agencyColumnDescriptorList.add(new JTableColumnDescriptor( "AgLoc", true, 50, "center" ) );
        _agencyColumnDescriptorList.add(new JTableColumnDescriptor( "Desc", true, 200, "center" ) );
    }
    
    private void populateDataInputPanel( Agency agency )
    {
        if ( agency != null )
        {
            _agCodeLTF.setTextField( agency.getAgCode() );
            _agLocLTF.setTextField( agency.getAgLoc() );
            _descLTF.setTextField( agency.getDes() );
        }
    }
    
    private void clearDataInputPanel()
    {
        _agCodeLTF.setTextField( "" );
        _agLocLTF.setTextField( "" );
        _descLTF.setTextField( "" );
    }
    
    private boolean saveAgency()
    {
        boolean saved = false;
        
        if ( ! ( ( _agCodeLTF.getTextFieldText().equalsIgnoreCase( "" ) ) ||
                ( _agLocLTF.getTextFieldText().equalsIgnoreCase( "" ) ) ) )
        {
            if ( _descLTF.getTextFieldText().length() <= 20 )
            {
                Agency agency = new Agency();

                agency.setAgCode( _agCodeLTF.getTextFieldText() );
                agency.setAgLoc( _agLocLTF.getTextFieldText() );
                agency.setDes( _descLTF.getTextFieldText() );

                saved = _dataMgr.saveAgency( agency );

                updateJTable();
            }
            else
            {
                DialogHelper.displayErrorDialog( this, "Please limit the description to 20 characters", "Error saving Agency" );
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "Please enter a value for AgLoc/AgCode", "Save Agency" );
        }

        return saved;
    }
    
    private void deleteAgency()
    {
        Agency agency = null;
        List selectedRowsData = _agencyTableManager.getSelectedRowsData();
        
        if ( ! selectedRowsData.isEmpty() )
        {
            ArcBaseAgencyJTableRowData rowData = (ArcBaseAgencyJTableRowData) selectedRowsData.get( 0 );
            agency = (Agency) _agencyRowDataToAgencyMap.get( rowData );
            if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + agency.keyString(), "Delete Agency" ) )
            {
                _dataMgr.deleteAgencyFromDataBase( agency );
                clearDataInputPanel();
                updateJTable();
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select an Agency", "Delete Agency" );
        }
    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();

        _closeButton.addActionListener( windowCloser );
        addWindowListener( windowCloser );
        _agencyTableManager.addTableListener( new MCETableListener() );
        
        _saveButton.addActionListener( new SaveButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
    }
    
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveAgency();
        }
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveAgency() )   // If the save is successful, close the window
            {
                closeWindow();
            }
        }
    }
    
    
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteAgency();
        }
    }
    
    private class MCETableListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                Agency agency = null;

                ArcBaseAgencyJTableRowData rowData = (ArcBaseAgencyJTableRowData) _agencyTableManager.getSelectedRowsData().get( 0 );
                agency = (Agency) _agencyRowDataToAgencyMap.get( rowData );
                populateDataInputPanel( agency );
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

        AgencyEditor agencyEditor = new AgencyEditor( frame, dataMgr );
        agencyEditor.displayGUI();
    }
}
