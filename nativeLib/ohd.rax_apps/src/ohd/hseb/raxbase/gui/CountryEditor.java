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
import ohd.hseb.raxbase.model.Country;
import ohd.hseb.raxbase.table.ArcBaseCountryJTableRowData;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledTextField;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class CountryEditor extends JDialog
{
    private Container _dialogContentPane = getContentPane();
    
    private RaxBaseDataMgr _dataMgr = null;
    
//  JTable variables
    private JTableManager _CETableManager = null;
    private List _CEColumnDescriptorList = null;
    private List _countryRowDataList = null;
    private JScrollPane _tableScrollPane = null;
    private JPanel _countrySelectionPanel = new JPanel( new GridBagLayout() );

    private JPanel _selectedItemPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _countryLTF = new LabeledTextField( "Country:  ", "", "Country", 10 );
    private LabeledTextField _countryFipsLTF = new LabeledTextField( "CountryFips:  ", "", "Country", 10 );
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _deleteButton = new JButton( "Delete" );

    private Map _countryRowDataToCountryMap = null;

    public CountryEditor( JFrame frame, RaxBaseDataMgr dataMgr )
    {
        super( frame, "Country Editor", true );
        _dataMgr = dataMgr;
    }
    
    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 310, 400 ) );
        initGUI();
    }
    
    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        initCountrySelectionPanel();
        initSelectedItemPanel();
        initButtonPanel();
        initFrameComponents();
        initHashMaps();
        addListeners();
        pack();
        setVisible( true );
    }
    
    private void initCountrySelectionPanel()
    {
        initJTable();
    }
    
    private void initFrameComponents()
    {
//                                                                                         X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _countrySelectionPanel,     0,  0,     4,     20, 1, 1, GridBagConstraints.BOTH );
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
        _selectedItemPanel.setBorder( BorderFactory.createTitledBorder( "Info for Selected Country" ) );
        JPanel vPanel = new JPanel();
        
//                                                                                X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _selectedItemPanel, _countryLTF,       0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _countryFipsLTF,   0,  1,    1,     1, GridBagConstraints.NONE );
    }


    private void initHashMaps()
    {
        _countryRowDataToCountryMap = _dataMgr.getCountryRowDataToCountryMap();
    }

    private void initJTable() 
    {
        _countryRowDataList = _dataMgr.getCountryRowDataList();
        setCEColumnDescriptorList();
        _CETableManager = new ComplexJTableManager( _CEColumnDescriptorList, _countryRowDataList, ListSelectionModel.SINGLE_SELECTION );
        String columnsSelected[] = _CETableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _CETableManager.setDisplayableColumns( columnsSelected, true, true );
        _tableScrollPane = _CETableManager.getJScrollPane();
        _tableScrollPane.setPreferredSize( new Dimension( 300, 100 ) );
//                                                                                    X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _countrySelectionPanel, _tableScrollPane,    0,   1,    1,     1, 1, 1, GridBagConstraints.BOTH );
    }

    private void updateJTable()
    {
        _countryRowDataList = _dataMgr.getCountryRowDataList();
        _CETableManager.setChangedAllRowDataList( _countryRowDataList );
        _CETableManager.refreshDisplay();
    }

    private void setCEColumnDescriptorList()
    {
        _CEColumnDescriptorList = new ArrayList();

        _CEColumnDescriptorList.add(new JTableColumnDescriptor( "Country", true, 120, "center" ) );
        _CEColumnDescriptorList.add(new JTableColumnDescriptor( "CountryFips", true, 120, "center" ) );
    }
    
    private void populateDataInputPanel( Country country )
    {
        if ( country != null )
        {
            _countryLTF.setTextField( country.getCountry() );
            _countryFipsLTF.setTextField( country.getCountryFips() );
        }
    }
    
    private void clearDataInputPanel()
    {
        _countryLTF.setTextField( "" );
        _countryFipsLTF.setTextField( "" );
    }
    
    private boolean saveCountry()
    {
        boolean saved = false;
        
        Country country = new Country();

        country.setCountry( _countryLTF.getTextFieldText() );
        country.setCountryFips( _countryFipsLTF.getTextFieldText() );
        
        saved = _dataMgr.saveCountry( country );
        
        updateJTable();

        return saved;
    }
    
    private void deleteCountry()
    {
        Country country = null;
        List selectedRowsData = _CETableManager.getSelectedRowsData();
        
        if ( ! selectedRowsData.isEmpty() )
        {
            ArcBaseCountryJTableRowData rowData = (ArcBaseCountryJTableRowData) selectedRowsData.get( 0 );
            country = (Country) _countryRowDataToCountryMap.get( rowData );
            if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + country.keyString(), "Delete Country" ) )
            {
                _dataMgr.deleteCountryFromDataBase( country );
                clearDataInputPanel();
                updateJTable();
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select a Country", "Delete Country" );
        }
    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();

        _closeButton.addActionListener( windowCloser );
        addWindowListener( windowCloser );
        _CETableManager.addTableListener( new MCETableListener() );
        
        _saveButton.addActionListener( new SaveButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
    }
    
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveCountry();
        }
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveCountry() )   // If the save is successful, close the window
            {
                closeWindow();
            }
        }
    }
    
    
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteCountry();
        }
    }
    
    private class MCETableListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                Country country = null;

                ArcBaseCountryJTableRowData rowData = (ArcBaseCountryJTableRowData) _CETableManager.getSelectedRowsData().get( 0 );
                country = (Country) _countryRowDataToCountryMap.get( rowData );
                populateDataInputPanel( country );
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

        CountryEditor countryEditor = new CountryEditor( frame, dataMgr );
        countryEditor.displayGUI();
    }
}
