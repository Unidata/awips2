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
import ohd.hseb.raxbase.model.ShefTS;
import ohd.hseb.raxbase.table.ArcBaseShefTSJTableRowData;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledTextField;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class ShefTSEditor extends JDialog
{
    private Container _dialogContentPane = getContentPane();
    
    private RaxBaseDataMgr _dataMgr = null;
    
//  JTable variables
    private JTableManager _shefTSTableManager = null;
    private List _shefTSColumnDescriptorList = null;
    private List _shefTSRowDataList = null;
    private JScrollPane _tableScrollPane = null;
    private JPanel _shefTSSelectionPanel = new JPanel( new GridBagLayout() );

    private JPanel _selectedItemPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _tsLTF = new LabeledTextField( "TS:", "", "SHEF TypeSource Code", 3 );
    private LabeledTextField _nameLTF = new LabeledTextField( "Name:", "", "Description (mixed case)", 21 );
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _deleteButton = new JButton( "Delete" );

    private Map _shefTsRowDataToShefTsMap = null;

    public ShefTSEditor( JFrame frame, RaxBaseDataMgr dataMgr )
    {
        super( frame, "ShefTS Editor", true );
        _dataMgr = dataMgr;
    }
    
    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 300, 400 ) );
        initGUI();
    }
    
    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        initShefTSSelectionPanel();
        initSelectedItemPanel();
        initButtonPanel();
        initFrameComponents();
        initHashMaps();
        addListeners();
        pack();
        setVisible( true );
    }
    
    private void initShefTSSelectionPanel()
    {
        initJTable();
    }
    
    private void initFrameComponents()
    {
//                                                                                         X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _shefTSSelectionPanel,     0,  0,     4,     20, 1, 1, GridBagConstraints.BOTH );
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
        ComponentHelper.addPanelComponent( _selectedItemPanel, _tsLTF, 0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _nameLTF,     0,  1,    1,     1, GridBagConstraints.NONE );
    }


    private void initHashMaps()
    {
        _shefTsRowDataToShefTsMap = _dataMgr.getShefTsRowDataToShefTsMap();
    }

    private void initJTable() 
    {
        _shefTSRowDataList = _dataMgr.getShefTsRowDataList();
        setCEColumnDescriptorList();
        _shefTSTableManager = new ComplexJTableManager( _shefTSColumnDescriptorList, _shefTSRowDataList, ListSelectionModel.SINGLE_SELECTION );
        String columnsSelected[] = _shefTSTableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _shefTSTableManager.setDisplayableColumns( columnsSelected, true, true );
        _tableScrollPane = _shefTSTableManager.getJScrollPane();
        _tableScrollPane.setPreferredSize( new Dimension( 100, 100 ) );
//                                                                                    X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _shefTSSelectionPanel, _tableScrollPane,    0,   1,    1,     1, 1, 1, GridBagConstraints.BOTH );
    }

    private void updateJTable()
    {
        _dataMgr.initShefTSList();
        _shefTSRowDataList = _dataMgr.getShefTsRowDataList();
        _shefTSTableManager.setChangedAllRowDataList( _shefTSRowDataList );
        _shefTSTableManager.refreshDisplay();
    }

    private void setCEColumnDescriptorList()
    {
        _shefTSColumnDescriptorList = new ArrayList();

        _shefTSColumnDescriptorList.add(new JTableColumnDescriptor( "TS", true, 50, "center" ) );
        _shefTSColumnDescriptorList.add(new JTableColumnDescriptor( "Name", true, 200, "center" ) );
    }
    
    private void populateDataInputPanel( ShefTS shefTs )
    {
        if ( shefTs != null )
        {
            _tsLTF.setTextField( shefTs.getTs() );
            _nameLTF.setTextField( shefTs.getName() );
        }
    }
    
    private void clearDataInputPanel()
    {
        _tsLTF.setTextField( "" );
        _nameLTF.setTextField( "" );
    }
    
    private boolean saveShefTs()
    {
        boolean saved = false;
        
        if ( ! _tsLTF.getTextFieldText().equalsIgnoreCase( "" ) )
        {
            ShefTS shefTs = new ShefTS();

            shefTs.setTs( _tsLTF.getTextFieldText() );
            shefTs.setName( _nameLTF.getTextFieldText() );
            
            saved = _dataMgr.saveShefTs( shefTs );
            
            updateJTable();
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "Please enter a value for TS", "Save ShefTS" );
        }

        return saved;
    }
    
    private void deleteShefTs()
    {
        ShefTS shefTs = null;
        List selectedRowsData = _shefTSTableManager.getSelectedRowsData();
        
        if ( ! selectedRowsData.isEmpty() )
        {
            ArcBaseShefTSJTableRowData rowData = (ArcBaseShefTSJTableRowData) selectedRowsData.get( 0 );
            shefTs = (ShefTS) _shefTsRowDataToShefTsMap.get( rowData );
            if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + shefTs.keyString(), "Delete ShefTS" ) )
            {
                _dataMgr.deleteShefTsFromDataBase( shefTs );
                clearDataInputPanel();
                updateJTable();
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select a ShefTS", "Delete ShefTS" );
        }
    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();

        _closeButton.addActionListener( windowCloser );
        addWindowListener( windowCloser );
        _shefTSTableManager.addTableListener( new MCETableListener() );
        
        _saveButton.addActionListener( new SaveButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
    }
    
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveShefTs();
        }
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveShefTs() )   // If the save is successful, close the window
            {
                closeWindow();
            }
        }
    }
    
    
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteShefTs();
        }
    }
    
    private class MCETableListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                ShefTS shefTs = null;

                ArcBaseShefTSJTableRowData rowData = (ArcBaseShefTSJTableRowData) _shefTSTableManager.getSelectedRowsData().get( 0 );
                shefTs = (ShefTS) _shefTsRowDataToShefTsMap.get( rowData );
                populateDataInputPanel( shefTs );
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

        ShefTSEditor shefTSEditor = new ShefTSEditor( frame, dataMgr );
        shefTSEditor.displayGUI();
    }
}
