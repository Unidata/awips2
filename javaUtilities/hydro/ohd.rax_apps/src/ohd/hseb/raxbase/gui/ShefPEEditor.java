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
import ohd.hseb.raxbase.model.ShefPE;
import ohd.hseb.raxbase.table.ArcBaseShefPEJTableRowData;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledTextField;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class ShefPEEditor extends JDialog
{
    private Container _dialogContentPane = getContentPane();
    
    private RaxBaseDataMgr _dataMgr = null;
    
//  JTable variables
    private JTableManager _shefPeTableManager = null;
    private List _shefPeColumnDescriptorList = null;
    private List _shefPeRowDataList = null;
    private JScrollPane _tableScrollPane = null;
    private JPanel _shefPeSelectionPanel = new JPanel( new GridBagLayout() );

    private JPanel _selectedItemPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _peLTF = new LabeledTextField( "PE:", "", "SHEF PE code", 3 );
    private LabeledTextField _nameLTF = new LabeledTextField( "Name:", "", "Description (mixed case)", 31 );
    private LabeledTextField _engUnitLTF = new LabeledTextField( "English Unit:", "", "Abbreviation of the English unit for the specified SHEF PE. (eg, ft for feet)", 9 );
    private LabeledTextField _metUnitLTF = new LabeledTextField( "Metric Unit:", "", "Abbreviation of the Metric unit for the specified SHEF PE (eg, deg C)", 9 );
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _deleteButton = new JButton( "Delete" );

    private Map _shefPeRowDataToShefPeMap = null;

    public ShefPEEditor( JFrame frame, RaxBaseDataMgr dataMgr )
    {
        super( frame, "ShefPE Editor", true );
        _dataMgr = dataMgr;
    }
    
    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 600, 400 ) );
        initGUI();
    }
    
    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        initShefPeSelectionPanel();
        initSelectedItemPanel();
        initButtonPanel();
        initFrameComponents();
        initHashMaps();
        addListeners();
        pack();
        setVisible( true );
    }
    
    private void initShefPeSelectionPanel()
    {
        initJTable();
    }
    
    private void initFrameComponents()
    {
//                                                                                         X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _shefPeSelectionPanel,     0,  0,     4,     20, 1, 1, GridBagConstraints.VERTICAL );
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
        _selectedItemPanel.setBorder( BorderFactory.createTitledBorder( "Info for Selected ShefPE" ) );
        JPanel vPanel = new JPanel();
        
//                                                                                X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _selectedItemPanel, _peLTF,            0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _nameLTF,  0,  1,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _engUnitLTF,    0,  2,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _metUnitLTF,    0,  3,    1,     1, GridBagConstraints.NONE );
    }


    private void initHashMaps()
    {
        _shefPeRowDataToShefPeMap = _dataMgr.getShefPeRowDataToShefPeMap();
    }

    private void initJTable() 
    {
        _shefPeRowDataList = _dataMgr.getShefPeRowDataList();
        setCEColumnDescriptorList();
        _shefPeTableManager = new ComplexJTableManager( _shefPeColumnDescriptorList, _shefPeRowDataList, ListSelectionModel.SINGLE_SELECTION );
        String columnsSelected[] = _shefPeTableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _shefPeTableManager.setDisplayableColumns( columnsSelected, true, true );
        _tableScrollPane = _shefPeTableManager.getJScrollPane();
        _tableScrollPane.setPreferredSize( new Dimension( 520, 100 ) );
//                                                                                    X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _shefPeSelectionPanel, _tableScrollPane,    0,   1,    1,     1, 1, 1, GridBagConstraints.VERTICAL );
    }

    private void updateJTable()
    {
        _dataMgr.initShefPeList();
        _shefPeRowDataList = _dataMgr.getShefPeRowDataList();
        _shefPeTableManager.setChangedAllRowDataList( _shefPeRowDataList );
        _shefPeTableManager.refreshDisplay();
    }

    private void setCEColumnDescriptorList()
    {
        _shefPeColumnDescriptorList = new ArrayList();

        _shefPeColumnDescriptorList.add(new JTableColumnDescriptor( "PE", true, 50, "center" ) );
        _shefPeColumnDescriptorList.add(new JTableColumnDescriptor( "Name", true, 250, "center" ) );
        _shefPeColumnDescriptorList.add(new JTableColumnDescriptor( "MetUnit", true, 100, "center" ) );
        _shefPeColumnDescriptorList.add(new JTableColumnDescriptor( "EngUnit", true, 100, "center" ) );
    }
    
    private void populateDataInputPanel( ShefPE shefPe )
    {
        if ( shefPe != null )
        {
            _peLTF.setTextField( shefPe.getPe() );
            _nameLTF.setTextField( shefPe.getName() );
            _engUnitLTF.setTextField( shefPe.getEngUnit() );
            _metUnitLTF.setTextField( shefPe.getMetUnit() );
        }
    }
    
    private void clearDataInputPanel()
    {
        _peLTF.setTextField( "" );
        _nameLTF.setTextField( "" );
        _engUnitLTF.setTextField( "" );
        _metUnitLTF.setTextField( "" );
    }
    
    private boolean saveShefPe()
    {
        boolean saved = false;
        
        if ( ! _peLTF.getTextFieldText().equalsIgnoreCase( "" ) )  
        {
            ShefPE shefPe = new ShefPE();

            shefPe.setPe( _peLTF.getTextFieldText() );
            shefPe.setName( _nameLTF.getTextFieldText() );
            shefPe.setEngUnit( _engUnitLTF.getTextFieldText() );
            shefPe.setMetUnit( _metUnitLTF.getTextFieldText() );
            
            saved = _dataMgr.saveShefPe( shefPe );
            
            updateJTable();
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "Please enter a value for PE", "Save ShefPE" );
        }

        return saved;
    }
    
    private void deleteShefPe()
    {
        ShefPE shefPe = null;
        List selectedRowsData = _shefPeTableManager.getSelectedRowsData();
        
        if ( ! selectedRowsData.isEmpty() )
        {
            ArcBaseShefPEJTableRowData rowData = (ArcBaseShefPEJTableRowData) selectedRowsData.get( 0 );
            shefPe = (ShefPE) _shefPeRowDataToShefPeMap.get( rowData );
            if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + shefPe.keyString(), "Delete ShefPE" ) )
            {
                _dataMgr.deleteShefPeFromDataBase( shefPe );
                clearDataInputPanel();
                updateJTable();
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select a ShefPE", "Delete ShefPE" );
        }
    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();

        _closeButton.addActionListener( windowCloser );
        addWindowListener( windowCloser );
        _shefPeTableManager.addTableListener( new MCETableListener() );
        
        _saveButton.addActionListener( new SaveButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
    }
    
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveShefPe();
        }
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveShefPe() )   // If the save is successful, close the window
            {
                closeWindow();
            }
        }
    }
    
    
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteShefPe();
        }
    }
    
    private class MCETableListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                ShefPE shefPe = null;

                ArcBaseShefPEJTableRowData rowData = (ArcBaseShefPEJTableRowData) _shefPeTableManager.getSelectedRowsData().get( 0 );
                shefPe = (ShefPE) _shefPeRowDataToShefPeMap.get( rowData );
                populateDataInputPanel( shefPe );
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

        ShefPEEditor shefPEEditor = new ShefPEEditor( frame, dataMgr );
        shefPEEditor.displayGUI();
    }
}
