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
import ohd.hseb.raxbase.model.ShefExtremum;
import ohd.hseb.raxbase.table.ArcBaseShefExtremumJTableRowData;
import ohd.hseb.util.StringDataConverter;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledTextField;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class ShefExtremumEditor extends JDialog
{
    private Container _dialogContentPane = getContentPane();
    
    private RaxBaseDataMgr _dataMgr = null;
    
//  JTable variables
    private JTableManager _shefExTableManager = null;
    private List _shefExColumnDescriptorList = null;
    private List _shefExRowDataList = null;
    private JScrollPane _tableScrollPane = null;
    private JPanel _shefExSelectionPanel = new JPanel( new GridBagLayout() );

    private JPanel _selectedItemPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _extremumLTF = new LabeledTextField( "Extremum:", "", "SHEF Extremum code of adjuster", 2 );
    private LabeledTextField _nameLTF = new LabeledTextField( "Name:", "", "Description (mixed case)", 21 );
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _deleteButton = new JButton( "Delete" );

    private Map _shefExRowDataToShefExMap = null;

    public ShefExtremumEditor( JFrame frame, RaxBaseDataMgr dataMgr )
    {
        super( frame, "ShefExtremum Editor", true );
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
        initShefExtremumSelectionPanel();
        initSelectedItemPanel();
        initButtonPanel();
        initFrameComponents();
        initHashMaps();
        addListeners();
        pack();
        setVisible( true );
    }
    
    private void initShefExtremumSelectionPanel()
    {
        initJTable();
    }
    
    private void initFrameComponents()
    {
//                                                                                         X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _shefExSelectionPanel,     0,  0,     4,     20, 1, 1, GridBagConstraints.BOTH );
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
        _selectedItemPanel.setBorder( BorderFactory.createTitledBorder( "Info for Selected ShefEx" ) );
        JPanel vPanel = new JPanel();
        
//                                                                           X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _selectedItemPanel, _extremumLTF, 0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _nameLTF,     0,  1,    1,     1, GridBagConstraints.NONE );
    }


    private void initHashMaps()
    {
        _shefExRowDataToShefExMap = _dataMgr.getShefExRowDataToShefExMap();
    }

    private void initJTable() 
    {
        _shefExRowDataList = _dataMgr.getShefExRowDataList();
        setCEColumnDescriptorList();
        _shefExTableManager = new ComplexJTableManager( _shefExColumnDescriptorList, _shefExRowDataList, ListSelectionModel.SINGLE_SELECTION );
        String columnsSelected[] = _shefExTableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _shefExTableManager.setDisplayableColumns( columnsSelected, true, true );
        _tableScrollPane = _shefExTableManager.getJScrollPane();
        _tableScrollPane.setPreferredSize( new Dimension( 330, 100 ) );
//                                                                                    X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _shefExSelectionPanel, _tableScrollPane,    0,   1,    1,     1, 1, 1, GridBagConstraints.BOTH );
    }

    private void updateJTable()
    {
        _dataMgr.initShefExtremumList();
        _shefExRowDataList = _dataMgr.getShefExRowDataList();
        _shefExTableManager.setChangedAllRowDataList( _shefExRowDataList );
        _shefExTableManager.refreshDisplay();
    }

    private void setCEColumnDescriptorList()
    {
        _shefExColumnDescriptorList = new ArrayList();

        _shefExColumnDescriptorList.add(new JTableColumnDescriptor( "Extremum", true, 50, "center" ) );
        _shefExColumnDescriptorList.add(new JTableColumnDescriptor( "Name", true, 200, "center" ) );
    }
    
    private void populateDataInputPanel( ShefExtremum shefEx )
    {
        if ( shefEx != null )
        {
            _extremumLTF.setTextField( shefEx.getExtremum() );
            _nameLTF.setTextField( shefEx.getName() );
        }
    }
    
    private void clearDataInputPanel()
    {
        _extremumLTF.setTextField( "" );
        _nameLTF.setTextField( "" );
    }
    
    private boolean saveShefExtremum()
    {
        boolean saved = false;
        StringDataConverter converter = new StringDataConverter();
        
        if ( ! _extremumLTF.getTextFieldText().equalsIgnoreCase( "" ) )
        {
            ShefExtremum shefEx = new ShefExtremum();

            shefEx.setExtremum( _extremumLTF.getTextFieldText() );
            shefEx.setName( _nameLTF.getTextFieldText() );
            
            saved = _dataMgr.saveShefEx( shefEx );
            
            updateJTable();
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "Please enter a value for Extremum", "Save Extremum" );
        }

        return saved;
    }
    
    private void deleteShefExtremum()
    {
        ShefExtremum shefEx = null;
        List selectedRowsData = _shefExTableManager.getSelectedRowsData();
        
        if ( ! selectedRowsData.isEmpty() )
        {
            ArcBaseShefExtremumJTableRowData rowData = (ArcBaseShefExtremumJTableRowData) selectedRowsData.get( 0 );
            shefEx = (ShefExtremum) _shefExRowDataToShefExMap.get( rowData );
            if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + shefEx.keyString(), "Delete ShefExtremum" ) )
            {
                _dataMgr.deleteShefExtremumFromDataBase( shefEx );
                clearDataInputPanel();
                updateJTable();
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select a ShefExtremum", "Delete ShefExtremum" );
        }
    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();

        _closeButton.addActionListener( windowCloser );
        addWindowListener( windowCloser );
        _shefExTableManager.addTableListener( new MCETableListener() );
        
        _saveButton.addActionListener( new SaveButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
    }
    
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveShefExtremum();
        }
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveShefExtremum() )   // If the save is successful, close the window
            {
                closeWindow();
            }
        }
    }
    
    
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteShefExtremum();
        }
    }
    
    private class MCETableListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                ShefExtremum shefExtremum = null;

                ArcBaseShefExtremumJTableRowData rowData = (ArcBaseShefExtremumJTableRowData) _shefExTableManager.getSelectedRowsData().get( 0 );
                shefExtremum = (ShefExtremum) _shefExRowDataToShefExMap.get( rowData );
                populateDataInputPanel( shefExtremum );
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

        ShefExtremumEditor shefExtremumEditor = new ShefExtremumEditor( frame, dataMgr );
        shefExtremumEditor.displayGUI();
    }
}
