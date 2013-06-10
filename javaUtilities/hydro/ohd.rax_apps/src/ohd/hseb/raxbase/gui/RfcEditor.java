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
import ohd.hseb.raxbase.model.Rfc;
import ohd.hseb.raxbase.table.ArcBaseRfcJTableRowData;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledTextField;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class RfcEditor extends JDialog
{
    private Container _dialogContentPane = getContentPane();
    
    private RaxBaseDataMgr _dataMgr = null;
    
//  JTable variables
    private JTableManager _rfcTableManager = null;
    private List _rfcColumnDescriptorList = null;
    private List _rfcRowDataList = null;
    private JScrollPane _tableScrollPane = null;
    private JPanel _rfcSelectionPanel = new JPanel( new GridBagLayout() );

    private JPanel _selectedItemPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _rfcLTF = new LabeledTextField( "RFC:", "", "First 2 characters of the RFC acronym (upper case) (eg, MB for MBRFC)", 3 );
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _deleteButton = new JButton( "Delete" );

    private Map _rfcRowDataToRfcMap = null;

    public RfcEditor( JFrame frame, RaxBaseDataMgr dataMgr )
    {
        super( frame, "Rfc Editor", true );
        _dataMgr = dataMgr;
    }
    
    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 240, 400 ) );
        initGUI();
    }
    
    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        initRfcSelectionPanel();
        initSelectedItemPanel();
        initButtonPanel();
        initFrameComponents();
        initHashMaps();
        addListeners();
        pack();
        setVisible( true );
    }
    
    private void initRfcSelectionPanel()
    {
        initJTable();
    }
    
    private void initFrameComponents()
    {
//                                                                                         X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _rfcSelectionPanel,     0,  0,     4,     20, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _selectedItemPanel,         0,  21,    4,     1, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _buttonPanel,               0,  22,    4,     1, 1, 1, GridBagConstraints.BOTH );

    }
    private void initButtonPanel()
    {
//                                                                              X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _buttonPanel, _saveAndCloseButton,   0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _saveButton,           1,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _deleteButton,         0,  1,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _closeButton,          1,  1,    1,     1, GridBagConstraints.NONE );
    }

    private void initSelectedItemPanel()
    {
        _selectedItemPanel.setBorder( BorderFactory.createTitledBorder( "Info for Selected Rfc" ) );
        JPanel vPanel = new JPanel();
        
//                                                                           X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _selectedItemPanel, _rfcLTF,   0,  0,    1,     1, GridBagConstraints.NONE );
    }


    private void initHashMaps()
    {
        _rfcRowDataToRfcMap = _dataMgr.getRfcRowDataToRfcMap();
    }

    private void initJTable() 
    {
        _rfcRowDataList = _dataMgr.getRfcRowDataList();
        setCEColumnDescriptorList();
        _rfcTableManager = new ComplexJTableManager( _rfcColumnDescriptorList, _rfcRowDataList, ListSelectionModel.SINGLE_SELECTION );
        String columnsSelected[] = _rfcTableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _rfcTableManager.setDisplayableColumns( columnsSelected, true, true );
        _tableScrollPane = _rfcTableManager.getJScrollPane();
        _tableScrollPane.setPreferredSize( new Dimension( 90, 100 ) );
//                                                                                    X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _rfcSelectionPanel, _tableScrollPane,    0,   1,    1,     1, 1, 1, GridBagConstraints.VERTICAL );
    }

    private void updateJTable()
    {
        _rfcRowDataList = _dataMgr.getRfcRowDataList();
        _rfcTableManager.setChangedAllRowDataList( _rfcRowDataList );
        _rfcTableManager.refreshDisplay();
    }

    private void setCEColumnDescriptorList()
    {
        _rfcColumnDescriptorList = new ArrayList();

        _rfcColumnDescriptorList.add(new JTableColumnDescriptor( "RFC", true, 70, "center" ) );
    }
    
    private void populateDataInputPanel( Rfc rfc )
    {
        if ( rfc != null )
        {
            _rfcLTF.setTextField( rfc.getRfc() );
        }
    }
    
    private void clearDataInputPanel()
    {
        _rfcLTF.setTextField( "" );
    }
    
    private boolean saveRfc()
    {
        boolean saved = false;
        
        Rfc rfc = new Rfc();

        rfc.setRfc( _rfcLTF.getTextFieldText() );
        
        saved = _dataMgr.saveRfc( rfc );
        
        updateJTable();

        return saved;
    }
    
    private void deleteRfc()
    {
        Rfc rfc = null;
        List selectedRowsData = _rfcTableManager.getSelectedRowsData();
        
        if ( ! selectedRowsData.isEmpty() )
        {
            ArcBaseRfcJTableRowData rowData = (ArcBaseRfcJTableRowData) selectedRowsData.get( 0 );
            rfc = (Rfc) _rfcRowDataToRfcMap.get( rowData );
            if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + rfc.keyString(), "Delete Rfc" ) )
            {
                _dataMgr.deleteRfcFromDataBase( rfc );
                clearDataInputPanel();
                updateJTable();
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select a Rfc", "Delete Rfc" );
        }
    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();

        _closeButton.addActionListener( windowCloser );
        addWindowListener( windowCloser );
        _rfcTableManager.addTableListener( new MCETableListener() );
        
        _saveButton.addActionListener( new SaveButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
    }
    
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveRfc();
        }
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveRfc() )   // If the save is successful, close the window
            {
                closeWindow();
            }
        }
    }
    
    
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteRfc();
        }
    }
    
    private class MCETableListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                Rfc rfc = null;

                ArcBaseRfcJTableRowData rowData = (ArcBaseRfcJTableRowData) _rfcTableManager.getSelectedRowsData().get( 0 );
                rfc = (Rfc) _rfcRowDataToRfcMap.get( rowData );
                populateDataInputPanel( rfc );
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

        RfcEditor rfcEditor = new RfcEditor( frame, dataMgr );
        rfcEditor.displayGUI();
    }
}
