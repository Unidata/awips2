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
import ohd.hseb.raxbase.model.WfoHsa;
import ohd.hseb.raxbase.table.ArcBaseWfoHsaJTableRowData;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledTextField;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class WfoHsaEditor extends JDialog
{
    private Container _dialogContentPane = getContentPane();
    
    private RaxBaseDataMgr _dataMgr = null;
    
//  JTable variables
    private JTableManager _wfoHsaTableManager = null;
    private List _wfoHsaColumnDescriptorList = null;
    private List _wfoHsaRowDataList = null;
    private JScrollPane _tableScrollPane = null;
    private JPanel _wfoHsaSelectionPanel = new JPanel( new GridBagLayout() );

    private JPanel _selectedItemPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _wfoHsaLTF = new LabeledTextField( "WfoHsa:", "", "WFO identifier (upper case)", 4 );
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _deleteButton = new JButton( "Delete" );

    private Map _wfoHsaRowDataToWfoHsaMap = null;

    public WfoHsaEditor( JFrame frame, RaxBaseDataMgr dataMgr )
    {
        super( frame, "WfoHsa Editor", true );
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
        initWfoHsaSelectionPanel();
        initSelectedItemPanel();
        initButtonPanel();
        initFrameComponents();
        initHashMaps();
        addListeners();
        pack();
        setVisible( true );
    }
    
    private void initWfoHsaSelectionPanel()
    {
        initJTable();
    }
    
    private void initFrameComponents()
    {
//                                                                                         X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _wfoHsaSelectionPanel,     0,  0,     4,     20, 1, 1, GridBagConstraints.BOTH );
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
        _selectedItemPanel.setBorder( BorderFactory.createTitledBorder( "Info for Selected WfoHsa" ) );
        JPanel vPanel = new JPanel();
        
//                                                                           X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _selectedItemPanel, _wfoHsaLTF,   0,  0,    1,     1, GridBagConstraints.NONE );
    }


    private void initHashMaps()
    {
        _wfoHsaRowDataToWfoHsaMap = _dataMgr.getWfoHsaRowDataToWfoHsaMap();
    }

    private void initJTable() 
    {
        _wfoHsaRowDataList = _dataMgr.getWfoHsaRowDataList();
        setCEColumnDescriptorList();
        _wfoHsaTableManager = new ComplexJTableManager( _wfoHsaColumnDescriptorList, _wfoHsaRowDataList, ListSelectionModel.SINGLE_SELECTION );
        String columnsSelected[] = _wfoHsaTableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _wfoHsaTableManager.setDisplayableColumns( columnsSelected, true, true );
        _tableScrollPane = _wfoHsaTableManager.getJScrollPane();
        _tableScrollPane.setPreferredSize( new Dimension( 90, 100 ) );
//                                                                                    X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _wfoHsaSelectionPanel, _tableScrollPane,    0,   1,    1,     1, 1, 1, GridBagConstraints.VERTICAL );
    }

    private void updateJTable()
    {
        _wfoHsaRowDataList = _dataMgr.getWfoHsaRowDataList();
        _wfoHsaTableManager.setChangedAllRowDataList( _wfoHsaRowDataList );
        _wfoHsaTableManager.refreshDisplay();
    }

    private void setCEColumnDescriptorList()
    {
        _wfoHsaColumnDescriptorList = new ArrayList();

        _wfoHsaColumnDescriptorList.add(new JTableColumnDescriptor( "WfoHsa", true, 70, "center" ) );
    }
    
    private void populateDataInputPanel( WfoHsa wfoHsa )
    {
        if ( wfoHsa != null )
        {
            _wfoHsaLTF.setTextField( wfoHsa.getWfoHsa() );
        }
    }
    
    private void clearDataInputPanel()
    {
        _wfoHsaLTF.setTextField( "" );
    }
    
    private boolean saveWfoHsa()
    {
        boolean saved = false;
        
        WfoHsa wfoHsa = new WfoHsa();

        wfoHsa.setWfoHsa( _wfoHsaLTF.getTextFieldText() );
        
        saved = _dataMgr.saveWfoHsa( wfoHsa );
        
        updateJTable();

        return saved;
    }
    
    private void deleteWfoHsa()
    {
        WfoHsa wfoHsa = null;
        List selectedRowsData = _wfoHsaTableManager.getSelectedRowsData();
        
        if ( ! selectedRowsData.isEmpty() )
        {
            ArcBaseWfoHsaJTableRowData rowData = (ArcBaseWfoHsaJTableRowData) selectedRowsData.get( 0 );
            wfoHsa = (WfoHsa) _wfoHsaRowDataToWfoHsaMap.get( rowData );
            if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + wfoHsa.keyString(), "Delete WfoHsa" ) )
            {
                _dataMgr.deleteWfoHsaFromDataBase( wfoHsa );
                clearDataInputPanel();
                updateJTable();
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select a WfoHsa", "Delete WfoHsa" );
        }
    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();

        _closeButton.addActionListener( windowCloser );
        addWindowListener( windowCloser );
        _wfoHsaTableManager.addTableListener( new MCETableListener() );
        
        _saveButton.addActionListener( new SaveButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
    }
    
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveWfoHsa();
        }
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveWfoHsa() )   // If the save is successful, close the window
            {
                closeWindow();
            }
        }
    }
    
    
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteWfoHsa();
        }
    }
    
    private class MCETableListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                WfoHsa wfoHsa = null;

                ArcBaseWfoHsaJTableRowData rowData = (ArcBaseWfoHsaJTableRowData) _wfoHsaTableManager.getSelectedRowsData().get( 0 );
                wfoHsa = (WfoHsa) _wfoHsaRowDataToWfoHsaMap.get( rowData );
                populateDataInputPanel( wfoHsa );
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

        WfoHsaEditor wfoHsaEditor = new WfoHsaEditor( frame, dataMgr );
        wfoHsaEditor.displayGUI();
    }
}
