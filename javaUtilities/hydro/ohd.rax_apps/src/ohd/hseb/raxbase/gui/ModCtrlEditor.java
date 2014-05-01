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
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import ohd.hseb.raxbase.RaxBaseDataMgr;
import ohd.hseb.raxbase.model.ModCtrl;
import ohd.hseb.raxbase.table.ArcBaseModCtrlJTableRowData;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledTextField;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class ModCtrlEditor extends JDialog
{
    private Container _dialogContentPane = getContentPane();
    
    private RaxBaseDataMgr _dataMgr = null;
    
//  JTable variables
    private JTableManager _MCETableManager = null;
    private List _MCEColumnDescriptorList = null;
    private List _modCtrlRowDataList = null;
    private JScrollPane _tableScrollPane = null;
    private JPanel _modCtrlSelectionPanel = new JPanel( new GridBagLayout() );

    private JPanel _selectedItemPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _modNameLTF = new LabeledTextField( "ModName:  ", "", "Mod Name", 10 );
    private JCheckBox _loadCB = new JCheckBox( "Load" );
    private JCheckBox _fetchOperCB = new JCheckBox( "Fetch Oper" );
    private JCheckBox _fetchSpinCB = new JCheckBox( "Fetch Spin" );
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _deleteButton = new JButton( "Delete" );

    private Map _modCtrlRowDataToModCtrlMap = null;

    public ModCtrlEditor( JFrame frame, RaxBaseDataMgr dataMgr )
    {
        super( frame, "ModCtrl Editor", true );
        _dataMgr = dataMgr;
    }
    
    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 510, 625 ) );
        initGUI();
    }
    
    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        initModCtrlSelectionPanel();
        initSelectedItemPanel();
        initButtonPanel();
        initFrameComponents();
        initHashMaps();
        addListeners();
        pack();
        setVisible( true );
    }
    
    private void initModCtrlSelectionPanel()
    {
        initJTable();
        
        ComponentHelper.addPanelComponent( _modCtrlSelectionPanel, _deleteButton,    0,   2,    1,     1, 1, 1, GridBagConstraints.NONE );

    }
    
    private void initFrameComponents()
    {
//                                                                                         X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _modCtrlSelectionPanel,     0,  0,     4,     20, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _selectedItemPanel,         0,  21,    4,     1, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _buttonPanel,               0,  22,    4,     1, 1, 1, GridBagConstraints.BOTH );

    }
    private void initButtonPanel()
    {
//                                                                              X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _buttonPanel, _saveAndCloseButton,   0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _saveButton,           1,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _closeButton,          2,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _deleteButton,         3,  0,    1,     1, GridBagConstraints.NONE );
    }

    private void initSelectedItemPanel()
    {
        _selectedItemPanel.setBorder( BorderFactory.createTitledBorder( "Info for Selected ModCtrl" ) );
        JPanel vPanel = new JPanel();
        
//                                                                             X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _selectedItemPanel, _modNameLTF,    0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _loadCB,        0,  1,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _fetchOperCB,   0,  2,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _fetchSpinCB,   0,  3,    1,     1, GridBagConstraints.NONE );
    }


    private void initHashMaps()
    {
        _modCtrlRowDataToModCtrlMap = _dataMgr.getModCtrlRowDataToModCtrlMap();
    }

    private void initJTable() 
    {
        _modCtrlRowDataList = _dataMgr.getModCtrlRowDataList();
        setMCEColumnDescriptorList();
        _MCETableManager = new ComplexJTableManager( _MCEColumnDescriptorList, _modCtrlRowDataList, ListSelectionModel.SINGLE_SELECTION );
        String columnsSelected[] = _MCETableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _MCETableManager.setDisplayableColumns( columnsSelected, true, true );
        _tableScrollPane = _MCETableManager.getJScrollPane();
        _tableScrollPane.setPreferredSize( new Dimension( 300, 300 ) );
//                                                                                    X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _modCtrlSelectionPanel, _tableScrollPane,    0,   1,    1,     1, 1, 1, GridBagConstraints.BOTH );
    }

    private void updateJTable()
    {
        _modCtrlRowDataList = _dataMgr.getModCtrlRowDataList();
        _MCETableManager.setChangedAllRowDataList( _modCtrlRowDataList );
        _MCETableManager.refreshDisplay();
    }

    private void setMCEColumnDescriptorList()
    {
        _MCEColumnDescriptorList = new ArrayList();

        _MCEColumnDescriptorList.add(new JTableColumnDescriptor( "ModName", true, 120, "center" ) );
        _MCEColumnDescriptorList.add(new JTableColumnDescriptor( "Load", true, 120, "center" ) );
        _MCEColumnDescriptorList.add(new JTableColumnDescriptor( "FetchOper", true, 120, "center" ) );
        _MCEColumnDescriptorList.add(new JTableColumnDescriptor( "FetchSpin", true, 120, "center" ) );
    }
    
    private void populateDataInputPanel( ModCtrl modCtrl )
    {
        if ( modCtrl != null )
        {
            _modNameLTF.setTextField( modCtrl.getModName() );
            _loadCB.setSelected( modCtrl.isLoad() );
            _fetchOperCB.setSelected( modCtrl.isFetchOper() );
            _fetchSpinCB.setSelected( modCtrl.isFetchSpin() );
        }
    }
    
    private void clearDataInputPanel()
    {
        _modNameLTF.setTextField( "" );
        _loadCB.setSelected( false );
        _fetchOperCB.setSelected( false );
        _fetchSpinCB.setSelected( false );
    }
    
    private boolean saveModCtrl()
    {
        boolean saved = false;
        
        ModCtrl modCtrl = new ModCtrl();
        
        modCtrl.setModName( _modNameLTF.getTextFieldText() );
        modCtrl.setLoad( _loadCB.isSelected() );
        modCtrl.setFetchOper( _fetchOperCB.isSelected() );
        modCtrl.setFetchSpin( _fetchSpinCB.isSelected() );
        
        saved = _dataMgr.saveModCtrl( modCtrl );
        
        updateJTable();

        return saved;
    }
    
    private void deleteModCtrl()
    {
        ModCtrl modCtrl = null;
        List selectedRowsData = _MCETableManager.getSelectedRowsData();
        
        if ( ! selectedRowsData.isEmpty() )
        {
            ArcBaseModCtrlJTableRowData rowData = (ArcBaseModCtrlJTableRowData) selectedRowsData.get( 0 );
            modCtrl = (ModCtrl) _modCtrlRowDataToModCtrlMap.get( rowData );
            if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + modCtrl.keyString(), "Delete ModCtrl" ) )
            {
                _dataMgr.deleteModCtrlFromDataBase( modCtrl );
                clearDataInputPanel();
                updateJTable();
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select a modctrl", "Delete ModCtrl" );
        }
    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();

        _closeButton.addActionListener( windowCloser );
        addWindowListener( windowCloser );
        _MCETableManager.addTableListener( new MCETableListener() );
        
        _saveButton.addActionListener( new SaveButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
    }
    
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveModCtrl();
        }
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveModCtrl() )   // If the save is successful, close the window
            {
                closeWindow();
            }
        }
    }
    
    
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteModCtrl();
        }
    }
    
    private class MCETableListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                ModCtrl modCtrl = null;

                ArcBaseModCtrlJTableRowData rowData = (ArcBaseModCtrlJTableRowData) _MCETableManager.getSelectedRowsData().get( 0 );
                modCtrl = (ModCtrl) _modCtrlRowDataToModCtrlMap.get( rowData );
                populateDataInputPanel( modCtrl );
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

        ModCtrlEditor modCtrlEditor = new ModCtrlEditor( frame, dataMgr );
        modCtrlEditor.displayGUI();        
    }
}
