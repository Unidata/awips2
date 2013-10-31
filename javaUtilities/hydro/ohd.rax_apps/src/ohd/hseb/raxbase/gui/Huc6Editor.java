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
import ohd.hseb.raxbase.model.Huc6;
import ohd.hseb.raxbase.table.ArcBaseHuc6JTableRowData;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledTextField;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class Huc6Editor extends JDialog
{
    private Container _dialogContentPane = getContentPane();
    
    private RaxBaseDataMgr _dataMgr = null;
    
//  JTable variables
    private JTableManager _huc6TableManager = null;
    private List _huc6ColumnDescriptorList = null;
    private List _huc6RowDataList = null;
    private JScrollPane _tableScrollPane = null;
    private JPanel _huc6SelectionPanel = new JPanel( new GridBagLayout() );

    private JPanel _selectedItemPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _code12LTF = new LabeledTextField( "Code12:", "", "USGS region code", 3 );
    private LabeledTextField _code34LTF = new LabeledTextField( "Code34:", "", "USGS region code", 3 );
    private LabeledTextField _code56LTF = new LabeledTextField( "Code56:", "", "USGS region code", 3 );
    private LabeledTextField _code6LTF = new LabeledTextField( "Code6:", "", "USGS region code", 7 );
    private LabeledTextField _desacctLTF = new LabeledTextField( "Description:", "", "Description", 60 );
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _deleteButton = new JButton( "Delete" );

    private Map _huc6RowDataToHuc6Map = null;

    public Huc6Editor( JFrame frame, RaxBaseDataMgr dataMgr )
    {
        super( frame, "Huc6 Editor", true );
        _dataMgr = dataMgr;
    }
    
    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 1050, 500 ) );
        initGUI();
    }
    
    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        initHuc6SelectionPanel();
        initSelectedItemPanel();
        initButtonPanel();
        initFrameComponents();
        initHashMaps();
        addListeners();
        pack();
        setVisible( true );
    }
    
    private void initHuc6SelectionPanel()
    {
        initJTable();
    }
    
    private void initFrameComponents()
    {
//                                                                                         X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _huc6SelectionPanel,     0,  0,     4,     20, 1, 1, GridBagConstraints.BOTH );
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
        _selectedItemPanel.setBorder( BorderFactory.createTitledBorder( "Info for Selected Huc6" ) );
        JPanel vPanel = new JPanel();
        
//                                                                           X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _selectedItemPanel, _code12LTF,   0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _code34LTF,   0,  1,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _code56LTF,   0,  2,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _code6LTF,    0,  3,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _desacctLTF,  0,  4,    1,     1, GridBagConstraints.NONE );
    }


    private void initHashMaps()
    {
        _huc6RowDataToHuc6Map = _dataMgr.getHuc6RowDataToHuc6Map();
    }

    private void initJTable() 
    {
        _huc6RowDataList = _dataMgr.getHuc6RowDataList();
        setCEColumnDescriptorList();
        _huc6TableManager = new ComplexJTableManager( _huc6ColumnDescriptorList, _huc6RowDataList, ListSelectionModel.SINGLE_SELECTION );
        String columnsSelected[] = _huc6TableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _huc6TableManager.setDisplayableColumns( columnsSelected, true, true );
        _tableScrollPane = _huc6TableManager.getJScrollPane();
        _tableScrollPane.setPreferredSize( new Dimension( 300, 100 ) );
//                                                                                    X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _huc6SelectionPanel, _tableScrollPane,    0,   1,    1,     1, 1, 1, GridBagConstraints.BOTH );
    }

    private void updateJTable()
    {
        _huc6RowDataList = _dataMgr.getHuc6RowDataList();
        _huc6TableManager.setChangedAllRowDataList( _huc6RowDataList );
        _huc6TableManager.refreshDisplay();
    }

    private void setCEColumnDescriptorList()
    {
        _huc6ColumnDescriptorList = new ArrayList();

        _huc6ColumnDescriptorList.add(new JTableColumnDescriptor( "Code12", true, 50, "center" ) );
        _huc6ColumnDescriptorList.add(new JTableColumnDescriptor( "Code34", true, 50, "center" ) );
        _huc6ColumnDescriptorList.add(new JTableColumnDescriptor( "Code56", true, 50, "center" ) );
        _huc6ColumnDescriptorList.add(new JTableColumnDescriptor( "Code6", true, 70, "center" ) );
        _huc6ColumnDescriptorList.add(new JTableColumnDescriptor( "Desc", true, 800, "center" ) );
    }
    
    private void populateDataInputPanel( Huc6 huc6 )
    {
        if ( huc6 != null )
        {
            _code12LTF.setTextField( huc6.getCode12() );
            _code34LTF.setTextField( huc6.getCode34() );
            _code56LTF.setTextField( huc6.getCode56() );
            _code6LTF.setTextField( huc6.getCode6() );
            _desacctLTF.setTextField( huc6.getDesacct() );
        }
    }
    
    private void clearDataInputPanel()
    {
        _code12LTF.setTextField( "" );
        _code34LTF.setTextField( "" );
        _code56LTF.setTextField( "" );
        _code6LTF.setTextField( "" );
        _desacctLTF.setTextField( "" );
    }
    
    private boolean saveHuc6()
    {
        boolean saved = false;
        
        Huc6 huc6 = new Huc6();

        huc6.setCode12( _code12LTF.getTextFieldText() );
        huc6.setCode34( _code34LTF.getTextFieldText() );
        huc6.setCode56( _code56LTF.getTextFieldText() );
        huc6.setCode6(  _code6LTF.getTextFieldText() );
        huc6.setDesacct( _desacctLTF.getTextFieldText() );
        
        saved = _dataMgr.saveHuc6( huc6 );
        
        updateJTable();

        return saved;
    }
    
    private void deleteHuc6()
    {
        Huc6 huc6 = null;
        List selectedRowsData = _huc6TableManager.getSelectedRowsData();
        
        if ( ! selectedRowsData.isEmpty() )
        {
            ArcBaseHuc6JTableRowData rowData = (ArcBaseHuc6JTableRowData) selectedRowsData.get( 0 );
            huc6 = (Huc6) _huc6RowDataToHuc6Map.get( rowData );
            if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + huc6.keyString(), "Delete Huc6" ) )
            {
                _dataMgr.deleteHuc6FromDataBase( huc6 );
                clearDataInputPanel();
                updateJTable();
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select a Huc6", "Delete Huc6" );
        }
    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();

        _closeButton.addActionListener( windowCloser );
        addWindowListener( windowCloser );
        _huc6TableManager.addTableListener( new MCETableListener() );
        
        _saveButton.addActionListener( new SaveButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
    }
    
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveHuc6();
        }
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveHuc6() )   // If the save is successful, close the window
            {
                closeWindow();
            }
        }
    }
    
    
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteHuc6();
        }
    }
    
    private class MCETableListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                Huc6 huc6 = null;

                ArcBaseHuc6JTableRowData rowData = (ArcBaseHuc6JTableRowData) _huc6TableManager.getSelectedRowsData().get( 0 );
                huc6 = (Huc6) _huc6RowDataToHuc6Map.get( rowData );
                populateDataInputPanel( huc6 );
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

        Huc6Editor huc6Editor = new Huc6Editor( frame, dataMgr );
        huc6Editor.displayGUI();
    }
}
