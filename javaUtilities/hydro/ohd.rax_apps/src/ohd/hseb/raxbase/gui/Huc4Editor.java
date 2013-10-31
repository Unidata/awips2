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
import ohd.hseb.raxbase.model.Huc4;
import ohd.hseb.raxbase.table.ArcBaseHuc4JTableRowData;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledTextField;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class Huc4Editor extends JDialog
{
    private Container _dialogContentPane = getContentPane();
    
    private RaxBaseDataMgr _dataMgr = null;
    
//  JTable variables
    private JTableManager _huc4TableManager = null;
    private List _huc4ColumnDescriptorList = null;
    private List _huc4RowDataList = null;
    private JScrollPane _tableScrollPane = null;
    private JPanel _huc4SelectionPanel = new JPanel( new GridBagLayout() );

    private JPanel _selectedItemPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _code12LTF = new LabeledTextField( "Code12:", "", "USGS region code", 3 );
    private LabeledTextField _code34LTF = new LabeledTextField( "Code34:", "", "USGS region code", 3 );
    private LabeledTextField _code4LTF = new LabeledTextField( "Code4:", "", "USGS region code", 5 );
    private LabeledTextField _dessubregLTF = new LabeledTextField( "Description:", "", "Description", 60 );
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _deleteButton = new JButton( "Delete" );

    private Map _huc4RowDataToHuc4Map = null;

    public Huc4Editor( JFrame frame, RaxBaseDataMgr dataMgr )
    {
        super( frame, "Huc4 Editor", true );
        _dataMgr = dataMgr;
    }
    
    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 980, 400 ) );
        initGUI();
    }
    
    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        initHuc4SelectionPanel();
        initSelectedItemPanel();
        initButtonPanel();
        initFrameComponents();
        initHashMaps();
        addListeners();
        pack();
        setVisible( true );
    }
    
    private void initHuc4SelectionPanel()
    {
        initJTable();
    }
    
    private void initFrameComponents()
    {
//                                                                                         X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _huc4SelectionPanel,     0,  0,     4,     20, 1, 1, GridBagConstraints.BOTH );
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
        _selectedItemPanel.setBorder( BorderFactory.createTitledBorder( "Info for Selected Huc4" ) );
        JPanel vPanel = new JPanel();
        
//                                                                           X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _selectedItemPanel, _code12LTF,   0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _code34LTF,   0,  1,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _code4LTF,    0,  2,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _dessubregLTF,   0,  3,    1,     1, GridBagConstraints.NONE );
    }


    private void initHashMaps()
    {
        _huc4RowDataToHuc4Map = _dataMgr.getHuc4RowDataToHuc4Map();
    }

    private void initJTable() 
    {
        _huc4RowDataList = _dataMgr.getHuc4RowDataList();
        setCEColumnDescriptorList();
        _huc4TableManager = new ComplexJTableManager( _huc4ColumnDescriptorList, _huc4RowDataList, ListSelectionModel.SINGLE_SELECTION );
        String columnsSelected[] = _huc4TableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _huc4TableManager.setDisplayableColumns( columnsSelected, true, true );
        _tableScrollPane = _huc4TableManager.getJScrollPane();
        _tableScrollPane.setPreferredSize( new Dimension( 300, 100 ) );
//                                                                                    X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _huc4SelectionPanel, _tableScrollPane,    0,   1,    1,     1, 1, 1, GridBagConstraints.BOTH );
    }

    private void updateJTable()
    {
        _huc4RowDataList = _dataMgr.getHuc4RowDataList();
        _huc4TableManager.setChangedAllRowDataList( _huc4RowDataList );
        _huc4TableManager.refreshDisplay();
    }

    private void setCEColumnDescriptorList()
    {
        _huc4ColumnDescriptorList = new ArrayList();

        _huc4ColumnDescriptorList.add(new JTableColumnDescriptor( "Code12", true, 50, "center" ) );
        _huc4ColumnDescriptorList.add(new JTableColumnDescriptor( "Code34", true, 50, "center" ) );
        _huc4ColumnDescriptorList.add(new JTableColumnDescriptor( "Code4", true, 50, "center" ) );
        _huc4ColumnDescriptorList.add(new JTableColumnDescriptor( "Desc", true, 800, "center" ) );
    }
    
    private void populateDataInputPanel( Huc4 huc4 )
    {
        if ( huc4 != null )
        {
            _code12LTF.setTextField( huc4.getCode12() );
            _code34LTF.setTextField( huc4.getCode34() );
            _code4LTF.setTextField( huc4.getCode4() );
            _dessubregLTF.setTextField( huc4.getDessubreg() );
        }
    }
    
    private void clearDataInputPanel()
    {
        _code12LTF.setTextField( "" );
        _code34LTF.setTextField( "" );
        _code4LTF.setTextField( "" );
        _dessubregLTF.setTextField( "" );
    }
    
    private boolean saveHuc4()
    {
        boolean saved = false;
        
        Huc4 huc4 = new Huc4();

        huc4.setCode12( _code12LTF.getTextFieldText() );
        huc4.setCode34( _code34LTF.getTextFieldText() );
        huc4.setCode4(  _code4LTF.getTextFieldText() );
        huc4.setDessubreg( _dessubregLTF.getTextFieldText() );
        
        saved = _dataMgr.saveHuc4( huc4 );
        
        updateJTable();

        return saved;
    }
    
    private void deleteHuc4()
    {
        Huc4 huc4 = null;
        List selectedRowsData = _huc4TableManager.getSelectedRowsData();
        
        if ( ! selectedRowsData.isEmpty() )
        {
            ArcBaseHuc4JTableRowData rowData = (ArcBaseHuc4JTableRowData) selectedRowsData.get( 0 );
            huc4 = (Huc4) _huc4RowDataToHuc4Map.get( rowData );
            if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + huc4.keyString(), "Delete Huc4" ) )
            {
                _dataMgr.deleteHuc4FromDataBase( huc4 );
                clearDataInputPanel();
                updateJTable();
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select a Huc4", "Delete Huc4" );
        }
    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();

        _closeButton.addActionListener( windowCloser );
        addWindowListener( windowCloser );
        _huc4TableManager.addTableListener( new MCETableListener() );
        
        _saveButton.addActionListener( new SaveButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
    }
    
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveHuc4();
        }
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveHuc4() )   // If the save is successful, close the window
            {
                closeWindow();
            }
        }
    }
    
    
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteHuc4();
        }
    }
    
    private class MCETableListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                Huc4 huc4 = null;

                ArcBaseHuc4JTableRowData rowData = (ArcBaseHuc4JTableRowData) _huc4TableManager.getSelectedRowsData().get( 0 );
                huc4 = (Huc4) _huc4RowDataToHuc4Map.get( rowData );
                populateDataInputPanel( huc4 );
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

        Huc4Editor huc4Editor = new Huc4Editor( frame, dataMgr );
        huc4Editor.displayGUI();
    }
}
