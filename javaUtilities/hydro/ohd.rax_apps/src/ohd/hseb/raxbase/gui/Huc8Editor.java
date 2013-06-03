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
import ohd.hseb.raxbase.model.Huc8;
import ohd.hseb.raxbase.table.ArcBaseHuc8JTableRowData;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledTextField;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class Huc8Editor extends JDialog
{
    private Container _dialogContentPane = getContentPane();
    
    private RaxBaseDataMgr _dataMgr = null;
    
//  JTable variables
    private JTableManager _huc8TableManager = null;
    private List _huc8ColumnDescriptorList = null;
    private List _huc8RowDataList = null;
    private JScrollPane _tableScrollPane = null;
    private JPanel _huc8SelectionPanel = new JPanel( new GridBagLayout() );

    private JPanel _selectedItemPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _code12LTF = new LabeledTextField( "Code12:", "", "USGS region code", 3 );
    private LabeledTextField _code34LTF = new LabeledTextField( "Code34:", "", "USGS region code", 3 );
    private LabeledTextField _code56LTF = new LabeledTextField( "Code56:", "", "USGS region code", 3 );
    private LabeledTextField _code78LTF = new LabeledTextField( "Code78:", "", "USGS region code", 3 );
    private LabeledTextField _code8LTF = new LabeledTextField( "Code8:", "", "USGS region code", 9 );
    private LabeledTextField _desactLTF = new LabeledTextField( "Description:", "", "Description", 60 );
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _deleteButton = new JButton( "Delete" );

    private Map _huc8RowDataToHuc8Map = null;

    public Huc8Editor( JFrame frame, RaxBaseDataMgr dataMgr )
    {
        super( frame, "Huc8 Editor", true );
        _dataMgr = dataMgr;
    }
    
    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 1150, 500 ) );
        initGUI();
    }
    
    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        initHuc8SelectionPanel();
        initSelectedItemPanel();
        initButtonPanel();
        initFrameComponents();
        initHashMaps();
        addListeners();
        pack();
        setVisible( true );
    }
    
    private void initHuc8SelectionPanel()
    {
        initJTable();
    }
    
    private void initFrameComponents()
    {
//                                                                                         X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _huc8SelectionPanel,     0,  0,     4,     20, 1, 1, GridBagConstraints.BOTH );
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
        _selectedItemPanel.setBorder( BorderFactory.createTitledBorder( "Info for Selected Huc8" ) );
        JPanel vPanel = new JPanel();
        
//                                                                           X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _selectedItemPanel, _code12LTF,   0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _code34LTF,   0,  1,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _code56LTF,   0,  2,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _code78LTF,   0,  3,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _code8LTF,    0,  4,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _desactLTF,   0,  5,    1,     1, GridBagConstraints.NONE );
    }


    private void initHashMaps()
    {
        _huc8RowDataToHuc8Map = _dataMgr.getHuc8RowDataToHuc8Map();
    }

    private void initJTable() 
    {
        _huc8RowDataList = _dataMgr.getHuc8RowDataList();
        setCEColumnDescriptorList();
        _huc8TableManager = new ComplexJTableManager( _huc8ColumnDescriptorList, _huc8RowDataList, ListSelectionModel.SINGLE_SELECTION );
        String columnsSelected[] = _huc8TableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _huc8TableManager.setDisplayableColumns( columnsSelected, true, true );
        _tableScrollPane = _huc8TableManager.getJScrollPane();
        _tableScrollPane.setPreferredSize( new Dimension( 300, 100 ) );
//                                                                                    X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _huc8SelectionPanel, _tableScrollPane,    0,   1,    1,     1, 1, 1, GridBagConstraints.BOTH );
    }

    private void updateJTable()
    {
        _huc8RowDataList = _dataMgr.getHuc8RowDataList();
        _huc8TableManager.setChangedAllRowDataList( _huc8RowDataList );
        _huc8TableManager.refreshDisplay();
    }

    private void setCEColumnDescriptorList()
    {
        _huc8ColumnDescriptorList = new ArrayList();

        _huc8ColumnDescriptorList.add(new JTableColumnDescriptor( "Code12", true, 60, "center" ) );
        _huc8ColumnDescriptorList.add(new JTableColumnDescriptor( "Code34", true, 60, "center" ) );
        _huc8ColumnDescriptorList.add(new JTableColumnDescriptor( "Code56", true, 60, "center" ) );
        _huc8ColumnDescriptorList.add(new JTableColumnDescriptor( "Code78", true, 60, "center" ) );
        _huc8ColumnDescriptorList.add(new JTableColumnDescriptor( "Code8", true, 80, "center" ) );
        _huc8ColumnDescriptorList.add(new JTableColumnDescriptor( "Desc", true, 800, "center" ) );
    }
    
    private void populateDataInputPanel( Huc8 huc8 )
    {
        if ( huc8 != null )
        {
            _code12LTF.setTextField( huc8.getCode12() );
            _code34LTF.setTextField( huc8.getCode34() );
            _code56LTF.setTextField( huc8.getCode56() );
            _code78LTF.setTextField( huc8.getCode78() );
            _code8LTF.setTextField( huc8.getCode8() );
            _desactLTF.setTextField( huc8.getDescat() );
        }
    }
    
    private void clearDataInputPanel()
    {
        _code12LTF.setTextField( "" );
        _code34LTF.setTextField( "" );
        _code56LTF.setTextField( "" );
        _code78LTF.setTextField( "" );
        _code8LTF.setTextField( "" );
        _desactLTF.setTextField( "" );
    }
    
    private boolean saveHuc8()
    {
        boolean saved = false;
        
        Huc8 huc8 = new Huc8();

        huc8.setCode12( _code12LTF.getTextFieldText() );
        huc8.setCode34( _code34LTF.getTextFieldText() );
        huc8.setCode56( _code56LTF.getTextFieldText() );
        huc8.setCode78( _code78LTF.getTextFieldText() );
        huc8.setCode8(  _code8LTF.getTextFieldText() );
        huc8.setDescat( _desactLTF.getTextFieldText() );
        
        saved = _dataMgr.saveHuc8( huc8 );
        
        updateJTable();

        return saved;
    }
    
    private void deleteHuc8()
    {
        Huc8 huc8 = null;
        List selectedRowsData = _huc8TableManager.getSelectedRowsData();
        
        if ( ! selectedRowsData.isEmpty() )
        {
            ArcBaseHuc8JTableRowData rowData = (ArcBaseHuc8JTableRowData) selectedRowsData.get( 0 );
            huc8 = (Huc8) _huc8RowDataToHuc8Map.get( rowData );
            if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + huc8.keyString(), "Delete Huc8" ) )
            {
                _dataMgr.deleteHuc8FromDataBase( huc8 );
                clearDataInputPanel();
                updateJTable();
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select a Huc8", "Delete Huc8" );
        }
    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();

        _closeButton.addActionListener( windowCloser );
        addWindowListener( windowCloser );
        _huc8TableManager.addTableListener( new MCETableListener() );
        
        _saveButton.addActionListener( new SaveButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
    }
    
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveHuc8();
        }
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveHuc8() )   // If the save is successful, close the window
            {
                closeWindow();
            }
        }
    }
    
    
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteHuc8();
        }
    }
    
    private class MCETableListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                Huc8 huc8 = null;

                ArcBaseHuc8JTableRowData rowData = (ArcBaseHuc8JTableRowData) _huc8TableManager.getSelectedRowsData().get( 0 );
                huc8 = (Huc8) _huc8RowDataToHuc8Map.get( rowData );
                populateDataInputPanel( huc8 );
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

        Huc8Editor huc8Editor = new Huc8Editor( frame, dataMgr );
        huc8Editor.displayGUI();
    }
}
