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
import ohd.hseb.raxbase.model.Huc2;
import ohd.hseb.raxbase.table.ArcBaseHuc2JTableRowData;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledTextField;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class Huc2Editor extends JDialog
{
    private Container _dialogContentPane = getContentPane();
    
    private RaxBaseDataMgr _dataMgr = null;
    
//  JTable variables
    private JTableManager _huc2TableManager = null;
    private List _huc2ColumnDescriptorList = null;
    private List _huc2RowDataList = null;
    private JScrollPane _tableScrollPane = null;
    private JPanel _huc2SelectionPanel = new JPanel( new GridBagLayout() );

    private JPanel _selectedItemPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _code12LTF = new LabeledTextField( "Code12:", "", "USGS region code", 3 );
    private LabeledTextField _code2LTF = new LabeledTextField( "Code2:", "", "USGS region code", 3 );
    private LabeledTextField _desregLTF = new LabeledTextField( "Description:", "", "Description", 60 );
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _deleteButton = new JButton( "Delete" );

    private Map _huc2RowDataToHuc2Map = null;

    public Huc2Editor( JFrame frame, RaxBaseDataMgr dataMgr )
    {
        super( frame, "Huc2 Editor", true );
        _dataMgr = dataMgr;
    }
    
    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 940, 400 ) );
        initGUI();
    }
    
    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        initHuc2SelectionPanel();
        initSelectedItemPanel();
        initButtonPanel();
        initFrameComponents();
        initHashMaps();
        addListeners();
        pack();
        setVisible( true );
    }
    
    private void initHuc2SelectionPanel()
    {
        initJTable();
    }
    
    private void initFrameComponents()
    {
//                                                                                         X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _huc2SelectionPanel,     0,  0,     4,     20, 1, 1, GridBagConstraints.BOTH );
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
        _selectedItemPanel.setBorder( BorderFactory.createTitledBorder( "Info for Selected Huc2" ) );
        JPanel vPanel = new JPanel();
        
//                                                                           X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _selectedItemPanel, _code12LTF,   0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _code2LTF,    0,  1,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _desregLTF,   0,  2,    1,     1, GridBagConstraints.NONE );
    }


    private void initHashMaps()
    {
        _huc2RowDataToHuc2Map = _dataMgr.getHuc2RowDataToHuc2Map();
    }

    private void initJTable() 
    {
        _huc2RowDataList = _dataMgr.getHuc2RowDataList();
        setCEColumnDescriptorList();
        _huc2TableManager = new ComplexJTableManager( _huc2ColumnDescriptorList, _huc2RowDataList, ListSelectionModel.SINGLE_SELECTION );
        String columnsSelected[] = _huc2TableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _huc2TableManager.setDisplayableColumns( columnsSelected, true, true );
        _tableScrollPane = _huc2TableManager.getJScrollPane();
        _tableScrollPane.setPreferredSize( new Dimension( 300, 100 ) );
//                                                                                    X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _huc2SelectionPanel, _tableScrollPane,    0,   1,    1,     1, 1, 1, GridBagConstraints.BOTH );
    }

    private void updateJTable()
    {
        _huc2RowDataList = _dataMgr.getHuc2RowDataList();
        _huc2TableManager.setChangedAllRowDataList( _huc2RowDataList );
        _huc2TableManager.refreshDisplay();
    }

    private void setCEColumnDescriptorList()
    {
        _huc2ColumnDescriptorList = new ArrayList();

        _huc2ColumnDescriptorList.add(new JTableColumnDescriptor( "Code12", true, 50, "center" ) );
        _huc2ColumnDescriptorList.add(new JTableColumnDescriptor( "Code2", true, 50, "center" ) );
        _huc2ColumnDescriptorList.add(new JTableColumnDescriptor( "Desc", true, 800, "center" ) );
    }
    
    private void populateDataInputPanel( Huc2 huc2 )
    {
        if ( huc2 != null )
        {
            _code12LTF.setTextField( huc2.getCode12() );
            _code2LTF.setTextField( huc2.getCode2() );
            _desregLTF.setTextField( huc2.getDesreg() );
        }
    }
    
    private void clearDataInputPanel()
    {
        _code12LTF.setTextField( "" );
        _code2LTF.setTextField( "" );
        _desregLTF.setTextField( "" );
    }
    
    private boolean saveHuc2()
    {
        boolean saved = false;
        
        Huc2 huc2 = new Huc2();

        huc2.setCode12( _code12LTF.getTextFieldText() );
        huc2.setCode2(  _code2LTF.getTextFieldText() );
        huc2.setDesreg( _desregLTF.getTextFieldText() );
        
        saved = _dataMgr.saveHuc2( huc2 );
        
        updateJTable();

        return saved;
    }
    
    private void deleteHuc2()
    {
        Huc2 huc2 = null;
        List selectedRowsData = _huc2TableManager.getSelectedRowsData();
        
        if ( ! selectedRowsData.isEmpty() )
        {
            ArcBaseHuc2JTableRowData rowData = (ArcBaseHuc2JTableRowData) selectedRowsData.get( 0 );
            huc2 = (Huc2) _huc2RowDataToHuc2Map.get( rowData );
            if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + huc2.keyString(), "Delete Huc2" ) )
            {
                _dataMgr.deleteHuc2FromDataBase( huc2 );
                clearDataInputPanel();
                updateJTable();
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select a Huc2", "Delete Huc2" );
        }
    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();

        _closeButton.addActionListener( windowCloser );
        addWindowListener( windowCloser );
        _huc2TableManager.addTableListener( new MCETableListener() );
        
        _saveButton.addActionListener( new SaveButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
    }
    
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveHuc2();
        }
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveHuc2() )   // If the save is successful, close the window
            {
                closeWindow();
            }
        }
    }
    
    
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteHuc2();
        }
    }
    
    private class MCETableListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                Huc2 huc2 = null;

                ArcBaseHuc2JTableRowData rowData = (ArcBaseHuc2JTableRowData) _huc2TableManager.getSelectedRowsData().get( 0 );
                huc2 = (Huc2) _huc2RowDataToHuc2Map.get( rowData );
                populateDataInputPanel( huc2 );
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

        Huc2Editor huc2Editor = new Huc2Editor( frame, dataMgr );
        huc2Editor.displayGUI();
    }
}
