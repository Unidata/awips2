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
import ohd.hseb.raxbase.model.ShefQC;
import ohd.hseb.raxbase.table.ArcBaseShefQCJTableRowData;
import ohd.hseb.util.StringDataConverter;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledTextField;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class ShefQCEditor extends JDialog
{
    private Container _dialogContentPane = getContentPane();
    
    private RaxBaseDataMgr _dataMgr = null;
    
//  JTable variables
    private JTableManager _shefQCTableManager = null;
    private List _shefQCColumnDescriptorList = null;
    private List _shefQCRowDataList = null;
    private JScrollPane _tableScrollPane = null;
    private JPanel _shefQCSelectionPanel = new JPanel( new GridBagLayout() );

    private JPanel _selectedItemPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _shefQualifierCodeLTF = new LabeledTextField( "Shef Qualifier Code:", "", "SHEF Qualifier code", 2 );
    private LabeledTextField _powerLTF = new LabeledTextField( "Ranking Power:", "", "Ranking order of quality codes", 15 );
    private LabeledTextField _nameLTF = new LabeledTextField( "Name:", "", "Description (mixed case)", 21 );
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _deleteButton = new JButton( "Delete" );

    private Map _shefQCRowDataToShefQCMap = null;

    public ShefQCEditor( JFrame frame, RaxBaseDataMgr dataMgr )
    {
        super( frame, "ShefQC Editor", true );
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
        initShefQCSelectionPanel();
        initSelectedItemPanel();
        initButtonPanel();
        initFrameComponents();
        initHashMaps();
        addListeners();
        pack();
        setVisible( true );
    }
    
    private void initShefQCSelectionPanel()
    {
        initJTable();
    }
    
    private void initFrameComponents()
    {
//                                                                                         X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _shefQCSelectionPanel,     0,  0,     4,     20, 1, 1, GridBagConstraints.VERTICAL );
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
        _selectedItemPanel.setBorder( BorderFactory.createTitledBorder( "Info for Selected ShefQC" ) );
        JPanel vPanel = new JPanel();
        
//                                                                                X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _selectedItemPanel, _shefQualifierCodeLTF,            0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _powerLTF,  0,  1,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _nameLTF,    0,  2,    1,     1, GridBagConstraints.NONE );
    }


    private void initHashMaps()
    {
        _shefQCRowDataToShefQCMap = _dataMgr.getShefQCRowDataToShefQCMap();
    }

    private void initJTable() 
    {
        _shefQCRowDataList = _dataMgr.getShefQCRowDataList();
        setCEColumnDescriptorList();
        _shefQCTableManager = new ComplexJTableManager( _shefQCColumnDescriptorList, _shefQCRowDataList, ListSelectionModel.SINGLE_SELECTION );
        String columnsSelected[] = _shefQCTableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _shefQCTableManager.setDisplayableColumns( columnsSelected, true, true );
        _tableScrollPane = _shefQCTableManager.getJScrollPane();
        _tableScrollPane.setPreferredSize( new Dimension( 420, 100 ) );
//                                                                                    X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _shefQCSelectionPanel, _tableScrollPane,    0,   1,    1,     1, 1, 1, GridBagConstraints.VERTICAL );
    }

    private void updateJTable()
    {
        _shefQCRowDataList = _dataMgr.getShefQCRowDataList();
        _shefQCTableManager.setChangedAllRowDataList( _shefQCRowDataList );
        _shefQCTableManager.refreshDisplay();
    }

    private void setCEColumnDescriptorList()
    {
        _shefQCColumnDescriptorList = new ArrayList();

        _shefQCColumnDescriptorList.add(new JTableColumnDescriptor( "ShefQCCode", true, 100, "center" ) );
        _shefQCColumnDescriptorList.add(new JTableColumnDescriptor( "Power", true, 50, "center" ) );
        _shefQCColumnDescriptorList.add(new JTableColumnDescriptor( "Name", true, 250, "center" ) );
    }
    
    private void populateDataInputPanel( ShefQC shefQC )
    {
        if ( shefQC != null )
        {
            _shefQualifierCodeLTF.setTextField( shefQC.getShefQualifierCode() );
            _powerLTF.setTextField( shefQC.getPower() );
            _nameLTF.setTextField( shefQC.getName() );
        }
    }
    
    private void clearDataInputPanel()
    {
        _shefQualifierCodeLTF.setTextField( "" );
        _powerLTF.setTextField( "" );
        _nameLTF.setTextField( "" );
    }
    
    private boolean saveShefQC()
    {
        boolean saved = false;
        
        StringDataConverter converter = new StringDataConverter();
        
        if ( ! _powerLTF.getTextFieldText().equalsIgnoreCase( "" ) )  
        {
            ShefQC shefQC = new ShefQC();

            shefQC.setShefQualifierCode( _shefQualifierCodeLTF.getTextFieldText() );
            shefQC.setPower( converter.getIntValue( _powerLTF.getTextFieldText() ) );
            shefQC.setName( _nameLTF.getTextFieldText() );
            
            saved = _dataMgr.saveShefQC( shefQC );
            
            updateJTable();
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "Please enter a value for Power", "Save ShefQC" );
        }

        return saved;
    }
    
    private void deleteShefQC()
    {
        ShefQC shefQC = null;
        List selectedRowsData = _shefQCTableManager.getSelectedRowsData();
        
        if ( ! selectedRowsData.isEmpty() )
        {
            ArcBaseShefQCJTableRowData rowData = (ArcBaseShefQCJTableRowData) selectedRowsData.get( 0 );
            shefQC = (ShefQC) _shefQCRowDataToShefQCMap.get( rowData );
            if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + shefQC.keyString(), "Delete ShefQC" ) )
            {
                _dataMgr.deleteShefQCFromDataBase( shefQC );
                clearDataInputPanel();
                updateJTable();
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select a ShefQC", "Delete ShefQC" );
        }
    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();

        _closeButton.addActionListener( windowCloser );
        addWindowListener( windowCloser );
        _shefQCTableManager.addTableListener( new MCETableListener() );
        
        _saveButton.addActionListener( new SaveButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
    }
    
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveShefQC();
        }
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveShefQC() )   // If the save is successful, close the window
            {
                closeWindow();
            }
        }
    }
    
    
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteShefQC();
        }
    }
    
    private class MCETableListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                ShefQC shefQC = null;

                ArcBaseShefQCJTableRowData rowData = (ArcBaseShefQCJTableRowData) _shefQCTableManager.getSelectedRowsData().get( 0 );
                shefQC = (ShefQC) _shefQCRowDataToShefQCMap.get( rowData );
                populateDataInputPanel( shefQC );
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

        ShefQCEditor shefQCEditor = new ShefQCEditor( frame, dataMgr );
        shefQCEditor.displayGUI();
    }
}
