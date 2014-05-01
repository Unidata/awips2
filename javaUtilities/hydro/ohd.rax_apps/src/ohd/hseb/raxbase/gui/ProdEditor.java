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
import ohd.hseb.raxbase.model.Prod;
import ohd.hseb.raxbase.table.ArcBaseProdJTableRowData;
import ohd.hseb.util.StringDataConverter;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledComboBox;
import ohd.hseb.util.gui.LabeledTextField;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class ProdEditor extends JDialog
{
    private Container _dialogContentPane = getContentPane();
    
    private RaxBaseDataMgr _dataMgr = null;
    
//  JTable variables
    private JTableManager _prodTableManager = null;
    private List _prodColumnDescriptorList = null;
    private List _prodRowDataList = null;
    private JScrollPane _tableScrollPane = null;
    private JPanel _prodSelectionPanel = new JPanel( new GridBagLayout() );

    private JPanel _selectedItemPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _idLTF = new LabeledTextField( "ID:", "", "Product identifier", 11 );
    private LabeledTextField _pmaxLTF = new LabeledTextField( "PMax:", "", "Number of versions to store", 15 );
    private JCheckBox _papCB = new JCheckBox( "Pap:" );
    private String[] errStringArray = { "-1", "0", "1" };
    private LabeledComboBox _errCB = new LabeledComboBox( "Err:", errStringArray, "Parse & post error print flag: -1=no, 0=yes if errors, 1=yes" );
    private JCheckBox _graCB = new JCheckBox( "Gra:" );
    private JCheckBox _pr1CB = new JCheckBox( "Pr1:" );
    private JCheckBox _netCB = new JCheckBox( "Net:" );
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _deleteButton = new JButton( "Delete" );

    private Map _prodRowDataToProdMap = null;

    public ProdEditor( JFrame frame, RaxBaseDataMgr dataMgr )
    {
        super( frame, "Prod Editor", true );
        _dataMgr = dataMgr;
    }
    
    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 600, 500 ) );
        initGUI();
    }
    
    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        initProdSelectionPanel();
        initSelectedItemPanel();
        initButtonPanel();
        initFrameComponents();
        initHashMaps();
        addListeners();
        pack();
        setVisible( true );
    }
    
    private void initProdSelectionPanel()
    {
        initJTable();
    }
    
    private void initFrameComponents()
    {
//                                                                                         X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _prodSelectionPanel,     0,  0,     4,     20, 1, 1, GridBagConstraints.BOTH );
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
        _selectedItemPanel.setBorder( BorderFactory.createTitledBorder( "Info for Selected Prod" ) );
        JPanel vPanel = new JPanel();
        
//                                                                       X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _selectedItemPanel, _idLTF,   0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _pmaxLTF, 0,  1,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _papCB,   0,  2,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _errCB,   0,  3,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _graCB,   0,  4,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _pr1CB,   0,  5,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _netCB,   0,  6,    1,     1, GridBagConstraints.NONE );
    }


    private void initHashMaps()
    {
        _prodRowDataToProdMap = _dataMgr.getProdRowDataToProdMap();
    }

    private void initJTable() 
    {
        _prodRowDataList = _dataMgr.getProdRowDataList();
        setCEColumnDescriptorList();
        _prodTableManager = new ComplexJTableManager( _prodColumnDescriptorList, _prodRowDataList, ListSelectionModel.SINGLE_SELECTION );
        String columnsSelected[] = _prodTableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _prodTableManager.setDisplayableColumns( columnsSelected, true, true );
        _tableScrollPane = _prodTableManager.getJScrollPane();
        _tableScrollPane.setPreferredSize( new Dimension( 525, 100 ) );
//                                                                                    X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _prodSelectionPanel, _tableScrollPane,    0,   1,    1,     1, 1, 1, GridBagConstraints.VERTICAL );
    }

    private void updateJTable()
    {
        _prodRowDataList = _dataMgr.getProdRowDataList();
        _prodTableManager.setChangedAllRowDataList( _prodRowDataList );
        _prodTableManager.refreshDisplay();
    }

    private void setCEColumnDescriptorList()
    {
        _prodColumnDescriptorList = new ArrayList();

        _prodColumnDescriptorList.add(new JTableColumnDescriptor( "Id", true, 100, "center" ) );
        _prodColumnDescriptorList.add(new JTableColumnDescriptor( "PMax", true, 100, "center" ) );
        _prodColumnDescriptorList.add(new JTableColumnDescriptor( "Pap", true, 60, "center" ) );
        _prodColumnDescriptorList.add(new JTableColumnDescriptor( "Err", true, 60, "center" ) );
        _prodColumnDescriptorList.add(new JTableColumnDescriptor( "Gra", true, 80, "center" ) );
        _prodColumnDescriptorList.add(new JTableColumnDescriptor( "Pr1", true, 60, "center" ) );
        _prodColumnDescriptorList.add(new JTableColumnDescriptor( "Net", true, 60, "center" ) );
    }
    
    private void populateDataInputPanel( Prod prod )
    {
        if ( prod != null )
        {
            _idLTF.setTextField( prod.getId() );
            _pmaxLTF.setTextField( prod.getPmax() );
            _papCB.setSelected( getBooleanValue( prod.getPap() ) );
            _errCB.setSelectedItem( getErrValue( prod.getErr() ) );
            _graCB.setSelected( getBooleanValue( prod.getGra() ) );
            _pr1CB.setSelected( getBooleanValue( prod.getPr1() ) );
            _netCB.setSelected( getBooleanValue( prod.getNet() ) );
        }
    }
    
    private String getErrValue( int errValue )
    {
        String returnString = null;
        
        if ( errValue == -1 )
        {
            returnString = "-1";
        }
        else if ( errValue == 0 )
        {
            returnString = "0";
        }
        else if ( errValue == 1 )
        {
            returnString = "1";
        }
        
        return returnString;
    }
    
    private boolean getBooleanValue( String stringValue )
    {
        boolean returnValue = false;
        
        if ( stringValue.equalsIgnoreCase( "Y" ) )
        {
            returnValue = true;
        }
        
        return returnValue;
    }

    private String getStringValue( boolean booleanValue )
    {
        String returnValue = "N";
        
        if ( booleanValue )
        {
            returnValue = "Y";
        }
        
        return returnValue;
    }
    
    private int getIntValue( boolean booleanValue )
    {
        int intValue = 0;
        
        if ( booleanValue )
        {
            intValue = 1;
        }
        
        return intValue;
    }
    
    private boolean getBooleanValue( int intValue )
    {
        boolean returnValue = false;
        
        if ( intValue == 1 )
        {
            returnValue = true;
        }
        
        return returnValue;
    }

    private void clearDataInputPanel()
    {
        _idLTF.setTextField( "" );
        _pmaxLTF.setTextField( "" );
        _papCB.setSelected( false );
        _errCB.setSelectedItem( "-1" );
        _graCB.setSelected( false );
        _pr1CB.setSelected( false );
        _netCB.setSelected( false );
    }
    
    private boolean saveProd()
    {
        boolean saved = false;
        StringDataConverter converter = new StringDataConverter();
        
        Prod prod = new Prod();

        prod.setId( _idLTF.getTextFieldText() );
        prod.setPmax( converter.getIntValue( _pmaxLTF.getTextFieldText() ) );
        prod.setPap( getIntValue( _papCB.isSelected() ) );
        prod.setErr( Integer.parseInt( (String) _errCB.getSelectedItem() ) );
        prod.setGra( getStringValue( _graCB.isSelected() ) );
        prod.setPr1( getStringValue( _pr1CB.isSelected() ) );
        prod.setNet( getStringValue( _netCB.isSelected() ) );
        
        saved = _dataMgr.saveProd( prod );
        
        updateJTable();

        return saved;
    }
    
    private void deleteProd()
    {
        Prod prod = null;
        List selectedRowsData = _prodTableManager.getSelectedRowsData();
        
        if ( ! selectedRowsData.isEmpty() )
        {
            ArcBaseProdJTableRowData rowData = (ArcBaseProdJTableRowData) selectedRowsData.get( 0 );
            prod = (Prod) _prodRowDataToProdMap.get( rowData );
            if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + prod.keyString(), "Delete Prod" ) )
            {
                _dataMgr.deleteProdFromDataBase( prod );
                clearDataInputPanel();
                updateJTable();
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select a Prod", "Delete Prod" );
        }
    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();

        _closeButton.addActionListener( windowCloser );
        addWindowListener( windowCloser );
        _prodTableManager.addTableListener( new MCETableListener() );
        
        _saveButton.addActionListener( new SaveButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
    }
    
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveProd();
        }
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveProd() )   // If the save is successful, close the window
            {
                closeWindow();
            }
        }
    }
    
    
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteProd();
        }
    }
    
    private class MCETableListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                Prod prod = null;

                ArcBaseProdJTableRowData rowData = (ArcBaseProdJTableRowData) _prodTableManager.getSelectedRowsData().get( 0 );
                prod = (Prod) _prodRowDataToProdMap.get( rowData );
                populateDataInputPanel( prod );
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

        ProdEditor prodEditor = new ProdEditor( frame, dataMgr );
        prodEditor.displayGUI();
    }
}
