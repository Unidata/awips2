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
import ohd.hseb.raxbase.model.ShefProb;
import ohd.hseb.raxbase.table.ArcBaseShefProbJTableRowData;
import ohd.hseb.util.StringDataConverter;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledTextField;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class ShefProbEditor extends JDialog
{
    private Container _dialogContentPane = getContentPane();
    
    private RaxBaseDataMgr _dataMgr = null;
    
//  JTable variables
    private JTableManager _shefProbTableManager = null;
    private List _shefProbColumnDescriptorList = null;
    private List _shefProbRowDataList = null;
    private JScrollPane _tableScrollPane = null;
    private JPanel _shefProbSelectionPanel = new JPanel( new GridBagLayout() );

    private JPanel _selectedItemPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _pLTF = new LabeledTextField( "P:", "", "Owner agency location", 2 );
    private LabeledTextField _probLTF = new LabeledTextField( "Probability:", "", "Numerical value of forecast probability as defined in Table 6 of the SHEF manual (two codes do not have assigned probability values in the SHEF Manual, M (mean) and Z (filler); the SHEF Parser program assigns values of M = -0.5, Z = 1.0)", 15 );
    private LabeledTextField _nameLTF = new LabeledTextField( "Name:", "", "Description (mixed case)", 21 );
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _deleteButton = new JButton( "Delete" );

    private Map _shefProbRowDataToShefProbMap = null;

    public ShefProbEditor( JFrame frame, RaxBaseDataMgr dataMgr )
    {
        super( frame, "ShefProb Editor", true );
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
        initShefProbSelectionPanel();
        initSelectedItemPanel();
        initButtonPanel();
        initFrameComponents();
        initHashMaps();
        addListeners();
        pack();
        setVisible( true );
    }
    
    private void initShefProbSelectionPanel()
    {
        initJTable();
    }
    
    private void initFrameComponents()
    {
//                                                                                         X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _shefProbSelectionPanel,     0,  0,     4,     20, 1, 1, GridBagConstraints.VERTICAL );
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
        _selectedItemPanel.setBorder( BorderFactory.createTitledBorder( "Info for Selected ShefProb" ) );
        JPanel vPanel = new JPanel();
        
//                                                                                X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _selectedItemPanel, _pLTF,            0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _probLTF,  0,  1,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _nameLTF,    0,  2,    1,     1, GridBagConstraints.NONE );
    }


    private void initHashMaps()
    {
        _shefProbRowDataToShefProbMap = _dataMgr.getShefProbRowDataToShefProbMap();
    }

    private void initJTable() 
    {
        _shefProbRowDataList = _dataMgr.getShefProbRowDataList();
        setCEColumnDescriptorList();
        _shefProbTableManager = new ComplexJTableManager( _shefProbColumnDescriptorList, _shefProbRowDataList, ListSelectionModel.SINGLE_SELECTION );
        String columnsSelected[] = _shefProbTableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _shefProbTableManager.setDisplayableColumns( columnsSelected, true, true );
        _tableScrollPane = _shefProbTableManager.getJScrollPane();
        _tableScrollPane.setPreferredSize( new Dimension( 345, 100 ) );
//                                                                                    X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _shefProbSelectionPanel, _tableScrollPane,    0,   1,    1,     1, 1, 1, GridBagConstraints.VERTICAL );
    }

    private void updateJTable()
    {
        _dataMgr.initShefProbList();
        _shefProbRowDataList = _dataMgr.getShefProbRowDataList();
        _shefProbTableManager.setChangedAllRowDataList( _shefProbRowDataList );
        _shefProbTableManager.refreshDisplay();
    }

    private void setCEColumnDescriptorList()
    {
        _shefProbColumnDescriptorList = new ArrayList();

        _shefProbColumnDescriptorList.add(new JTableColumnDescriptor( "P", true, 50, "center" ) );
        _shefProbColumnDescriptorList.add(new JTableColumnDescriptor( "Probability", true, 100, "center" ) );
        _shefProbColumnDescriptorList.add(new JTableColumnDescriptor( "Name", true, 175, "center" ) );
    }
    
    private void populateDataInputPanel( ShefProb shefProb )
    {
        if ( shefProb != null )
        {
            _pLTF.setTextField( shefProb.getP() );
            _probLTF.setTextField( shefProb.getProbability() );
            _nameLTF.setTextField( shefProb.getName() );
        }
    }
    
    private void clearDataInputPanel()
    {
        _pLTF.setTextField( "" );
        _probLTF.setTextField( "" );
        _nameLTF.setTextField( "" );
    }
    
    private boolean saveShefProb()
    {
        boolean saved = false;
        
        StringDataConverter converter = new StringDataConverter();
        
        if ( ! _pLTF.getTextFieldText().equalsIgnoreCase( "" ) )  
        {
            ShefProb shefProb = new ShefProb();

            shefProb.setP( _pLTF.getTextFieldText() );
            shefProb.setName( _nameLTF.getTextFieldText() );
            shefProb.setProbability( converter.getFloatValue( _probLTF.getTextFieldText() ) );
            
            saved = _dataMgr.saveShefProb( shefProb );
            
            updateJTable();
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "Please enter a value for P", "Save ShefProb" );
        }

        return saved;
    }
    
    private void deleteShefProb()
    {
        ShefProb shefProb = null;
        List selectedRowsData = _shefProbTableManager.getSelectedRowsData();
        
        if ( ! selectedRowsData.isEmpty() )
        {
            ArcBaseShefProbJTableRowData rowData = (ArcBaseShefProbJTableRowData) selectedRowsData.get( 0 );
            shefProb = (ShefProb) _shefProbRowDataToShefProbMap.get( rowData );
            if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + shefProb.keyString(), "Delete ShefProb" ) )
            {
                _dataMgr.deleteShefProbFromDataBase( shefProb );
                clearDataInputPanel();
                updateJTable();
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select a ShefProb", "Delete ShefProb" );
        }
    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();

        _closeButton.addActionListener( windowCloser );
        addWindowListener( windowCloser );
        _shefProbTableManager.addTableListener( new MCETableListener() );
        
        _saveButton.addActionListener( new SaveButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
    }
    
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveShefProb();
        }
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveShefProb() )   // If the save is successful, close the window
            {
                closeWindow();
            }
        }
    }
    
    
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteShefProb();
        }
    }
    
    private class MCETableListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                ShefProb shefProb = null;

                ArcBaseShefProbJTableRowData rowData = (ArcBaseShefProbJTableRowData) _shefProbTableManager.getSelectedRowsData().get( 0 );
                shefProb = (ShefProb) _shefProbRowDataToShefProbMap.get( rowData );
                populateDataInputPanel( shefProb );
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

        ShefProbEditor shefProbEditor = new ShefProbEditor( frame, dataMgr );
        shefProbEditor.displayGUI();
    }
}
