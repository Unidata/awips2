package ohd.hseb.raxbase.gui;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
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
import ohd.hseb.raxbase.model.ShefPETrans;
import ohd.hseb.raxbase.table.ArcBaseShefPETransJTableRowData;
import ohd.hseb.util.StringDataConverter;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledTextField;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class ShefPETransEditor extends JDialog
{
    private Container _dialogContentPane = getContentPane();
    
    private RaxBaseDataMgr _dataMgr = null;
    
//  JTable variables
    private JTableManager _shefPeTransTableManager = null;
    private List _shefPeTransColumnDescriptorList = null;
    private List _shefPeTransRowDataList = null;
    private JScrollPane _tableScrollPane = null;
    private JPanel _shefPeTransSelectionPanel = new JPanel( new GridBagLayout() );

    private JPanel _selectedItemPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _peLTF = new LabeledTextField( "PE:", "", "SHEF PE code", 3 );
    private LabeledTextField _codePositionLTF = new LabeledTextField( "CodePosition:", "", "Position of code within value sent with certain SHEF PE codes (specifically IR and SR), valid values are \" \" (space), 1, 2, 3,and 4 (depending on the SHEF PE code)", 2 );
    private LabeledTextField _codedValueLTF = new LabeledTextField( "CodedValue:", "", "Integer code used by SHEF PEs (AF, AM, GR, GS, HI, IR, NC, PE, PT, SR, XP, and XW) that are not data values but rather translate into English phrases (see the V1.3 SHEF Manual)", 9 );
    private LabeledTextField _valueTransLTF = new LabeledTextField( "ValueTrans:", "", "Description of what the coded value translates to", 81 );
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _deleteButton = new JButton( "Delete" );

    private Map _shefPeTransRowDataToShefPeTransMap = null;

    public ShefPETransEditor( JFrame frame, RaxBaseDataMgr dataMgr )
    {
        super( frame, "ShefPETrans Editor", true );
        _dataMgr = dataMgr;
    }
    
    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 1000, 400 ) );
        initGUI();
    }
    
    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        initShefPeTransSelectionPanel();
        initSelectedItemPanel();
        initButtonPanel();
        initFrameComponents();
        initHashMaps();
        addListeners();
        pack();
        setVisible( true );
    }
    
    private void initShefPeTransSelectionPanel()
    {
        initJTable();
    }
    
    private void initFrameComponents()
    {
//                                                                                         X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _shefPeTransSelectionPanel,     0,  0,     4,     20, 1, 1, GridBagConstraints.VERTICAL );
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
        _selectedItemPanel.setBorder( BorderFactory.createTitledBorder( "Info for Selected ShefPETrans" ) );
        JPanel vPanel = new JPanel();
        
//                                                                                X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _selectedItemPanel, _peLTF,            0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _codePositionLTF,  0,  1,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _codedValueLTF,    0,  2,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _valueTransLTF,    0,  3,    1,     1, GridBagConstraints.NONE );
        
        _codePositionLTF.setTextFieldWidth( 2 );
    }


    private void initHashMaps()
    {
        _shefPeTransRowDataToShefPeTransMap = _dataMgr.getShefPeTransRowDataToShefPeTransMap();
    }

    private void initJTable() 
    {
        _shefPeTransRowDataList = _dataMgr.getShefPeTransRowDataList();
        setCEColumnDescriptorList();
        _shefPeTransTableManager = new ComplexJTableManager( _shefPeTransColumnDescriptorList, _shefPeTransRowDataList, ListSelectionModel.SINGLE_SELECTION );
        String columnsSelected[] = _shefPeTransTableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _shefPeTransTableManager.setDisplayableColumns( columnsSelected, true, true );
        _tableScrollPane = _shefPeTransTableManager.getJScrollPane();
        _tableScrollPane.setPreferredSize( new Dimension( 270, 100 ) );
//                                                                                    X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _shefPeTransSelectionPanel, _tableScrollPane,    0,   1,    1,     1, 1, 1, GridBagConstraints.VERTICAL );
    }

    private void updateJTable()
    {
        _shefPeTransRowDataList = _dataMgr.getShefPeTransRowDataList();
        _shefPeTransTableManager.setChangedAllRowDataList( _shefPeTransRowDataList );
        _shefPeTransTableManager.refreshDisplay();
    }

    private void setCEColumnDescriptorList()
    {
        _shefPeTransColumnDescriptorList = new ArrayList();

        _shefPeTransColumnDescriptorList.add(new JTableColumnDescriptor( "PE", true, 50, "center" ) );
        _shefPeTransColumnDescriptorList.add(new JTableColumnDescriptor( "CodePosition", true, 100, "center" ) );
        _shefPeTransColumnDescriptorList.add(new JTableColumnDescriptor( "CodedValue", true, 100, "center" ) );
    }
    
    private void populateDataInputPanel( ShefPETrans shefPeTrans )
    {
        if ( shefPeTrans != null )
        {
            _peLTF.setTextField( shefPeTrans.getPe() );
            _codePositionLTF.setTextField( shefPeTrans.getCodePosition() );
            _codedValueLTF.setTextField( shefPeTrans.getCodedValue() );
            _valueTransLTF.setTextField( shefPeTrans.getValueTrans() );
        }
    }
    
    private void clearDataInputPanel()
    {
        _peLTF.setTextField( "" );
        _codePositionLTF.setTextField( "" );
        _codedValueLTF.setTextField( "" );
        _valueTransLTF.setTextField( "" );
    }
    
    private boolean saveShefPeTrans()
    {
        boolean saved = false;
        boolean validNumber = true;
        StringDataConverter converter = new StringDataConverter();
        
        if ( ! ( ( _peLTF.getTextFieldText().equalsIgnoreCase( "" ) ) ||
                ( _codePositionLTF.getTextFieldText().equalsIgnoreCase( "" ) ) ||
                ( _codedValueLTF.getTextFieldText().equalsIgnoreCase( "" ) ) ) ) 
        {
            ShefPETrans shefPeTrans = new ShefPETrans();

            shefPeTrans.setPe( _peLTF.getTextFieldText() );
            shefPeTrans.setCodePosition( _codePositionLTF.getTextFieldText() );
            try
            {
                shefPeTrans.setCodedValue( converter.getIntValue( _codedValueLTF.getTextFieldText() ) );
            }
            catch ( NumberFormatException e )
            {
                DialogHelper.displayErrorDialog( this, "Please enter a valid numeric value for Coded Value", "Save ShefPETrans" );
                validNumber = false;
            }
           
            if ( validNumber )
            {
                shefPeTrans.setValueTrans( _valueTransLTF.getTextFieldText() );

                saved = _dataMgr.saveShefPeTrans( shefPeTrans );

                updateJTable();
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "Please enter a value for PE/CodePosition/CodedValue", "Save ShefPETrans" );
        }

        return saved;
    }
    
    private void deleteShefPeTrans()
    {
        ShefPETrans shefPeTrans = null;
        List selectedRowsData = _shefPeTransTableManager.getSelectedRowsData();
        
        if ( ! selectedRowsData.isEmpty() )
        {
            ArcBaseShefPETransJTableRowData rowData = (ArcBaseShefPETransJTableRowData) selectedRowsData.get( 0 );
            shefPeTrans = (ShefPETrans) _shefPeTransRowDataToShefPeTransMap.get( rowData );
            if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + shefPeTrans.keyString(), "Delete ShefPETrans" ) )
            {
                _dataMgr.deleteShefPeTransFromDataBase( shefPeTrans );
                clearDataInputPanel();
                updateJTable();
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select a ShefPETrans", "Delete ShefPETrans" );
        }
    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();

        _closeButton.addActionListener( windowCloser );
        addWindowListener( windowCloser );
        _shefPeTransTableManager.addTableListener( new MCETableListener() );
        
        _saveButton.addActionListener( new SaveButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
        _codePositionLTF.addTextFieldKeyListener( new CodePositionKeyAdapter() );
    }
    
    private class CodePositionKeyAdapter implements KeyListener
    {

        public void keyPressed( KeyEvent arg0 ){}
        public void keyTyped( KeyEvent e ){}
        public void keyReleased( KeyEvent arg0 )
        {
            String codePositionString = _codePositionLTF.getTextFieldText();
            int length = codePositionString.length();
            if ( length > 1 )
            {
                _codePositionLTF.setTextField( codePositionString.substring( 0, 1 ) );
            }
        }

    }
    
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveShefPeTrans();
        }
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveShefPeTrans() )   // If the save is successful, close the window
            {
                closeWindow();
            }
        }
    }
    
    
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteShefPeTrans();
        }
    }
    
    private class MCETableListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                ShefPETrans shefPeTrans = null;

                ArcBaseShefPETransJTableRowData rowData = (ArcBaseShefPETransJTableRowData) _shefPeTransTableManager.getSelectedRowsData().get( 0 );
                shefPeTrans = (ShefPETrans) _shefPeTransRowDataToShefPeTransMap.get( rowData );
                populateDataInputPanel( shefPeTrans );
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

        ShefPETransEditor shefPETransEditor = new ShefPETransEditor( frame, dataMgr );
        shefPETransEditor.displayGUI();
    }
}
