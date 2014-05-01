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
import ohd.hseb.raxbase.model.ShefDuration;
import ohd.hseb.raxbase.table.ArcBaseShefDurationJTableRowData;
import ohd.hseb.util.StringDataConverter;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledTextField;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class ShefDurationEditor extends JDialog
{
    private Container _dialogContentPane = getContentPane();
    
    private RaxBaseDataMgr _dataMgr = null;
    
//  JTable variables
    private JTableManager _shefDurTableManager = null;
    private List _shefDurColumnDescriptorList = null;
    private List _shefDurRowDataList = null;
    private JScrollPane _tableScrollPane = null;
    private JPanel _shefDurSelectionPanel = new JPanel( new GridBagLayout() );

    private JPanel _selectedItemPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _durationLTF = new LabeledTextField( "Duration:", "", "SHEF Duration code", 2 );
    private LabeledTextField _iDurationLTF = new LabeledTextField( "IDuration:", "", "Numeric value of SHEF Duration code", 9 );
    private LabeledTextField _nameLTF = new LabeledTextField( "Name:", "", "Description (mixed case)", 21 );
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _deleteButton = new JButton( "Delete" );

    private Map _shefDurRowDataToShefDurMap = null;

    public ShefDurationEditor( JFrame frame, RaxBaseDataMgr dataMgr )
    {
        super( frame, "ShefDuration Editor", true );
        _dataMgr = dataMgr;
    }
    
    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 330, 400 ) );
        initGUI();
    }
    
    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        initShefDurationSelectionPanel();
        initSelectedItemPanel();
        initButtonPanel();
        initFrameComponents();
        initHashMaps();
        addListeners();
        pack();
        setVisible( true );
    }
    
    private void initShefDurationSelectionPanel()
    {
        initJTable();
    }
    
    private void initFrameComponents()
    {
//                                                                                         X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _shefDurSelectionPanel,     0,  0,     4,     20, 1, 1, GridBagConstraints.BOTH );
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
        _selectedItemPanel.setBorder( BorderFactory.createTitledBorder( "Info for Selected ShefDur" ) );
        JPanel vPanel = new JPanel();
        
//                                                                           X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _selectedItemPanel, _durationLTF,   0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _iDurationLTF,    0,  1,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _nameLTF,   0,  2,    1,     1, GridBagConstraints.NONE );
    }


    private void initHashMaps()
    {
        _shefDurRowDataToShefDurMap = _dataMgr.getShefDurRowDataToShefDurMap();
    }

    private void initJTable() 
    {
        _shefDurRowDataList = _dataMgr.getShefDurRowDataList();
        setCEColumnDescriptorList();
        _shefDurTableManager = new ComplexJTableManager( _shefDurColumnDescriptorList, _shefDurRowDataList, ListSelectionModel.SINGLE_SELECTION );
        String columnsSelected[] = _shefDurTableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _shefDurTableManager.setDisplayableColumns( columnsSelected, true, true );
        _tableScrollPane = _shefDurTableManager.getJScrollPane();
        _tableScrollPane.setPreferredSize( new Dimension( 300, 100 ) );
//                                                                                    X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _shefDurSelectionPanel, _tableScrollPane,    0,   1,    1,     1, 1, 1, GridBagConstraints.BOTH );
    }

    private void updateJTable()
    {
        _shefDurRowDataList = _dataMgr.getShefDurRowDataList();
        _shefDurTableManager.setChangedAllRowDataList( _shefDurRowDataList );
        _shefDurTableManager.refreshDisplay();
    }

    private void setCEColumnDescriptorList()
    {
        _shefDurColumnDescriptorList = new ArrayList();

        _shefDurColumnDescriptorList.add(new JTableColumnDescriptor( "Dur", true, 50, "center" ) );
        _shefDurColumnDescriptorList.add(new JTableColumnDescriptor( "IDur", true, 50, "center" ) );
        _shefDurColumnDescriptorList.add(new JTableColumnDescriptor( "Name", true, 200, "center" ) );
    }
    
    private void populateDataInputPanel( ShefDuration shefDur )
    {
        if ( shefDur != null )
        {
            _durationLTF.setTextField( shefDur.getDuration() );
            _iDurationLTF.setTextField( shefDur.getIduration() );
            _nameLTF.setTextField( shefDur.getName() );
        }
    }
    
    private void clearDataInputPanel()
    {
        _durationLTF.setTextField( "" );
        _iDurationLTF.setTextField( "" );
        _nameLTF.setTextField( "" );
    }
    
    private boolean saveShefDuration()
    {
        boolean saved = false;
        StringDataConverter converter = new StringDataConverter();
        
        if ( ! ( ( _iDurationLTF.getTextFieldText().equalsIgnoreCase( "" ) ) ||
             ( _durationLTF.getTextFieldText().equalsIgnoreCase( "" ) ) ) )
        {
            ShefDuration shefDur = new ShefDuration();

            shefDur.setDuration( _durationLTF.getTextFieldText() );
            shefDur.setIduration( converter.getShortValue( _iDurationLTF.getTextFieldText() ) );
            shefDur.setName( _nameLTF.getTextFieldText() );
            
            saved = _dataMgr.saveShefDur( shefDur );
            
            updateJTable();
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "Please enter a value for IDuration/Duration", "Save Duration" );
        }

        return saved;
    }
    
    private void deleteShefDuration()
    {
        ShefDuration shefDur = null;
        List selectedRowsData = _shefDurTableManager.getSelectedRowsData();
        
        if ( ! selectedRowsData.isEmpty() )
        {
            ArcBaseShefDurationJTableRowData rowData = (ArcBaseShefDurationJTableRowData) selectedRowsData.get( 0 );
            shefDur = (ShefDuration) _shefDurRowDataToShefDurMap.get( rowData );
            if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + shefDur.keyString(), "Delete ShefDuration" ) )
            {
                _dataMgr.deleteShefDurationFromDataBase( shefDur );
                clearDataInputPanel();
                updateJTable();
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select a ShefDuration", "Delete ShefDuration" );
        }
    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();

        _closeButton.addActionListener( windowCloser );
        addWindowListener( windowCloser );
        _shefDurTableManager.addTableListener( new MCETableListener() );
        
        _saveButton.addActionListener( new SaveButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
    }
    
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveShefDuration();
        }
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveShefDuration() )   // If the save is successful, close the window
            {
                closeWindow();
            }
        }
    }
    
    
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteShefDuration();
        }
    }
    
    private class MCETableListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                ShefDuration shefDuration = null;

                ArcBaseShefDurationJTableRowData rowData = (ArcBaseShefDurationJTableRowData) _shefDurTableManager.getSelectedRowsData().get( 0 );
                shefDuration = (ShefDuration) _shefDurRowDataToShefDurMap.get( rowData );
                populateDataInputPanel( shefDuration );
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

        ShefDurationEditor shefDurationEditor = new ShefDurationEditor( frame, dataMgr );
        shefDurationEditor.displayGUI();
    }
}
