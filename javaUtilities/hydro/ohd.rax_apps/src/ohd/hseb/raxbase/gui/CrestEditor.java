package ohd.hseb.raxbase.gui;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.HashMap;
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
import ohd.hseb.raxbase.model.RaxCrest;
import ohd.hseb.raxbase.table.ArcBaseCrestJTableRowData;
import ohd.hseb.raxbase.util.DateManager;
import ohd.hseb.raxbase.util.HDateSuperChooser;
import ohd.hseb.rfc.util.graphics.HDateChooserOwner;
import ohd.hseb.util.StringDataConverter;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledComboBox;
import ohd.hseb.util.gui.LabeledTextField;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class CrestEditor extends JDialog implements HDateChooserOwner
{
    private Container _dialogContentPane = getContentPane();
    
    private RaxBaseDataMgr _dataMgr = null;
    
    private String _lid = null;

    private HDateSuperChooser _hDateChooser = new HDateSuperChooser( false );

    private StringDataConverter _converter = new StringDataConverter();
    
//  JTable variables
    private JTableManager _CETableManager = null;
    private List _CEColumnDescriptorList = null;
    private List _crestRowDataList = null;
    private JScrollPane _tableScrollPane = null;
    private JPanel _crestSelectionPanel = new JPanel( new GridBagLayout() );

    private JPanel _selectedItemPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _stageLTF = new LabeledTextField( "                     Stage:  ", "", "Stage", 10 );
    private LabeledTextField _stgQualLTF = new LabeledTextField( "Stage Quality Code:", "", "Quality Code", 10 );
    private LabeledTextField _flowLTF = new LabeledTextField( "                      Flow:  ", "", "Flow", 10 );
    private LabeledTextField _flowQualLTF = new LabeledTextField( " Flow Quality Code: ", "", "Quality Code", 10 );
    private LabeledTextField _dateLTF = new LabeledTextField( "Date:", "", "Date", 10 );
    private LabeledTextField _timeLTF = new LabeledTextField( "Time:", "", "Time", 10 );
    
    private JCheckBox _oldDatumCB = new JCheckBox( "Based on Old Datum" );
    private JCheckBox _hwMarkCB = new JCheckBox( "Observed by High Water Mark" );
    private JCheckBox _jamCB = new JCheckBox( "Affected by Ice Jam" );
    private String[] _prelimStringArray = { "", "Official Crest", "Record Crest", "Preliminary Status" };
    private LabeledComboBox _prelimLCB = new LabeledComboBox( "Prelim:", _prelimStringArray );
    private Map _prelimCBStringToPrelimMap = new HashMap();
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _newButton = new JButton( "New" );
    private JButton _deleteButton = new JButton( "Delete" );
    
    private Map _raxCrestRowDataToRaxCrestMap = null;
    
    public CrestEditor( JFrame frame, RaxBaseDataMgr dataMgr, String lid, String title )
    {
        super( frame, "Crest Editor - " + title, true );
        _dataMgr = dataMgr;
        _lid = lid;
    }
    
    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 500, 625 ) );
        initGUI();
    }
    
    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        initCrestSelectionPanel();
        initSelectedItemPanel();
        initButtonPanel();
        initFrameComponents();
        initHashMaps();
        addListeners();
        pack();
        setVisible( true );
    }
    
    private void initCrestSelectionPanel()
    {
        initJTable();
        
        ComponentHelper.addPanelComponent( _crestSelectionPanel, _deleteButton,    0,   2,    1,     1, 1, 1, GridBagConstraints.NONE );

    }
    
    private void initSelectedItemPanel()
    {
        JPanel cbPanel = new JPanel( new GridBagLayout() );
        _selectedItemPanel.setBorder( BorderFactory.createTitledBorder( "Info for Selected Crest" ) );
        JPanel vPanel = new JPanel();
//        vPanel.setBackground( Color.RED );
        _dateLTF.setEditTextField( false );
        _stgQualLTF.setLabelPreferredSize( new Dimension( 130, 15 ) );
        
//                                                                             X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( cbPanel, _oldDatumCB,                0,  0,    1,     1, GridBagConstraints.BOTH );
        ComponentHelper.addPanelComponent( cbPanel, _hwMarkCB,                  0,  1,    1,     1, GridBagConstraints.BOTH );
        ComponentHelper.addPanelComponent( cbPanel, _jamCB,                     0,  2,    1,     1, GridBagConstraints.BOTH );
        
        ComponentHelper.addPanelComponent( _selectedItemPanel, _stageLTF,       0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _stgQualLTF,     0,  1,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _flowLTF,        0,  2,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _flowQualLTF,    0,  3,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _dateLTF,        1,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _timeLTF,        1,  1,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, cbPanel,         0,  4,    3,     1, GridBagConstraints.NONE );

//        ComponentHelper.addPanelComponent( _selectedItemPanel, _prelimComboBox, 1,  3,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _prelimLCB,      1,  3,    2,     1, GridBagConstraints.BOTH );
        
        
        for ( int i = 1; i < _prelimStringArray.length; i++ )
        {
            String prelimCharString = _prelimStringArray[ i ].substring( 0, 1 );
            _prelimCBStringToPrelimMap.put( _prelimStringArray[ i ], prelimCharString );
            _prelimCBStringToPrelimMap.put( prelimCharString, _prelimStringArray[ i ] );
        }
        _hDateChooser.addOwner( this );
    }
    
    private void initButtonPanel()
    {
//                                                                              X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _buttonPanel, _saveAndCloseButton,   0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _saveButton,           1,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _closeButton,          2,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _newButton,            3,  0,    1,     1, GridBagConstraints.NONE );
//        ComponentHelper.addPanelComponent( _buttonPanel, _deleteButton,         4,  0,    1,     1, GridBagConstraints.NONE );
    }
    
    private void initFrameComponents()
    {
//                                                                                         X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _crestSelectionPanel,       0,  0,     4,     20, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _selectedItemPanel,         0,  21,    4,     1, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _buttonPanel,               0,  22,    4,     1, 1, 1, GridBagConstraints.BOTH );

    }
    
    private void initHashMaps()
    {
        _raxCrestRowDataToRaxCrestMap = _dataMgr.getRaxCrestRowDataToRaxCrestMap();
    }
    
    private void initJTable() 
    {
        _crestRowDataList = _dataMgr.getCrestRowDataList( _lid );
        setCEColumnDescriptorList();
        _CETableManager = new ComplexJTableManager( _CEColumnDescriptorList, _crestRowDataList, ListSelectionModel.SINGLE_SELECTION );
        String columnsSelected[] = _CETableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _CETableManager.setDisplayableColumns( columnsSelected, true, true );
        _tableScrollPane = _CETableManager.getJScrollPane();
        _tableScrollPane.setPreferredSize( new Dimension( 300, 300 ) );
//                                                                                    X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _crestSelectionPanel, _tableScrollPane,    0,   1,    1,     1, 1, 1, GridBagConstraints.BOTH );
    }
    
    private void updateJTable()
    {
        _crestRowDataList = _dataMgr.getCrestRowDataList( _lid );
        _CETableManager.setChangedAllRowDataList( _crestRowDataList );
        _CETableManager.refreshDisplay();
    }
    
    private void setCEColumnDescriptorList()
    {
        _CEColumnDescriptorList = new ArrayList();

        _CEColumnDescriptorList.add(new JTableColumnDescriptor( "Stage", true, 120, "center" ) );
        _CEColumnDescriptorList.add(new JTableColumnDescriptor( "Flow", true, 120, "center" ) );
        _CEColumnDescriptorList.add(new JTableColumnDescriptor( "Date", true, 120, "center" ) );
        _CEColumnDescriptorList.add(new JTableColumnDescriptor( "Time", true, 120, "center" ) );
    }
    
    private void clearDataInputPanel()
    {
        _stageLTF.setTextField( "" );
        _stgQualLTF.setTextField( "" );
        _flowLTF.setTextField( "" );
        _flowQualLTF.setTextField( "" );
        _dateLTF.setTextField( "" );
        _timeLTF.setTextField( "" );
        _oldDatumCB.setSelected( false );
        _hwMarkCB.setSelected( false );
        _jamCB.setSelected( false );
        _prelimLCB.setSelectedIndex( 0 );
    }
    
    private void populateDataInputPanel( RaxCrest raxCrest )
    {
        _stageLTF.setTextField( raxCrest.getStage() );
        _stgQualLTF.setTextField( raxCrest.getStgQual() );
        _flowLTF.setTextField( raxCrest.getFlow() );
        _flowQualLTF.setTextField(  raxCrest.getFlowQual() );
        _dateLTF.setTextFieldDate( raxCrest.getDateCrest() );
        _timeLTF.setTextField( raxCrest.getCrestDateTime() );
        
        if ( raxCrest.getOldDatum().equalsIgnoreCase(  "Y" ) )
        {
            _oldDatumCB.setSelected( true );
        }
        else
        {
            _oldDatumCB.setSelected( false );
        }
        
        if ( raxCrest.getHw().equalsIgnoreCase( "Y" ) )
        {
            _hwMarkCB.setSelected( true );
        }
        else
        {
            _hwMarkCB.setSelected( false );
        }
        
        if ( raxCrest.getJam().equalsIgnoreCase( "Y" ) )
        {
            _jamCB.setSelected( true );
        }
        else
        {
            _jamCB.setSelected( false );
        }
        
        String prelim = raxCrest.getPrelim();
        _prelimLCB.setSelectedItem( (String) _prelimCBStringToPrelimMap.get( prelim ) );
    }
    
    private boolean saveCrest()
    {
        boolean saved = false;
        
        RaxCrest raxCrest = new RaxCrest();
        String dateString = _dateLTF.getTextFieldText().trim();
        
        if ( ! dateString.equalsIgnoreCase( "" ) )
        {
            raxCrest.setLid( _lid );

            raxCrest.setDateCrest( _converter.getLongDateValue( dateString ) );
            
            raxCrest.setCrestDateTime( _timeLTF.getTextFieldText() );
            raxCrest.setStage( _converter.getDoubleValue( _stageLTF.getTextFieldText() ) );
            raxCrest.setStgQual( _stgQualLTF.getTextFieldText() );
            raxCrest.setFlow( _converter.getDoubleValue( _flowLTF.getTextFieldText() ) );
            raxCrest.setFlowQual( _flowQualLTF.getTextFieldText() );
            raxCrest.setHw( getYNStringFromCB( _hwMarkCB ) );
            raxCrest.setJam( getYNStringFromCB( _jamCB ) );
            raxCrest.setOldDatum( getYNStringFromCB( _oldDatumCB ) );
            String prelimString = (String) _prelimCBStringToPrelimMap.get( (String ) _prelimLCB.getSelectedItem() );
            raxCrest.setPrelim( prelimString );
            
            _dataMgr.saveRaxCrestToDatabase( raxCrest );
            
            saved = true;
            
            updateJTable();
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "Must select a valid date", "Save Crest" );
        }

        return saved;
    }
    
    private void deleteCrest()
    {
        RaxCrest raxCrest = null;
        List selectedRowsData = _CETableManager.getSelectedRowsData();
        
        if ( ! selectedRowsData.isEmpty() )
        {
            ArcBaseCrestJTableRowData rowData = (ArcBaseCrestJTableRowData) selectedRowsData.get( 0 );
            raxCrest = (RaxCrest) _raxCrestRowDataToRaxCrestMap.get( rowData );
            if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + raxCrest.keyString(), "Delete Crest" ) )
            {
                _dataMgr.deleteRaxCrestFromDatabase( raxCrest );
                clearDataInputPanel();
                updateJTable();
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select a crest", "Delete Crest" );
        }
    }
    
    private String getYNStringFromCB( JCheckBox checkBox )
    {
        String ynString;
        
        if ( checkBox.isSelected() )
        {
            ynString = "Y";
        }
        else
        {
            ynString = "N";
        }
        
        return ynString;
    }
    
    private void launchDateWindow()
    {
        GregorianCalendar calendar = new GregorianCalendar();
        long dayInMillis = 60*60*24*1000;
        long dateLong = System.currentTimeMillis();

        if ( ! _dateLTF.getTextFieldText().equalsIgnoreCase( "" ) )
        {
            dateLong = _converter.getLongDateValue( _dateLTF.getTextFieldText() ) + dayInMillis;
        }

        calendar.setTimeInMillis( dateLong );

        _hDateChooser.setDate( calendar );
        _hDateChooser.setModal( true );
        _hDateChooser.setVisible( true );
    }
    
    public void dateChosen( JDialog jDialog )
    {
        Calendar calendar = _hDateChooser.getDate();
        String dateString = null;
        
        if ( _hDateChooser.isClearDate() )
        {
            dateString = "";
        }
        else
        {
            dateString = DateManager.getDateStringFromCalendar( calendar );
        }
        
        _dateLTF.setTextField( dateString );
    }
    
    private boolean isQualityTextFieldLengthToLong( String qcString, KeyEvent e )
    {
        boolean tooLong = false;
        
        if ( qcString.length() > 1 )
        {
            tooLong = true;
        }
        
        return tooLong;
    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();

        _closeButton.addActionListener( windowCloser );
        addWindowListener( windowCloser );
        _CETableManager.addTableListener( new CETableListener() );
        _newButton.addActionListener( new NewButtonListener() );
        _saveButton.addActionListener( new SaveButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
        _dateLTF.addTextFieldMouseListener( new DateFieldMouseListener() );
        _stgQualLTF.addTextFieldKeyListener( new StgQualKeyListener() );
        _flowQualLTF.addTextFieldKeyListener( new FlowQualKeyListener() );
    }
    
    private class StgQualKeyListener implements KeyListener
    {
        String __oldString = null;

        public void keyPressed( KeyEvent arg0 )
        {
            if ( _stgQualLTF.getTextFieldText().length() > 1 )
            {
                __oldString = _stgQualLTF.getTextFieldText().substring( 0, 1 );
            }
            else
            {
                __oldString = _stgQualLTF.getTextFieldText();
            }
        }

        public void keyReleased( KeyEvent e )
        {
            if ( isQualityTextFieldLengthToLong( _flowQualLTF.getTextFieldText(), e ) )
            {
                _stgQualLTF.setTextField( __oldString );
            }
        }

        public void keyTyped( KeyEvent arg0 ){}
    }

    private class FlowQualKeyListener implements KeyListener
    {
        String __oldString = null;
        
        public void keyPressed( KeyEvent arg0 )
        {
            if ( _flowQualLTF.getTextFieldText().length() > 1 )
            {
                __oldString = _flowQualLTF.getTextFieldText().substring( 0, 1 );
            }
            else
            {
                __oldString = _flowQualLTF.getTextFieldText();
            }
        }

        public void keyReleased( KeyEvent e )
        {
            if ( isQualityTextFieldLengthToLong( _flowQualLTF.getTextFieldText(), e ) )
            {
                _flowQualLTF.setTextField( __oldString );
            }
        }

        public void keyTyped( KeyEvent arg0 ){}
    }
    private class DateFieldMouseListener implements MouseListener
    {
        public void mouseClicked( MouseEvent e )
        {
        }

        public void mouseEntered( MouseEvent e )
        {
        }

        public void mouseExited( MouseEvent e )
        {
        }

        public void mousePressed( MouseEvent e )
        {
            launchDateWindow();
        }

        public void mouseReleased( MouseEvent e )
        {
        }
    }
    
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveCrest();
        }
    }
    
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteCrest();
        }
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveCrest() )   // If the save is successful, close the window
            {
                closeWindow();
            }
        }
    }
    
    private class CETableListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                RaxCrest raxCrest = null;

                ArcBaseCrestJTableRowData rowData = (ArcBaseCrestJTableRowData) _CETableManager.getSelectedRowsData().get( 0 );
                raxCrest = (RaxCrest) _raxCrestRowDataToRaxCrestMap.get( rowData );
                populateDataInputPanel( raxCrest );
            }
        }
    }

    private class NewButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            clearDataInputPanel();
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
        
        String title = "ZZV";
        String name = dataMgr.getRaxLocation( "ZZV" ).getName();
        
        if ( name != null )
        {
            title += " - " + name;
        }
        

        CrestEditor crestEditorGUI = new CrestEditor( frame, dataMgr, "ZZV", title );
        crestEditorGUI.displayGUI();
    }

}
