package ohd.hseb.raxbase.gui;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
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
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.raxbase.RaxBaseDataMgr;
import ohd.hseb.raxbase.model.RaxAdjustFactor;
import ohd.hseb.raxbase.model.ShefDuration;
import ohd.hseb.raxbase.model.ShefExtremum;
import ohd.hseb.raxbase.model.ShefPE;
import ohd.hseb.raxbase.model.ShefTS;
import ohd.hseb.raxbase.table.ArcBaseAdjustFactorJTableRowData;
import ohd.hseb.raxbase.util.DateManager;
import ohd.hseb.raxbase.util.HDateSuperChooser;
import ohd.hseb.rfc.util.graphics.HDateChooserOwner;
import ohd.hseb.util.StringDataConverter;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledTextField;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableColumnDescriptor;
import ohd.hseb.util.gui.jtable.JTableManager;

public class AdjustFactorEditor extends JDialog implements HDateChooserOwner
{
    private Container _dialogContentPane = getContentPane();
    
    private RaxBaseDataMgr _dataMgr = null;

    private HDateSuperChooser _hDateChooser = new HDateSuperChooser( false );

    private StringDataConverter _converter = new StringDataConverter();
    
    private Map _peJLStringToPeMap = new HashMap();
    private Map _durCBStringToDurMap = new HashMap();
    private Map _extremumCBStringToExtremumMap = new HashMap();
    private Map _tsCBStringToTSMap = new HashMap();

//  JTable variables
    private JTableManager _AFETableManager = null;
    private List _AFEColumnDescriptorList = null;
    private List _adjustFactorRowDataList = null;
    private List _filteredRowDataList = null;
    private JScrollPane _tableScrollPane = null;
    private JPanel _tableScrollPanePanel = new JPanel( new GridBagLayout() );

    private JPanel _selectedItemPanel = new JPanel( new GridBagLayout() );
    private JComboBox _durationComboBox = null;
    private JComboBox _tsComboBox = null;
    private JComboBox _extremumComboBox = null;
    private JLabel _peLabel = new JLabel( "PE (Physical Element):" );
    private JList _peJList = new JList();
    private JScrollPane _peScrollPane = new JScrollPane();
    private DefaultListModel _peListModel = new DefaultListModel();
    private List _peJLStringList= null;

    private JLabel _descriptorLabel = new JLabel( "                      Adjusted Value = ( ( ( Raw Value / Divisor ) + Base ) * Multiplier ) + Adder" );
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _updateInsertButton = new JButton( "Update/Insert" );
    private JButton _deleteButton = new JButton( "Delete" );
    
    private LabeledTextField _lidLTF = new LabeledTextField( "Location:", "", "Location", 8 );
    private LabeledTextField _beginDateLTF = new LabeledTextField( "Begin Date:", "", "Begin Date", 12 );
    private LabeledTextField _divisorLTF = new LabeledTextField( "Divisor:", "", "Divisor factor in formula to change from raw value to adjusted value", 10 );
    private LabeledTextField _baseLTF = new LabeledTextField( "Base:", "", "Base factor in formula to change from raw value to adjusted value", 10 );
    private LabeledTextField _multiplierLTF = new LabeledTextField( "Multiplier:", "", "Multiplier in formula to change from raw value to adjusted value", 10 );
    private LabeledTextField _adderLTF = new LabeledTextField( "Adder:", "", "Additive factor in formula to change raw value to adjusted value for posting", 10 );
    
    private Map _raxAdjustFactorRowDataToRaxAdjustFactorMap = new HashMap();
    
    public AdjustFactorEditor( JFrame frame, RaxBaseDataMgr dataMgr )
    {
        super( frame, "Adjust Factor Editor", true );
        _dataMgr = dataMgr;
    }
    
    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 760, 825 ) );
        initGUI();
    }

    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        initItemSelectionPanel();
        initSelectedItemPanel();
        initButtonPanel();
        initFrameComponents();
        addListeners();
        _raxAdjustFactorRowDataToRaxAdjustFactorMap = _dataMgr.getRaxAdjustFactorRowDataToRaxAdjustFactorMap();
        pack();
        setVisible( true );
    }

    private void initButtonPanel()
    {
//                                                                            X,  Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _buttonPanel, _closeButton,        0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _updateInsertButton, 1,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _deleteButton,       2,  0,    1,     1, GridBagConstraints.NONE );
    }

    
    private void initSelectedItemPanel()
    {
        _selectedItemPanel.setBorder( BorderFactory.createTitledBorder( "Selected Item" ) );

        setupPEJList();
        setupDurationComboBox();
        setupTypeSourceComboBox();
        setupExtremumComboBox();
        
        _peJList.setSelectionMode( ListSelectionModel.SINGLE_SELECTION );
        _peScrollPane = new JScrollPane( _peJList );
        _peScrollPane.setPreferredSize( new Dimension( 250, 100 ) );
        JLabel durationLabel = new JLabel( "Duration:" );
        JLabel tsLabel = new JLabel( "TypeSource:" );
        JLabel extremumLabel = new JLabel( "Extremum:" );
        GridBagConstraints gbLabelConstraints = new GridBagConstraints();
        GridBagConstraints gbTFConstraints = new GridBagConstraints();

        gbLabelConstraints.anchor = GridBagConstraints.EAST;
        gbLabelConstraints.weightx = 1;
        gbLabelConstraints.weighty = 1;
        
        gbTFConstraints.anchor = GridBagConstraints.WEST;
        gbTFConstraints.weightx = 1;
        gbTFConstraints.weighty = 1;
        _beginDateLTF.setEditTextField( false );
        
//                                                                                                     X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _selectedItemPanel, _lidLTF,                                0,   0,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _selectedItemPanel, _beginDateLTF,                          0,   1,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, durationLabel, gbLabelConstraints,      0,   2,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _durationComboBox, gbTFConstraints,     1,   2,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, tsLabel, gbLabelConstraints,            0,   3,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _tsComboBox, gbTFConstraints,           1,   3,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, extremumLabel, gbLabelConstraints,      0,   4,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _extremumComboBox, gbTFConstraints,     1,   4,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _selectedItemPanel, _peLabel, gbTFConstraints,              2,   0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _selectedItemPanel, _peScrollPane,                          2,   1,    2,     5, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _selectedItemPanel, _divisorLTF,                            0,   6,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _selectedItemPanel, _baseLTF,                               2,   6,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _selectedItemPanel, _multiplierLTF,                         0,   7,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _selectedItemPanel, _adderLTF,                              2,   7,    2,     1, 1, 1, GridBagConstraints.NONE );
        _hDateChooser.addOwner( this );
    }
    
    private void initItemSelectionPanel()
    {
        initJTable();

        _tableScrollPanePanel.setBorder( BorderFactory.createTitledBorder( "Summary By Location of Data Adjustment Factors" ) );
    }
    
    private void initFrameComponents()
    {
//                                                                                         X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _tableScrollPanePanel,      0,   0,    4,     20, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _descriptorLabel,           0,  21,    4,     1, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _selectedItemPanel,         0,  22,    4,     1, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _buttonPanel,               0,  23,    4,     1, 1, 1, GridBagConstraints.BOTH );

    }
    
    private void updateJTable()
    {
        _adjustFactorRowDataList = _dataMgr.getAdjustFactorRowDataList();
        _AFETableManager.setChangedAllRowDataList( _adjustFactorRowDataList );
        _AFETableManager.refreshDisplay();
    }
    
    private void initJTable() 
    {
        _adjustFactorRowDataList = _dataMgr.getAdjustFactorRowDataList();
        setAFEColumnDescriptorList();
        _AFETableManager = new ComplexJTableManager( _AFEColumnDescriptorList, _adjustFactorRowDataList, ListSelectionModel.SINGLE_SELECTION );
        String columnsSelected[] = _AFETableManager.getColumnNamesThatAreCurrentlyDisplayed();
        _AFETableManager.setDisplayableColumns( columnsSelected, true, true );
        _tableScrollPane = _AFETableManager.getJScrollPane();
        _tableScrollPane.setPreferredSize( new Dimension( 600, 300 ) );
//                                                                                     X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _tableScrollPanePanel, _tableScrollPane,    0,   1,    1,     1, 1, 1, GridBagConstraints.BOTH );
    }
    
    private void setAFEColumnDescriptorList()
    {
        _AFEColumnDescriptorList = new ArrayList();

        _AFEColumnDescriptorList.add(new JTableColumnDescriptor( "Location", true, 75, "center" ) );
        _AFEColumnDescriptorList.add(new JTableColumnDescriptor( "PE", true, 30, "center" ) );
        _AFEColumnDescriptorList.add(new JTableColumnDescriptor( "Dur", true, 75, "center" ) );
        _AFEColumnDescriptorList.add(new JTableColumnDescriptor( "TS", true, 75, "center" ) );
        _AFEColumnDescriptorList.add(new JTableColumnDescriptor( "Extremum", true, 75, "center" ) );
        _AFEColumnDescriptorList.add(new JTableColumnDescriptor( "Begin Date", true, 100, "center" ) );
        _AFEColumnDescriptorList.add(new JTableColumnDescriptor( "Divisor", true, 75, "center" ) );
        _AFEColumnDescriptorList.add(new JTableColumnDescriptor( "Base", true, 75, "center" ) );
        _AFEColumnDescriptorList.add(new JTableColumnDescriptor( "Multiplier", true, 75, "center" ) );
        _AFEColumnDescriptorList.add(new JTableColumnDescriptor( "Adder", true, 75, "center" ) );
    }

    private void setupPEJList()
    {
        List shefPeList = _dataMgr.getShefPeList();
        String peString = null;

        _peJLStringList = new ArrayList();
        for ( int i = 0; i < shefPeList.size(); i++ )
        {
            ShefPE shefPE = (ShefPE) shefPeList.get( i );
            _peJLStringList.add( getShefPEJListString( shefPE ) );
            _peJLStringToPeMap.put( getShefPEJListString( shefPE ), shefPE );
        }
        
        for ( int i = 0; i < _peJLStringList.size(); i++ )
        {
            peString = (String) _peJLStringList.get( i );
            _peListModel.addElement( peString );
        }
        
        _peJList.setModel( _peListModel );
    }

    private String getShefPEJListString( ShefPE shefPE )
    {
        String shefPEString = shefPE.getPe() + " " + shefPE.getName();
        
        return shefPEString;
    }
    
    private void setupDurationComboBox()
    {
        List shefDurList = _dataMgr.getShefDurationList();
        List shefDurationStringList = new ArrayList();

        for ( int i = 0; i < shefDurList.size(); i++ )
        {
            ShefDuration shefDur = (ShefDuration) shefDurList.get( i );
            shefDurationStringList.add( getShefDurationComboBoxString( shefDur ) );
            _durCBStringToDurMap.put( getShefDurationComboBoxString( shefDur ), shefDur );
        }
        
        _durationComboBox = new JComboBox( shefDurationStringList.toArray() );
    }
    
    private String getShefDurationComboBoxString( ShefDuration shefDuration )
    {
        String shefDurationString = shefDuration.getName() + " ( " + shefDuration.getIduration() + "/" + shefDuration.getDuration() + " )";

        return shefDurationString;
    }

    private void setupTypeSourceComboBox()
    {
        List shefTSList = _dataMgr.getShefTSList();
        List shefTSStringList = new ArrayList();
        
        for ( int i = 0; i < shefTSList.size(); i++ )
        {
            ShefTS shefTS = (ShefTS) shefTSList.get( i );
            shefTSStringList.add( getShefTSComboBoxString( shefTS ) );
            _tsCBStringToTSMap.put( getShefTSComboBoxString( shefTS ), shefTS );
        }
        
        _tsComboBox = new JComboBox( shefTSStringList.toArray() );
    }
    
    private String getShefTSComboBoxString( ShefTS shefTS )
    {
        String shefTSString = shefTS.getName() + " (" + shefTS.getTs() + ") ";
        
        return shefTSString;
    }
    
    private void setupExtremumComboBox()
    {
        List shefExList = _dataMgr.getShefExtremumList();
        List shefExStringList = new ArrayList();
        
        for ( int i = 0; i < shefExList.size(); i++ )
        {
            ShefExtremum shefEX = (ShefExtremum) shefExList.get( i );
            shefExStringList.add( getShefExtremumComboBoxString( shefEX ) );
            _extremumCBStringToExtremumMap.put( getShefExtremumComboBoxString( shefEX ), shefEX );
        }
        
        _extremumComboBox = new JComboBox( shefExStringList.toArray() );
    }
    
    private String getShefExtremumComboBoxString( ShefExtremum shefEX )
    {
        String shefExString = shefEX.getName() + " (" + shefEX.getExtremum() + ") ";
        
        return shefExString;
    }

    private void updateInsertRaxAdjustFactor()
    {
        RaxAdjustFactor raxAdjustFactor = new RaxAdjustFactor();
        ShefPE pe = (ShefPE) _peJLStringToPeMap.get( (String) _peJList.getSelectedValue() );
        ShefTS ts = (ShefTS) _tsCBStringToTSMap.get( (String) _tsComboBox.getSelectedItem() );
        ShefExtremum extremum = (ShefExtremum) _extremumCBStringToExtremumMap.get( (String) _extremumComboBox.getSelectedItem() );
        ShefDuration dur = (ShefDuration) _durCBStringToDurMap.get( (String) _durationComboBox.getSelectedItem() );
        
        raxAdjustFactor.setLid( _lidLTF.getTextFieldText() );
        raxAdjustFactor.setPe( pe.getPe() );
        raxAdjustFactor.setTs( ts.getTs() );
        raxAdjustFactor.setExtremum( extremum.getExtremum() );
        raxAdjustFactor.setDur( dur.getDuration() );
        raxAdjustFactor.setIdur( dur.getIduration() );
        raxAdjustFactor.setDivisor( _converter.getDoubleValue( _divisorLTF.getTextFieldText() ) );
        raxAdjustFactor.setBase( _converter.getDoubleValue( _baseLTF.getTextFieldText() ) );
        raxAdjustFactor.setMultiplier( _converter.getDoubleValue( _multiplierLTF.getTextFieldText() ) );
        raxAdjustFactor.setAdder( _converter.getDoubleValue( _adderLTF.getTextFieldText() ) );
        raxAdjustFactor.setBeginDate( _converter.getLongDateValue( _beginDateLTF.getTextFieldText() ) );
        
        _dataMgr.saveRaxAdjustFactor( raxAdjustFactor );
        
        updateJTable();
    }
    
    private void clearSelectedItemPanel()
    {
        _lidLTF.setTextField( "" );
        _divisorLTF.setTextField( "" );
        _baseLTF.setTextField( "" );
        _multiplierLTF.setTextField( "" );
        _adderLTF.setTextField( "" );
        _beginDateLTF.setTextField( "" );
        _peJList.setSelectedIndex( 0 );
        _durationComboBox.setSelectedIndex( 0 );
        _tsComboBox.setSelectedIndex( 0 );
        _extremumComboBox.setSelectedIndex( 0 );
    }
    
    private void populateDataInputPanel( RaxAdjustFactor adjustFactor )
    {
        _lidLTF.setTextField( adjustFactor.getLid() );
        
        _divisorLTF.setTextField( adjustFactor.getDivisor() );
        _baseLTF.setTextField( adjustFactor.getBase() );
        _multiplierLTF.setTextField( adjustFactor.getMultiplier() );
        _adderLTF.setTextField( adjustFactor.getAdder() );
        _beginDateLTF.setTextFieldDate( adjustFactor.getBeginDate() );
        ShefPE shefPE = (ShefPE) _dataMgr.getShefPEMap().get( adjustFactor.getPe() );
        if ( shefPE != null )
        {
            _peJList.setSelectedValue( getShefPEJListString( shefPE ), true );
        }
        else
        {
            _peJList.clearSelection();
        }

        
        ShefDuration shefDur = (ShefDuration) _dataMgr.getShefDurationMap().get( adjustFactor.getDur() );
        if ( shefDur != null )
        {
            _durationComboBox.setSelectedItem( getShefDurationComboBoxString( shefDur ) );
        }
        else
        {
            _durationComboBox.setSelectedItem( "" );
        }
        
        ShefTS shefTS = (ShefTS) _dataMgr.getShefTSMap().get( adjustFactor.getTs() );
        if ( shefTS != null )
        {
            _tsComboBox.setSelectedItem( getShefTSComboBoxString( shefTS ) );
        }
        else
        {
            _tsComboBox.setSelectedItem( "" );
        }
        
        ShefExtremum shefEx = (ShefExtremum) _dataMgr.getShefExtremumMap().get( adjustFactor.getExtremum() );
        if ( shefEx != null )
        {
            _extremumComboBox.setSelectedItem( getShefExtremumComboBoxString( shefEx ) );
        }
        else
        {
            _extremumComboBox.setSelectedItem( "" );
        }
        
    }
    
    private void launchDateWindow()
    {
        GregorianCalendar calendar = new GregorianCalendar();
        long dayInMillis = 60*60*24*1000;
        long dateLong = 0;

        if ( _beginDateLTF.getTextFieldText().equalsIgnoreCase( "" ) )
        {
            dateLong = System.currentTimeMillis();
        }
        else
        {
            dateLong = DbTimeHelper.getLongTimeFromDateString( _beginDateLTF.getTextFieldText() ) + dayInMillis;
        }
        
        calendar.setTimeInMillis( dateLong );

        _hDateChooser.setDate( calendar );
        _hDateChooser.setModal( true );
        _hDateChooser.setVisible( true );
    }
    
    private void deleteRaxAdjustFactor()
    {
        RaxAdjustFactor raxAdjustFactor = null;
        List selectedRowsData = _AFETableManager.getSelectedRowsData();
        
        if ( ! selectedRowsData.isEmpty() )
        {
            ArcBaseAdjustFactorJTableRowData rowData = (ArcBaseAdjustFactorJTableRowData) selectedRowsData.get( 0 );
            raxAdjustFactor = (RaxAdjustFactor) _raxAdjustFactorRowDataToRaxAdjustFactorMap.get( rowData );
            if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + raxAdjustFactor.keyString(), "Delete Adjust Factor" ) )
            {
                _dataMgr.deleteRaxAdjustFactorFromDatabase( raxAdjustFactor );
                clearSelectedItemPanel();
                updateJTable();
            }
        }
        else
        {
            DialogHelper.displayErrorDialog( this, "You must select an Adjust Factor entry", "Delete Adjust Factor" );
        }
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
            
            _beginDateLTF.setTextField( dateString );
    }
    
    private void addListeners()
    {
        _AFETableManager.addTableListener( new AFETableListener() );
        WindowCloserListener windowCloser = new WindowCloserListener();
        _beginDateLTF.addTextFieldMouseListener( new DateFieldMouseListener() );

        _updateInsertButton.addActionListener( new UpdateInsertButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
        _closeButton.addActionListener( windowCloser );

        addWindowListener( windowCloser );
    }
    
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteRaxAdjustFactor();
        }
    }
    
    private class UpdateInsertButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            updateInsertRaxAdjustFactor();
        }
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

    private class AFETableListener implements ListSelectionListener
    {
        public void valueChanged( ListSelectionEvent e )
        {
            if ( e.getValueIsAdjusting() )
            {
                List rowDataList = null;
                
                RaxAdjustFactor raxAdjustFactor = null;
                
                ArcBaseAdjustFactorJTableRowData rowData = (ArcBaseAdjustFactorJTableRowData) _AFETableManager.getSelectedRowsData().get( 0 );
                    
                raxAdjustFactor = (RaxAdjustFactor) _raxAdjustFactorRowDataToRaxAdjustFactorMap.get( rowData );
                    
                populateDataInputPanel( raxAdjustFactor );
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
        RaxBaseDataMgr dataMgr = new RaxBaseDataMgr( "jdbc:postgresql://ax2:5432/adb_ob82krf?user=pguser", "jdbc:postgresql://lx5:5432/hd_ob83raxtest?user=pguser adb_ob83raxtest", "/fs/home/gsood/" );
        AdjustFactorEditor adjustFactorEditorGUI = new AdjustFactorEditor( frame, dataMgr );
        adjustFactorEditorGUI.displayGUI();
    }
}
