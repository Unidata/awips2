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
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;

import ohd.hseb.db.DbTable;
import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.raxbase.RaxBaseDataMgr;
import ohd.hseb.raxbase.model.RaxReservoir;
import ohd.hseb.raxbase.util.DateManager;
import ohd.hseb.raxbase.util.HDateSuperChooser;
import ohd.hseb.rfc.util.graphics.HDateChooserOwner;
import ohd.hseb.util.StringDataConverter;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledComboBox;
import ohd.hseb.util.gui.LabeledTextField;

public class ReservoirEditor extends JDialog implements HDateChooserOwner
{
    private Container _dialogContentPane = getContentPane();
    
    private List _raxReservoirList = null;
    
    private LabeledComboBox _selectionLCB = new LabeledComboBox();
    private Map _selectionStringToRaxReservoirMap = new HashMap();
    private String _lid = null;
    private RaxBaseDataMgr _dataMgr = null;
    
    private HDateSuperChooser _hDateChooser = new HDateSuperChooser( false );
    private int _clickedDateField = -9999;
    
    private JPanel _informationPanel = new JPanel( new GridBagLayout() );
    
    private long _beginDate = -9999;
    private StringDataConverter _converter = new StringDataConverter();
    
    private JPanel _metaDataPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _nameLTF = new LabeledTextField( "Name:", "", "Name", 22 );
    private LabeledTextField _beginDateLTF = new LabeledTextField( "Begin Date:", "", "Begin Date", 10 );
    private LabeledTextField _endDateLTF = new LabeledTextField( "End Date:", "", "End Date", 10 );
    private LabeledTextField _impoundDateLTF = new LabeledTextField( "Impound Date:", "", "The date that water was initially impounded by this reservoir", 12 );
    private LabeledTextField _gatesLTF = new LabeledTextField( "Gates:", "", "Number of gates", 10 );
    private LabeledTextField _typeLTF = new LabeledTextField( "Type:", "", "Type of dam construction (eg, earthen, concrete, levee, gravity, arch)", 12 );
    private LabeledTextField _ownerLTF = new LabeledTextField( "Owner:", "", "Owner of the dam", 12 );
    
    private JPanel _elevationPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _maxSurchargeLTF = new LabeledTextField( "Max Surcharge:", "", "Maximum elevation (ft MSL) for water stored in a reservoir before release is required", 10 );
    private LabeledTextField _topLTF = new LabeledTextField( "Top:", "", "Elevation (ft MSL) of the top of dam", 10 );
    private LabeledTextField _sillLTF = new LabeledTextField( "Sill:", "", "Elevation of the sill structure of the dam that holds a reservoir (ft MSL)", 10 );
    private LabeledTextField _reservoirLTF = new LabeledTextField( "Reservoir:", "", "Elevation (ft MSL)", 10 );

    private JPanel _poolsPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _floodLTF = new LabeledTextField( "Flood:", "", "Flood pool elevation (ft MSL)", 10 );
    private LabeledTextField _spillwayLTF = new LabeledTextField( "Spillway:", "", "Elevation of the dam spillway structure", 10 );
    private LabeledTextField _conservationLTF = new LabeledTextField( "Conservation:", "", "The minimum water level of a reservoir to be conserved in order to serve the needs of downstream water users during non-rainy times of the year (ft MSL)", 10 );
    private LabeledTextField _deadLTF = new LabeledTextField( "Dead:", "", "Lowest water level allowed for a reservoir through normal operations (ft MSL)", 10 );

    private JPanel _usesPanel = new JPanel( new GridBagLayout() );
    private JCheckBox _floodControlCB = new JCheckBox( "Flood Control" );
    private JCheckBox _hydroCB = new JCheckBox( "Hydroelectric" );
    private JCheckBox _lowFlowCB = new JCheckBox( "Low Flow Augmentation" );
    private JCheckBox _navigationCB = new JCheckBox( "Navigation" );
    private JCheckBox _recreationCB = new JCheckBox( "Recreation" );
    private JCheckBox _waterSupplyCB = new JCheckBox( "Water Supply" );
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _deleteButton = new JButton( "Delete" );
    
    public ReservoirEditor( JFrame frame, RaxBaseDataMgr dataMgr, String lid, String title )
    {
        super( frame, "Reservoir Editor - " + title, true );
        _dataMgr = dataMgr;
        _lid = lid;
    }

    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 600, 475 ) );
        initGUI();
    }
    
    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        updateRaxReservoirList();
        updateSelectionComboBox();
        initInformationPanel();
        initElevationPanel();
        initPoolsPanel();
        initButtonPanel();
        initFrameComponents();
        addListeners();
        initPopulateInformationPanel();
        
        pack();
        setVisible( true );
    }
    
    private void updateRaxReservoirList()
    {
        _raxReservoirList = _dataMgr.getRaxReservoirListFromRaxLocation( _lid );
    }
    
    private void initPopulateInformationPanel()
    {
        String selectedItem = _selectionLCB.getSelectedCBItem();

        RaxReservoir raxReservoir = (RaxReservoir) _selectionStringToRaxReservoirMap.get( selectedItem );

        if ( raxReservoir != null )
        {
            _beginDate = raxReservoir.getBeginDate();
        }
        else
        {
            _beginDate = 0;
        }
        if ( raxReservoir != null )
        {
            populateDataFields( raxReservoir );
        }
    }

    private void initButtonPanel()
    {
        _buttonPanel.setBorder( BorderFactory.createTitledBorder( "Database Controls" ) );
        
//                                                                           X,  Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _buttonPanel, _saveButton,        0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _buttonPanel, _closeButton,       1,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _buttonPanel, _deleteButton,      2,  0,    1,     1, GridBagConstraints.NONE );

    }
    
    
    private void updateSelectionComboBox()
    {
        RaxReservoir raxReservoir = null;
        List selectionComboBoxStringList = new ArrayList();
        String dateString = null;
        
        _selectionStringToRaxReservoirMap.clear();
        _selectionLCB.getComboBox().removeAllItems();
        
        for ( int i = 0; i < _raxReservoirList.size(); i++ )
        {
            raxReservoir = (RaxReservoir) _raxReservoirList.get( i );
            dateString = DbTimeHelper.getDateStringFromLongTime( raxReservoir.getBeginDate() );
            selectionComboBoxStringList.add( dateString );
            _selectionStringToRaxReservoirMap.put( dateString, raxReservoir );
        }
        
        if ( selectionComboBoxStringList.isEmpty() )
        {
            selectionComboBoxStringList.add( "No entries found" );
        }
        _selectionLCB.setComboBoxFromStringArray( selectionComboBoxStringList.toArray() );
        _selectionLCB.setLabel( "Service Begin Date:" );
        _selectionLCB.setLabelPreferredSize( new Dimension( 125, 15 ) );
        _selectionLCB.setComboBoxPreferredSize( new Dimension( 125, 15 ) );
    }
    
    private void initInformationPanel()
    {
        _informationPanel.setBorder( BorderFactory.createTitledBorder( "Information for " + _lid ) );
        
        initUsesPanel();
        _beginDateLTF.setEditTextField( false );
        _endDateLTF.setEditTextField( false );
        _impoundDateLTF.setEditTextField( false );
//                                                                             X,  Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _metaDataPanel, _nameLTF,           0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _metaDataPanel, _beginDateLTF,      0,  1,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _metaDataPanel, _endDateLTF,        0,  2,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _metaDataPanel, _impoundDateLTF,    0,  3,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _metaDataPanel, _gatesLTF,          0,  4,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _metaDataPanel, _typeLTF,           0,  5,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _metaDataPanel, _ownerLTF,          0,  6,    1,     1, GridBagConstraints.NONE );

        ComponentHelper.addFrameComponent( _informationPanel, _metaDataPanel,  0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _informationPanel, _usesPanel,      1,  0,    1,     1, GridBagConstraints.NONE );

        _hDateChooser.addOwner( this );
    }
    
    private void initElevationPanel()
    {
        _elevationPanel.setBorder( BorderFactory.createTitledBorder( "Elevations" ) );

//                                                                            X,  Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _elevationPanel, _maxSurchargeLTF, 0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _elevationPanel, _topLTF,          0,  1,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _elevationPanel, _sillLTF,         0,  2,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _elevationPanel, _reservoirLTF,    0,  3,    1,     1, GridBagConstraints.NONE );
    }

    private void initPoolsPanel()
    {
        _poolsPanel.setBorder( BorderFactory.createTitledBorder( "Pools" ) );

//                                                                         X,  Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _poolsPanel, _floodLTF,         0,  0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _poolsPanel, _spillwayLTF,      0,  1,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _poolsPanel, _conservationLTF,  0,  2,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _poolsPanel, _deadLTF,          0,  3,    1,     1, GridBagConstraints.NONE );
    }

    private void initUsesPanel()
    {
        _usesPanel.setBorder( BorderFactory.createTitledBorder( "Uses" ) );

//                                                                       X,  Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _usesPanel, _floodControlCB,  0,  0,    1,     1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _usesPanel, _hydroCB,         0,  1,    1,     1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _usesPanel, _lowFlowCB,       0,  2,    1,     1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _usesPanel, _navigationCB,    0,  3,    1,     1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _usesPanel, _recreationCB,    0,  4,    1,     1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _usesPanel, _waterSupplyCB,   0,  5,    1,     1, GridBagConstraints.BOTH );
        
    }
    private void initFrameComponents()
    {
//                                                                                 X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _selectionLCB,      0,   0,    2,     1, 1, 1, GridBagConstraints.NONE );
        
        ComponentHelper.addFrameComponent( _dialogContentPane, _informationPanel,  0,   1,    2,     1, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _elevationPanel,    0,   2,    1,     1, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _poolsPanel,        1,   2,    1,     1, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _buttonPanel,       0,   3,    2,     1, 1, 1, GridBagConstraints.BOTH );
    }
    
    private void populateDataFields( RaxReservoir raxReservoir )
    {
        _nameLTF.setTextField( raxReservoir.getName() );
        setDateField( _beginDateLTF, raxReservoir.getBeginDate() );
        setDateField( _endDateLTF, raxReservoir.getEndDate() );
        setDateField( _impoundDateLTF, raxReservoir.getImpounded() );
        _gatesLTF.setTextField( raxReservoir.getGates() );
        _typeLTF.setTextField( raxReservoir.getType() );
        _ownerLTF.setTextField( raxReservoir.getOwner() );
        _maxSurchargeLTF.setTextField( raxReservoir.getSurchg() );
        _topLTF.setTextField( raxReservoir.getTop() );
        _sillLTF.setTextField( raxReservoir.getSill() );
        _reservoirLTF.setTextField( raxReservoir.getElev() );
        _floodLTF.setTextField( raxReservoir.getFloodPool() );
        _spillwayLTF.setTextField( raxReservoir.getSpillWay() );
        _conservationLTF.setTextField( raxReservoir.getConserPool() );
        _deadLTF.setTextField( raxReservoir.getDeadPool() );
        
        populateCheckBoxes( raxReservoir );
    }
    
    public void setDateField( LabeledTextField ltf, long dateLongValue )
    {
        if ( dateLongValue == DbTable.getNullLong() )
        {
            ltf.setTextField( "" );
        }
        else
        {
            ltf.setTextField( DbTimeHelper.getDateStringFromLongTime( dateLongValue ) );
        }
    }
    
    
    private void populateCheckBoxes( RaxReservoir raxReservoir )
    {
        String uses = raxReservoir.getUses();
        if ( uses != null )
        {
            if ( uses.indexOf( "F" ) != -1 )
            {
                _floodControlCB.setSelected( true );
            }
            else
            {
                _floodControlCB.setSelected( false );
            }

            if ( uses.indexOf( "H" ) != -1 )
            {
                _hydroCB.setSelected( true );
            }
            else
            {
                _hydroCB.setSelected( false );
            }

            if ( uses.indexOf( "L" ) != -1 )
            {
                _lowFlowCB.setSelected( true );
            }
            else
            {
                _lowFlowCB.setSelected( false );
            }

            if ( uses.indexOf( "N" ) != -1 )
            {
                _navigationCB.setSelected( true );
            }
            else
            {
                _navigationCB.setSelected( false );
            }

            if ( uses.indexOf( "R" ) != -1 )
            {
                _recreationCB.setSelected( true );
            }
            else
            {
                _recreationCB.setSelected( false );
            }

            if ( uses.indexOf( "W" ) != -1 )
            {
                _waterSupplyCB.setSelected( true );
            }
            else
            {
                _waterSupplyCB.setSelected( false );
            }
        }
    }
    
    private void launchDateWindow( int clickedDateField )
    {
        _clickedDateField = clickedDateField;
        
        GregorianCalendar calendar = new GregorianCalendar();
        long dayInMillis = 60*60*24*1000;
        long dateLong = 0;
        String dateString = null;
        
        switch ( _clickedDateField )
        {
            case 0: dateString = _beginDateLTF.getTextFieldText(); break;
            case 1: dateString = _endDateLTF.getTextFieldText(); break;
            case 2: dateString = _impoundDateLTF.getTextFieldText(); break;
        }
        
        if ( ! dateString.equalsIgnoreCase( ""  ) )
        {
            dateLong = DbTimeHelper.getLongTimeFromDateString( dateString ) + dayInMillis;
        }
        else
        {
            dateLong = System.currentTimeMillis();
        }
        
        calendar.setTimeInMillis( dateLong );
        _hDateChooser.setDate( calendar );
        _hDateChooser.setModal( true );
        _hDateChooser.setVisible( true );

    }
    
    public void dateChosen( JDialog jDialog )
    {
        if ( _clickedDateField != -9999 )
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
            
            switch ( _clickedDateField )
            {
                case 0: _beginDateLTF.setTextField( dateString ); break;
                case 1: _endDateLTF.setTextField( dateString ); break;
                case 2: _impoundDateLTF.setTextField( dateString ); break;
            }
            _clickedDateField = -9999;
        }
    }

    private RaxReservoir getRaxReservoirToSave()
    {
        RaxReservoir reservoir = new RaxReservoir();
        StringBuffer usesStringBuffer = new StringBuffer();
        
        reservoir.setLid( _lid );
        reservoir.setBeginDate( _converter.getLongDateValue( _beginDateLTF.getTextFieldText() ) );
        reservoir.setEndDate( _converter.getLongDateValue( _endDateLTF.getTextFieldText() ) );
        reservoir.setName( _nameLTF.getTextFieldText() );
        reservoir.setType( _typeLTF.getTextFieldText() );
        reservoir.setOwner( _ownerLTF.getTextFieldText() );
        reservoir.setDeadPool( _converter.getDoubleValue( _deadLTF.getTextFieldText() ) );
        reservoir.setConserPool( _converter.getDoubleValue( _conservationLTF.getTextFieldText() ) );
        reservoir.setFloodPool( _converter.getDoubleValue( _floodLTF.getTextFieldText() ) );
        reservoir.setSpillWay( _converter.getDoubleValue( _spillwayLTF.getTextFieldText() ) );
        reservoir.setSill( _converter.getDoubleValue( _sillLTF.getTextFieldText() ) );
        reservoir.setTop( _converter.getDoubleValue( _topLTF.getTextFieldText() ) );
        reservoir.setSurchg( _converter.getDoubleValue( _maxSurchargeLTF.getTextFieldText() ) );
        reservoir.setElev( _converter.getDoubleValue( _reservoirLTF.getTextFieldText() ) );
        reservoir.setGates( _converter.getIntValue( _gatesLTF.getTextFieldText() ) );
        reservoir.setImpounded( _converter.getLongDateValue( _impoundDateLTF.getTextFieldText() ) );
        
        if ( _floodControlCB.isSelected() )
        {
            usesStringBuffer.append( "F" );
        }
        
        if ( _hydroCB.isSelected() )
        {
            usesStringBuffer.append( "H" );
        }
        
        if ( _lowFlowCB.isSelected() )
        {
            usesStringBuffer.append( "L" );
        }
        
        if ( _navigationCB.isSelected() )
        {
            usesStringBuffer.append( "N" );
        }
        
        if ( _recreationCB.isSelected() )
        {
            usesStringBuffer.append( "R" );
        }
        
        if ( _waterSupplyCB.isSelected() )
        {
            usesStringBuffer.append( "W" );
        }
        
        reservoir.setUses( usesStringBuffer.toString() );
        
        return reservoir;
    }
    
    private void deleteReservoir()
    {
        RaxReservoir raxReservoir = (RaxReservoir) _selectionStringToRaxReservoirMap.get( _beginDateLTF.getTextFieldText() );

        if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + raxReservoir.keyString(), "Delete Reservoir Entry" ) )
        {
            _dataMgr.deleteRaxReservoirFromDatabase( raxReservoir );
            raxReservoir = new RaxReservoir();
            populateDataFields( raxReservoir );
//            clearDataInputPanel();
            updateRaxReservoirList();
            updateSelectionComboBox();
        }
    }
    
    private void saveReservoir()
    {
        if ( _converter.getLongDateValue( _beginDateLTF.getTextFieldText() )  != DbTable.getNullDouble() )
        {
            RaxReservoir reservoir = getRaxReservoirToSave();
            RaxReservoir extractedReservoir = (RaxReservoir) _selectionStringToRaxReservoirMap.get( _beginDateLTF.getTextFieldText() );
            boolean startDateChanged = true;
            
            long endDate = _converter.getLongDateValue( _endDateLTF.getTextFieldText() );
            
            if ( reservoir.getBeginDate() == _beginDate )
            {
                startDateChanged = false;
            }
            
            boolean recordExists = _dataMgr.doesRaxReservoirExist( reservoir );
            
            if ( recordExists )  // record exists in database (lid/sbd)
            {
                if ( endDate != DbTable.getNullDouble() )  // end date is specified
                {
                    if ( startDateChanged )  // Start date has been changed since the gui popped up
                    {
                        if ( DialogHelper.displayConfirmDialog( this, "A record already exists for " + reservoir + "\n Are you sure you want to save", "Save RaxReservoir" ) );
                        {
                            _dataMgr.saveReservoir( reservoir ); //User confirms that they want to overwrite the changes
                        }
                    }
                    else  // Start date has not been changed
                    {
                        _dataMgr.saveReservoir( reservoir );
                    }
                }
                else  // End date is not specified
                {
                    boolean newerRecordExists = _dataMgr.doesNewerRaxReservoirExist( reservoir );

                    if ( startDateChanged )
                    {
                        if ( newerRecordExists )
                        {
                            DialogHelper.displayErrorDialog( this, "A record with a newer Sbd and null Sed exists", "Save RaxReservoir" );
                        }
                        else // Newer record does not exist
                        {
                            if ( DialogHelper.displayConfirmDialog( this, "A record with a newer Sbd and null Sed exists", "Save RaxReservoir" ) );
                            {
                                _dataMgr.saveReservoir( reservoir ); //User confirms that they want to overwrite the changes
                            }
                        }
                    }
                    else // Start date has not been changed
                    {
                        if ( newerRecordExists )
                        {
                            if ( DialogHelper.displayConfirmDialog( this, "A record with a newer Sbd and null Sed exists", "Save RaxReservoir" ) );
                            {
                                _dataMgr.saveReservoir( reservoir ); //User confirms that they want to overwrite the changes
                            }
                        }
                        else // Newer record does not exist
                        {
                            _dataMgr.saveReservoir( reservoir );
                        }
                    }
                }
            }
            else // Record does not exist
            {
                boolean newerRecordExists = _dataMgr.doesNewerRaxReservoirExist( reservoir );

                if ( endDate != DbTable.getNullDouble() )  // end date is specified
                {
                    _dataMgr.saveReservoir( reservoir );
                }
                else // end date is not specified
                {
                    if ( newerRecordExists )
                    {
                        DialogHelper.displayErrorDialog( this, "A record with a newer Sbd and null Sed exists", "Save RaxReservoir" );
                    }
                    else
                    {
                        _dataMgr.saveReservoir( reservoir );
                        _dataMgr.setEndDateForLidInReservoir( reservoir );
                    }
                }
            }
        }
        
        updateRaxReservoirList();
        updateSelectionComboBox();
    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();
        DateFieldMouseListener beginDateMouseListener = new DateFieldMouseListener( 0 );
        DateFieldMouseListener endDateMouseListener = new DateFieldMouseListener( 1 );
        DateFieldMouseListener impoundDateMouseListener = new DateFieldMouseListener( 2 );

        _selectionLCB.addComboBoxActionListener( new ItemSelectionListener() );
        _closeButton.addActionListener( windowCloser );
        _saveButton.addActionListener( new SaveButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener()  );
        _beginDateLTF.addTextFieldMouseListener( beginDateMouseListener );
        _endDateLTF.addTextFieldMouseListener( endDateMouseListener );
        _impoundDateLTF.addTextFieldMouseListener( impoundDateMouseListener );
        addWindowListener( windowCloser );
    }
    
    private class DateFieldMouseListener implements MouseListener
    {
        private int __clickedDateField = -9999;

        public DateFieldMouseListener( int clickedDateField )
        {
            __clickedDateField = clickedDateField;
        }
        public void mouseClicked( MouseEvent e ){}
        public void mouseEntered( MouseEvent e ){}
        public void mouseReleased( MouseEvent e ){}
        public void mouseExited( MouseEvent e ){}
        public void mousePressed( MouseEvent e )
        {
            launchDateWindow( __clickedDateField );
        }
    }
    
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveReservoir();
        }
    }
    
    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteReservoir();
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

    private class ItemSelectionListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e)
        {
            String selectedItem = _selectionLCB.getSelectedCBItem();
            
            RaxReservoir raxReservoir = (RaxReservoir) _selectionStringToRaxReservoirMap.get( selectedItem );
            
            if ( raxReservoir != null )
            {
                populateDataFields( raxReservoir );
            }
        }
    }
    
    public static void main( String args[] )
    {
        JFrame frame = new JFrame();
        frame.setSize( new Dimension( 1024, 768 ) );

        RaxBaseDataMgr dataMgr = new RaxBaseDataMgr( "jdbc:postgresql://ax2:5432/adb_ob82krf?user=pguser" );
        
        String title = "MLDN1";
        String name = dataMgr.getRaxLocation( "MLDN1" ).getName();
        
        if ( name != null )
        {
            title += " - " + name;
        }
        

        ReservoirEditor reservoirEditorGUI = new ReservoirEditor( frame, dataMgr, "MLDN1", title );
        reservoirEditorGUI.displayGUI();
    }
}
