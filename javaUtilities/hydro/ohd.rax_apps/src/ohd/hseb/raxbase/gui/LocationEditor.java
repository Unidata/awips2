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
import java.util.Calendar;
import java.util.GregorianCalendar;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;

import ohd.hseb.db.DbTable;
import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.raxbase.RaxBaseDataMgr;
import ohd.hseb.raxbase.model.RaxLocation;
import ohd.hseb.raxbase.util.DateManager;
import ohd.hseb.raxbase.util.HDateSuperChooser;
import ohd.hseb.rfc.util.graphics.HDateChooserOwner;
import ohd.hseb.util.StringDataConverter;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledTextField;

public class LocationEditor extends JDialog implements HDateChooserOwner
{
    private static final short MISSING = -9999;
    
    private Container _dialogContentPane = getContentPane();
    private RaxLocation _raxLocation = null;

    private HDateSuperChooser _hDateChooser = new HDateSuperChooser( false );

    private int _clickedDateField = MISSING;

    private long _beginDate = MISSING;
    
    private StringDataConverter _converter = new StringDataConverter();
    
    private RaxBaseDataMgr _dataMgr = null;
    
    private JPanel _geographicPhysicalPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _lidLTF = new LabeledTextField( "                Location:", "", 5 );
    private LabeledTextField _startDateLTF = new LabeledTextField( "                               Start Date:", "", 11 );
    private LabeledTextField _endDateLTF = new LabeledTextField( "                                 End Date:", "", 11 );
    private LabeledTextField _goesLTF = new LabeledTextField( "                                 GOES:", "", 9 );
    private LabeledTextField _nameLTF = new LabeledTextField( "Name:", "", 35 );
    private LabeledTextField _detailLTF = new LabeledTextField( "Detail:", "", 45 );
    private LabeledTextField _latitudeLTF = new LabeledTextField( "                          Latitude:", "", 8 );
    private LabeledTextField _longitudeLTF = new LabeledTextField( "                       Longitude:", "", 8 );
    private LabeledTextField _elevationLTF = new LabeledTextField( "                         Elevation:", "", 8 );
    private LabeledTextField _stateLTF = new LabeledTextField( "               State:", "", 2 );
    private LabeledTextField _hucLTF = new LabeledTextField( "                                HUC:", "", 8 );
    private LabeledTextField _countyFipsLTF = new LabeledTextField( "      County FIPS:", "", 3 );
    private LabeledTextField _zonLTF = new LabeledTextField( "          Zone Code:", "", 4 );
    private LabeledTextField _hsaLTF = new LabeledTextField( "                 HSA:", "", 3 );
    private LabeledTextField _wfoLTF = new LabeledTextField( "                 WFO:", "", 3 );
    private LabeledTextField _postLTF = new LabeledTextField( "     Post Code:", "", 2 );
    private LabeledTextField _dbSourceLTF = new LabeledTextField( "       DB Source:", "", 3 );
    private LabeledTextField _rfcLTF = new LabeledTextField( "              RFC:", "", 2 );
    private LabeledTextField _countryFipsLTF = new LabeledTextField( "Country FIPS:", "", 2 );
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _okButton = new JButton( "Save and Close" );
    private JButton _saveToDatabaseButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    
    private boolean _modifyLocation = false;
    
    
    public LocationEditor( JFrame frame, RaxBaseDataMgr dataMgr, RaxLocation location )
    {
        super( frame, "Location Editor", true );
        _raxLocation = location;
        _dataMgr = dataMgr;
        
        if ( _raxLocation != null )
        {
            setTitle( "Modify Location - " + _raxLocation.getLid() );
            _lidLTF.setEditTextField( false );
            _modifyLocation = true;
        }
        else
        {
            setTitle( "Add Location" );
            _lidLTF.setEditTextField( true );
            _modifyLocation = false;
        }
    }
    
    public LocationEditor( JFrame frame, RaxBaseDataMgr dataMgr )
    {
        this( frame, dataMgr, null );
    }

    
    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 700, 500 ) );
        populateFields();
        if ( _raxLocation != null )
        {
            _beginDate = _raxLocation.getBeginDate();
        }
        initGUI();
    }
    
    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        initFrameComponenets();
        initGeographicPhysicalPanel();
        initButtonPanel();
        addListeners();
        pack();
        setVisible( true );
    }
    
    private void populateFields()
    {
        
        if ( _raxLocation != null )
        {
            double lon = _raxLocation.getLongitude();
            double lat = _raxLocation.getLatitude();
            String lonString = "";
            String latString = "";
            
            if ( lon != DbTable.getNullDouble() )
            {
                lonString = _dataMgr.getFormattedDouble( lon );
            }
            
            if ( lat != DbTable.getNullDouble() )
            {
                latString = _dataMgr.getFormattedDouble( lat );
            }
           
            _lidLTF.setTextField( _raxLocation.getLid() );
            _goesLTF.setTextField( _raxLocation.getGoes() );
            _nameLTF.setTextField( _raxLocation.getName() );
            _detailLTF.setTextField( _raxLocation.getDetailInfo() );

            _latitudeLTF.setTextField( latString );
            _longitudeLTF.setTextField( lonString );
            _elevationLTF.setTextField( _converter.getStringFromInt( _raxLocation.getElevation() ) );
            _postLTF.setTextField( _converter.getStringFromShort( _raxLocation.getPostCode() ) );
            _startDateLTF.setTextField( _converter.getDateStringFromDateLong( _raxLocation.getBeginDate() ) );
            _endDateLTF.setTextField( _converter.getDateStringFromDateLong( _raxLocation.getEndDate() ) );
            _stateLTF.setTextField( _raxLocation.getState() );
            _hucLTF.setTextField( _raxLocation.getHuc() );
            _countyFipsLTF.setTextField( _raxLocation.getCountyFips() );
            _zonLTF.setTextField( _raxLocation.getZone() );
            _hsaLTF.setTextField( _raxLocation.getHsa() );
            _wfoLTF.setTextField( _raxLocation.getWfo() );
            _dbSourceLTF.setTextField( _raxLocation.getDbsource() );
            _rfcLTF.setTextField( _raxLocation.getRfc() );
            _countryFipsLTF.setTextField( _raxLocation.getCountryfips() );
        }
    }
    
    private void initGeographicPhysicalPanel()
    {
        GridBagConstraints gbLabelConstraints = new GridBagConstraints();
        GridBagConstraints gbTFConstraints = new GridBagConstraints();
        _geographicPhysicalPanel.setBorder( BorderFactory.createTitledBorder( "Geographic/Physical" ) );

        gbLabelConstraints.anchor = GridBagConstraints.EAST;
        gbLabelConstraints.weightx = 1;
        gbLabelConstraints.weighty = 0;
        
        gbTFConstraints.anchor = GridBagConstraints.WEST;
        gbTFConstraints.weightx = 1;
        gbTFConstraints.weighty = 0;
        JPanel vPanel = new JPanel();
        vPanel.setPreferredSize( new Dimension( 100, 100 ) );
//        vPanel.setBackground( Color.RED );
        
//                                                                                      X,    Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _geographicPhysicalPanel, _lidLTF,           1,    0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _geographicPhysicalPanel, _startDateLTF,     1,    1,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _geographicPhysicalPanel, _endDateLTF,       1,    2,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _geographicPhysicalPanel, _goesLTF,          1,    3,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _geographicPhysicalPanel, _nameLTF,          1,    4,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _geographicPhysicalPanel, _detailLTF,        1,    5,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _geographicPhysicalPanel, _latitudeLTF,      1,    6,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _geographicPhysicalPanel, _longitudeLTF,     1,    7,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _geographicPhysicalPanel, _elevationLTF,     1,    8,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _geographicPhysicalPanel, _stateLTF,         1,    9,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _geographicPhysicalPanel, _hucLTF,           1,   10,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _geographicPhysicalPanel, _countyFipsLTF,    1,   11,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _geographicPhysicalPanel, _zonLTF,           1,   12,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _geographicPhysicalPanel, _hsaLTF,           1,   13,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _geographicPhysicalPanel, _wfoLTF,           1,   14,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _geographicPhysicalPanel, _postLTF,          1,   15,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _geographicPhysicalPanel, _dbSourceLTF,      1,   16,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _geographicPhysicalPanel, _rfcLTF,           1,   17,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _geographicPhysicalPanel, _countryFipsLTF,   1,   18,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _geographicPhysicalPanel, vPanel,            2,    0,    1,    19, GridBagConstraints.BOTH );
        
        _hDateChooser.addOwner( this );

        _startDateLTF.setEditTextField( false );
        _endDateLTF.setEditTextField( false );
    }
    
    private void initFrameComponenets()
    {
        _dialogContentPane.setLayout( new GridBagLayout() );
        ComponentHelper.addFrameComponent( _dialogContentPane, _geographicPhysicalPanel, 0, 0, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _buttonPanel,             0, 1, 1, 1, GridBagConstraints.BOTH );
    }
    
    private void initButtonPanel()
    {
        _buttonPanel.setBorder( BorderFactory.createTitledBorder( "Database Controls" ) );
//                                                                                    X,    Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _buttonPanel, _okButton,                   0,    0,    1,     1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _saveToDatabaseButton,       1,    0,    1,     1, GridBagConstraints.VERTICAL );
        ComponentHelper.addPanelComponent( _buttonPanel, _closeButton,                2,    0,    1,     1, GridBagConstraints.VERTICAL );
    }
    
    private RaxLocation getRaxLocationToSave()
    {
        RaxLocation raxLocation = new RaxLocation();
        
        raxLocation.setLid( _lidLTF.getTextFieldText().toUpperCase() );
        raxLocation.setGoes( _goesLTF.getTextFieldText() );
        raxLocation.setName( _nameLTF.getTextFieldText() );
        raxLocation.setDetailInfo( _detailLTF.getTextFieldText() );
        raxLocation.setLatitude( _converter.getDoubleValue( _latitudeLTF.getTextFieldText() ) );
        raxLocation.setLongitude( _converter.getDoubleValue( _longitudeLTF.getTextFieldText() ) );
        raxLocation.setElevation( _converter.getIntValue( _elevationLTF.getTextFieldText() ) );
        raxLocation.setPostCode( _converter.getShortValue( _postLTF.getTextFieldText() ) );
        raxLocation.setBeginDate( _converter.getLongDateValue( _startDateLTF.getTextFieldText() ) );
        raxLocation.setEndDate( _converter.getLongDateValue( _endDateLTF.getTextFieldText() ) );
        raxLocation.setState( _stateLTF.getTextFieldText() );
        raxLocation.setHuc( _hucLTF.getTextFieldText() );
        raxLocation.setCountyFips( _countyFipsLTF.getTextFieldText() );
        raxLocation.setZone( _zonLTF.getTextFieldText() );
        raxLocation.setHsa( _hsaLTF.getTextFieldText() );
        raxLocation.setWfo( _wfoLTF.getTextFieldText() );
        raxLocation.setDbsource( _dbSourceLTF.getTextFieldText() );
        raxLocation.setRfc( _rfcLTF.getTextFieldText() );
        raxLocation.setCountryfips( _countryFipsLTF.getTextFieldText() );

        return raxLocation;
    }
    
    private void saveLocation()
    {
/*   Location Save Logic

 Add Location Save Button
User click's Add Location.
User enters all of the information.
When user click's save:
If the record with the same Lid/Sbd combination exists:
     Pop up an error message saying the record exists already.
If the record does not exist:
    If a Sed is not specified:
         If a previous record with the same (Lid/null Sed) combo and a more recent Sbd exists
              Pop up an error message letting the user know that a more recent (Lid/Sbd/Null Sed) combo exists.
         If a previous record with the same (Lid/null Sed) combo and an older Sbd exists
               Insert the new record into the database and set the older entries with the same (Lid/null Sed)s Sed to todays date              
    If a Sed is specified:
       add the new record to the database.

Modify Location Save Button
User click's Modify Location
User enters/modifies information.
When user click's save:
If a record with the same (Lid/Sbd) combination exists:
    If a Sed is specified:
         If the user has changed the Sbd since bringing up the Modify Location window:
             Pop up a warning message that queries the user on whether they are sure they want to overwrite the existing record.
         If the user has not modified the Sbd since bringing up the Modify Location window:
             Update the record in the database with the new information without prompting/warning the user.
    If a Sed is not specified:
         If the user has changed the Sbd since bringing up the Modify Location window:
             If a previous record with the same (Lid/null Sed) combo and a more recent Sbd does not exist:
                  Pop up a warning message that queries the user on whether they are sure they want to overwrite the existing record.
             If a previous record with the same (Lid/null Sed) combo and a more recent Sbd does exist:
                  Pop up an error message letting the user know that a more recent (Lid/Sbd/Null Sed) combo exists.
         If the user has not modified the Sbd since bringing up the Modify Location window:
             If a previous record with the same (Lid/null Sed) combo and a more recent Sbd does not exist:
                  Update the record in the database with the new information without prompting/warning the user.
             If a previous record with the same (Lid/null Sed) combo and a more recent Sbd does exist:
                  Pop up a warning message that queries the user on whether they are sure they want to overwrite the existing record.
If the record does not exist:
    If a Sed is not specified:
         If a previous record with the same (Lid/null Sed) combo and a more recent Sbd exists
              Pop up an error message letting the user know that a more recent (Lid/Sbd/Null Sed) combo exists.
         If a previous record with the same (Lid/null Sed) combo and an older Sbd exists
               Insert the new record into the database and set the older entries with the same (Lid/null Sed)s Sed to todays date              
    If a Sed is specified:
       add the new record to the database.
       
        DialogHelper.displayMessageDialog( this, "Soon to be implemented", "Save Location" );
*/
        if ( _converter.getLongDateValue( _startDateLTF.getTextFieldText() ) != DbTable.getNullDouble() )
        {
            RaxLocation raxLocation = getRaxLocationToSave();
            
            boolean startDateChanged = true;
            
            long endDate = _converter.getLongDateValue( _endDateLTF.getTextFieldText() );

            if ( raxLocation.getBeginDate() == _beginDate )
            {
                startDateChanged = false;
            }

            if ( _modifyLocation )  // User selected Modify Location
            {
                boolean recordExists = _dataMgr.doesRaxLocationExist( raxLocation );

                if ( recordExists ) // Lid/Start Date exists
                {
                    if ( endDate != DbTable.getNullLong() ) // End date is specified
                    {
                        if ( startDateChanged ) // Record found in the database AND start date was changed
                            // Have user confirm whether to save or not
                        {
                            if ( DialogHelper.displayConfirmDialog( this, "A record already exists for " + raxLocation + "\n Are you sure you want to save", "Save RaxLocation" ) );
                            {
                                _dataMgr.saveLocation( raxLocation ); //User confirms that they want to overwrite the changes
                            }
                        }
                        else // Start date was not changed.  
                        {
                            _dataMgr.saveLocation( raxLocation );
                        }
                    }
                    else     // End date is not specified
                    {
                        boolean newerRecordExists = _dataMgr.doesNewerRaxLocationExist( raxLocation );
                        
                        if ( startDateChanged )  // Start date changed
                        {
                            if ( newerRecordExists )  // newer record exists
                            {
                                DialogHelper.displayErrorDialog( this, "A record with a newer Sbd and null Sed exists", "Save Location" );
                            }
                            else  // newer record does not exist
                            {
                                if ( DialogHelper.displayConfirmDialog( this, "A record already exists for " + raxLocation + "\n Are you sure you want to save", "Save RaxLocation" ) )
                                {
                                    _dataMgr.saveLocation( raxLocation ); //User confirms that they want to overwrite the changes
                                }
                            }
                        }
                        else  // Start date has not been changed
                        {
                            if ( newerRecordExists )  // newer record exists
                            {
                                if ( DialogHelper.displayConfirmDialog( this, "A record already exists for " + raxLocation + "\n Are you sure you want to save", "Save RaxLocation" ) )
                                {
                                    _dataMgr.saveLocation( raxLocation ); //User confirms that they want to overwrite the changes
                                }
                            }
                            else  // newer record does not exist
                            {
                                _dataMgr.saveLocation( raxLocation );
                            }
                        }
                    }
                }  
                else   // Lid/Start Date does not exist
                {
                    if ( endDate == DbTable.getNullLong() )  // End date is not specified
                    {
                        boolean newerRecordExists = _dataMgr.doesNewerRaxLocationExist( raxLocation );
                        if ( newerRecordExists )  // Newer record exists
                        {
                            DialogHelper.displayErrorDialog( this, "A record with a newer Sbd and null Sed exists", "Save Location" );
                        }
                        else  // No newer records exist.  Insert new record and close out all old records with null end dates
                        {
                            _dataMgr.insertNewLocation( raxLocation );
                            _dataMgr.setEndDateForLidInLocation( raxLocation );
                        }
                    }
                    else  // End date is specified
                    {
                        _dataMgr.saveLocation( raxLocation );
                    }
                }
            }
            else     // User selected Add Location
            {
                boolean recordExists = _dataMgr.doesRaxLocationExist( raxLocation );

                if ( recordExists )  // A record with the same Lid/Sbd exists already 
                {
                    DialogHelper.displayErrorDialog( this, raxLocation + "\n\nalready exists", "Save Location" );
                }
                else  
                {
                    if ( endDate == DbTable.getNullLong() )  // End date is not specified
                    {
                        boolean newerRecordExists = _dataMgr.doesNewerRaxLocationExist( raxLocation );
                        if ( newerRecordExists )  // Newer record exists
                        {
                            DialogHelper.displayErrorDialog( this, "A record with a newer Sbd and null Sed exists", "Save Location" );
                        }
                        else  // No newer records exist.  Insert new record and close out all old records with null end dates
                        {
                            _dataMgr.insertNewLocation( raxLocation );
                            _dataMgr.setEndDateForLidInLocation( raxLocation );
                        }
                    }
                    else  // End date is specified
                    {
                        _dataMgr.insertNewLocation( raxLocation );
                    }
                }
            }
        }
    }
    
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();
        DateFieldMouseListener startDateFieldMouseListener = new DateFieldMouseListener( 0 );
        DateFieldMouseListener endDateFieldMouseListener = new DateFieldMouseListener( 1 );
        
        _saveToDatabaseButton.addActionListener( new SaveToDatabaseActionListener() );
        _okButton.addActionListener( new SaveToDatabaseAndCloseActionListener() );
        _closeButton.addActionListener( windowCloser );

        _startDateLTF.addTextFieldMouseListener( startDateFieldMouseListener );
        _endDateLTF.addTextFieldMouseListener( endDateFieldMouseListener );

        addWindowListener( windowCloser );
    }
    
    private class DateFieldMouseListener implements MouseListener
    {
        private int __clickedDateField = -9999;
        
        public DateFieldMouseListener( int clickedDateField )
        {
            __clickedDateField = clickedDateField;
        }

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
            launchDateWindow( __clickedDateField );
        }

        public void mouseReleased( MouseEvent e )
        {
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
            case 0: dateString = _startDateLTF.getTextFieldText(); break;
            case 1: dateString = _endDateLTF.getTextFieldText(); break;
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
        String dateString = null;

        if ( _clickedDateField != -9999 )
        {
            Calendar calendar = _hDateChooser.getDate();
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
                case 0: _startDateLTF.setTextField( dateString ); break;
                case 1: _endDateLTF.setTextField( dateString ); break;
            }
            _clickedDateField = -9999;
        }
    }
    
    private class SaveToDatabaseAndCloseActionListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveLocation();
            closeWindow();
        }
    }

    private class SaveToDatabaseActionListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveLocation();
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
        RaxBaseDataMgr dataMgr = new RaxBaseDataMgr( "jdbc:postgresql://ax2:5432/adb_ob82krf?user=pguser" );

        LocationEditor locEditorGUI = new LocationEditor( frame, dataMgr );
        locEditorGUI.displayGUI();
    }
}
