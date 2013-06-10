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

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.raxbase.RaxBaseDataMgr;
import ohd.hseb.raxbase.model.Sensok;
import ohd.hseb.raxbase.model.ShefDuration;
import ohd.hseb.raxbase.model.ShefExtremum;
import ohd.hseb.raxbase.model.ShefPE;
import ohd.hseb.raxbase.model.ShefProb;
import ohd.hseb.raxbase.model.ShefTS;
import ohd.hseb.raxbase.util.DateManager;
import ohd.hseb.raxbase.util.HDateSuperChooser;
import ohd.hseb.raxbase.util.PEDTSEPManager;
import ohd.hseb.rfc.util.graphics.HDateChooserOwner;
import ohd.hseb.util.StringDataConverter;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledComboBox;
import ohd.hseb.util.gui.LabeledTextField;

public class SensOKEditor extends JDialog implements HDateChooserOwner
{
    private Container _dialogContentPane = getContentPane();
    
    private List _sensokList = null;
    
    private Sensok _selectedSensok = null;
    
    private LabeledComboBox _selectionLCB = new LabeledComboBox();
    private Map _selectionStringToSensokMap = new HashMap();
    private String _lid = null;
    private RaxBaseDataMgr _dataMgr = null;

    private HDateSuperChooser _hDateChooser = new HDateSuperChooser( true );

    private StringDataConverter _converter = new StringDataConverter();
    
    private PEDTSEPManager _pedtsepManager = null;

    private JPanel _dataPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _lidLTF = new LabeledTextField( "LID:", "", "Location ID", 9 );
    private LabeledComboBox _peLCB = null;
    private LabeledComboBox _durLCB = null;
    private LabeledComboBox _tsLCB = null;
    private LabeledComboBox _extremumLCB = null;
    private LabeledComboBox _probLCB = null;
    private LabeledTextField _oktimeLTF = new LabeledTextField( "OKTime:", "", "Date and time of change in status", 15 );
    private JCheckBox _okCheckBox = new JCheckBox( "OK:");
    private LabeledTextField _initLTF = new LabeledTextField( "Initials:", "", "Initials of person making the entry", 4 );
    private LabeledTextField _reasonLTF = new LabeledTextField( "Reason:", "", "Reason for change in status (optional)", 80 );
    private LabeledTextField _agcodeLTF = new LabeledTextField( "Agency:", "", "Abbreviation for agency", 7 );
    private LabeledTextField _aglocLTF = new LabeledTextField( "Agency Location:", "", "Location of agency", 4 );
    private LabeledTextField _commentLTF = new LabeledTextField( "Comment:", "", "Notification comment (optional)", 41 );
    
    
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _deleteButton = new JButton( "Delete from Database" );

    public SensOKEditor( JFrame frame, RaxBaseDataMgr dataMgr, String lid, String title )
    {
        super( frame, "Sensok Editor - " + title, true );
        _dataMgr = dataMgr;
        _pedtsepManager = new PEDTSEPManager( _dataMgr );
        _lid = lid;
    }

    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 1024, 550 ) );
        initGUI();
    }

    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        updateSensokList();
        
        updateSelectionComboBox();
        initComboBoxes();
        initDataPanel();
        initButtonPanel();
        initFrameComponents();
        addListeners();
        populateDataPanel( _selectedSensok );
        pack();
        setVisible( true );
    }
    
    private void initDataPanel()
    {
        _dataPanel.setBorder( BorderFactory.createTitledBorder( "Selected Entry" ) );
        _oktimeLTF.setEditTextField( false );
        _hDateChooser.addOwner( this );
        _lidLTF.setEditTextField( false );
        _lidLTF.setTextField( _lid );
        
//                                                                      X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _dataPanel, _lidLTF,         0,   0,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _peLCB,          0,   1,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _durLCB,         0,   2,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _tsLCB,          0,   3,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _extremumLCB,    0,   4,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _probLCB,        0,   5,    2,     1, 1, 1, GridBagConstraints.NONE );

        ComponentHelper.addPanelComponent( _dataPanel, _oktimeLTF,      0,   6,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _okCheckBox,     0,   7,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _initLTF,        0,   8,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _reasonLTF,      0,   9,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _agcodeLTF,      0,  10,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _aglocLTF,       0,  11,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _commentLTF,      0,  12,    2,     1, 1, 1, GridBagConstraints.NONE );
        
        _okCheckBox.setToolTipText( "Checked=Y, Unchecked=N" );
    }
    
    private void initButtonPanel()
    {
        _buttonPanel.setBorder( BorderFactory.createTitledBorder( "Database Controls" ) );
        
//                                                                              X,   Y,  #Col,  #Row
        ComponentHelper.addPanelComponent( _buttonPanel, _saveAndCloseButton,   0,   0,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _saveButton,           1,   0,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _deleteButton,         2,   0,    1,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _buttonPanel, _closeButton,          1,   1,    1,     1, 1, 1, GridBagConstraints.NONE );
    }

    private void initFrameComponents()
    {
//                                                                                 X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _selectionLCB,      0,   0,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _dialogContentPane, _dataPanel,         0,   1,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _dialogContentPane, _buttonPanel,       0,   2,    2,     1, 1, 1, GridBagConstraints.NONE );
    }

    private void populateDataPanel( Sensok sensok )
    {
        if ( sensok != null )
        {
            _lidLTF.setTextField( sensok.getLid() );

            ShefPE shefPE = (ShefPE) _dataMgr.getShefPEMap().get( sensok.getPe() );
            _peLCB.setSelectedItem( _pedtsepManager.getShefPEComboBoxString( shefPE ) );

            ShefDuration shefDur = (ShefDuration) _dataMgr.getShefDurationMap().get( sensok.getDur() );
            _durLCB.setSelectedItem( _pedtsepManager.getShefDurationComboBoxString( shefDur ) );

            ShefTS shefTS = (ShefTS) _dataMgr.getShefTSMap().get( sensok.getTs() );
            _tsLCB.setSelectedItem( _pedtsepManager.getShefTSString( shefTS ) );

            ShefExtremum shefEx = (ShefExtremum) _dataMgr.getShefExtremumMap().get( sensok.getExtremum() );
            _extremumLCB.setSelectedItem( _pedtsepManager.getShefExtremumComboBoxString( shefEx ) );

            ShefProb shefProb = (ShefProb) _dataMgr.getShefProbMap().get( sensok.getP() );
            _probLCB.setSelectedItem( _pedtsepManager.getShefProbComboBoxString( shefProb ) );
            
            _oktimeLTF.setTextField( _converter.getDateTimeStringFromDateTimeLong( sensok.getOktime() ) );
            
            if ( sensok.getOk().equalsIgnoreCase( "Y" ) )
            {
                _okCheckBox.setSelected( true );
            }
            else
            {
                _okCheckBox.setSelected( false );
            }
            _initLTF.setTextField( sensok.getInit() );
            _reasonLTF.setTextField( sensok.getReason() );
            _agcodeLTF.setTextField( sensok.getAgcode() );
            _aglocLTF.setTextField( sensok.getAgloc() );
            _commentLTF.setTextField( sensok.getComment() );
        }
    }

    private void initComboBoxes()
    {
        _peLCB = _pedtsepManager.getPeLCB();
        _durLCB = _pedtsepManager.getDurLCB();
        _tsLCB = _pedtsepManager.getTsLCB();
        _extremumLCB = _pedtsepManager.getExtremumLCB();
        _probLCB = _pedtsepManager.getProbLCB();
    }


    private void updateSelectionComboBox()
    {
        Sensok sensok = null;
        List selectionComboBoxStringList = new ArrayList();
        String dateString = null;
        
        _selectionStringToSensokMap.clear();
        _selectionLCB.getComboBox().removeAllItems();
        
        for ( int i = 0; i < _sensokList.size(); i++ )
        {
            sensok = (Sensok) _sensokList.get( i );
            selectionComboBoxStringList.add( getSelectedSensokString( sensok ) );
            _selectionStringToSensokMap.put( getSelectedSensokString( sensok ), sensok );
        }
        
        if ( selectionComboBoxStringList.isEmpty() )
        {
            selectionComboBoxStringList.add( "No entries found" );
        }
        _selectionLCB.setComboBoxFromStringArray( selectionComboBoxStringList.toArray() );
        _selectionLCB.setLabel( "Select Sensok Entry:" );
        _selectionLCB.setLabelPreferredSize( new Dimension( 150, 15 ) );
        _selectionLCB.setComboBoxPreferredSize( new Dimension( 600, 15 ) );

        String selectedItem = _selectionLCB.getSelectedCBItem();
        
        _selectedSensok = (Sensok) _selectionStringToSensokMap.get( selectedItem );
    }
    
    private String getSelectedSensokString( Sensok sensok )
    {
        return "LID = " + sensok.getLid() + " | PE = " + sensok.getPe() + " | Dur = " + sensok.getDur() + " | TS = " + 
        sensok.getTs() + " | Extremum = " + sensok.getExtremum() + " | P = " + sensok.getP() + " | OKTime = " + DbTimeHelper.getDateStringFromLongTime( sensok.getOktime() );
    }

    private void updateSensokList()
    {
        _sensokList = _dataMgr.getSensokList( _lid );
    }
    
    private void launchDateWindow()
    {
        GregorianCalendar calendar = new GregorianCalendar();
        long dayInMillis = 60*60*24*1000;
        long dateLong = System.currentTimeMillis();

        if ( ! _oktimeLTF.getTextFieldText().equalsIgnoreCase( "" ) )
        {
            dateLong = _converter.getLongDateValue( _oktimeLTF.getTextFieldText() ) + dayInMillis;
        }

        calendar.setTimeInMillis( dateLong );

        _hDateChooser.setDate( calendar );
        _hDateChooser.setModal( true );
        _hDateChooser.setVisible( true );
    }

    public void dateChosen( JDialog jdialog )
    {
        Calendar calendar = _hDateChooser.getDate();
        String dateString = null;
        
        if ( _hDateChooser.isClearDate() )
        {
            dateString = "";
        }
        else
        {
            dateString = DateManager.getDateTimeStringFromCalendar( calendar );
        }
        _oktimeLTF.setTextField( dateString );
    }
    
    private boolean saveSensok()
    {
        boolean saved = false;
        Sensok sensok = new Sensok();
        
        String pe = (String) _peLCB.getSelectedItem();
        String ts = (String) _tsLCB.getSelectedItem();
        String extremum = (String) _extremumLCB.getSelectedItem();
        String duration = (String) _durLCB.getSelectedItem();
        String prob = (String) _probLCB.getSelectedItem();
        short iDuration = (Short) _pedtsepManager.getDurCBStringToIDurMap().get( duration );

        
        sensok.setLid( _lidLTF.getTextFieldText() );
        sensok.setPe( (String) _pedtsepManager.getPeCBStringToPeMap().get( pe ) );
        sensok.setDur( (String) _pedtsepManager.getDurCBStringToDurMap().get( duration ) );
        sensok.setIdur( iDuration );
        sensok.setTs( (String) _pedtsepManager.getTsCBStringToTSMap().get( ts ) );
        sensok.setExtremum( (String) _pedtsepManager.getExtremumCBStringToExtremumMap().get( extremum ) );
        sensok.setP( (String) _pedtsepManager.getProbCBStringToProbMap().get( prob ) );

        sensok.setOktime( _converter.getLongDateTimeValue( _oktimeLTF.getTextFieldText() ) );
        if ( _okCheckBox.isSelected() )
        {
            sensok.setOk( "Y" );
        }
        else
        {
            sensok.setOk( "N" );
        }
        sensok.setInit( _initLTF.getTextFieldText() );
        sensok.setReason( _reasonLTF.getTextFieldText() );
        sensok.setAgcode( _agcodeLTF.getTextFieldText() );
        sensok.setAgloc( _aglocLTF.getTextFieldText() );
        sensok.setComment( _commentLTF.getTextFieldText() );
        
        saved = _dataMgr.saveSensok( sensok );
        updateSensokList();
        updateSelectionComboBox();
        
        return saved;
    }

    private void deleteSensok()
    {
        String selectedItem = _selectionLCB.getSelectedCBItem();
        
        Sensok sensok = (Sensok) _selectionStringToSensokMap.get( selectedItem );

        if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + sensok.keyString(), "Delete Sensok" ) )
        {
            boolean success = _dataMgr.deleteSensokFromDatabase( sensok );
            if ( ! success )
            {
                DialogHelper.displayErrorDialog( this, "Unable to delete Sensok entry", "Delete Sensok Error" );
            }
            updateSensokList();
            updateSelectionComboBox();
        }

    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();
        _selectionLCB.addComboBoxActionListener( new ItemSelectionListener() );

        _closeButton.addActionListener( windowCloser );
        _saveButton.addActionListener( new SaveButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
        
        _oktimeLTF.addTextFieldMouseListener( new DateFieldMouseListener() );
    }

    private class DateFieldMouseListener implements MouseListener
    {
        public void mouseReleased( MouseEvent e ){}
        public void mouseClicked( MouseEvent e ){}
        public void mouseEntered( MouseEvent e ){}
        public void mouseExited( MouseEvent e ){}
        public void mousePressed( MouseEvent e )
        {
            launchDateWindow();
        }
    }

    private class DeleteButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            deleteSensok();
        }
    }
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveSensok();
        }
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveSensok() )
            {
                closeWindow();
            }
            else
            {
                DialogHelper.displayErrorDialog( SensOKEditor.this, "Unable to save the Sensok entry", "Error Saving Sensok" );
            }
        }
    }
    
    private class ItemSelectionListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e)
        {
            String selectedItem = _selectionLCB.getSelectedCBItem();
            
            Sensok sensok = (Sensok) _selectionStringToSensokMap.get( selectedItem );
            
            if ( sensok != null )
            {
                populateDataPanel( sensok );
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

        RaxBaseDataMgr dataMgr = new RaxBaseDataMgr( "jdbc:postgresql://ax2:5432/adb_ob82krf?user=pguser" );
        String title = "ALEC2";
        String name = dataMgr.getRaxLocation( "ALEC2" ).getName();
        
        if ( name != null )
        {
            title += " - " + name;
        }
        

        SensOKEditor sensOKEditor = new SensOKEditor( frame, dataMgr, "ALEC2", title );
        sensOKEditor.displayGUI();
    }

}
