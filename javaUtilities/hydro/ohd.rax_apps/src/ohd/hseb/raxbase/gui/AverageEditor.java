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
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;

import ohd.hseb.raxbase.RaxBaseDataMgr;
import ohd.hseb.raxbase.model.Average;
import ohd.hseb.raxbase.model.ShefDuration;
import ohd.hseb.raxbase.model.ShefExtremum;
import ohd.hseb.raxbase.model.ShefPE;
import ohd.hseb.raxbase.model.ShefProb;
import ohd.hseb.raxbase.model.ShefTS;
import ohd.hseb.raxbase.util.DateManager;
import ohd.hseb.raxbase.util.HDateSuperChooser;
import ohd.hseb.rfc.util.graphics.HDateChooserOwner;
import ohd.hseb.util.StringDataConverter;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.LabeledComboBox;
import ohd.hseb.util.gui.LabeledTextField;

public class AverageEditor extends JDialog implements HDateChooserOwner
{
    private Container _dialogContentPane = getContentPane();
    
    private List _averageList = null;
    
    private Average _selectedAverage = null;
    
    private LabeledComboBox _selectionLCB = new LabeledComboBox();
    private Map _selectionStringToAverageMap = new HashMap();
    private String _lid = null;
    private RaxBaseDataMgr _dataMgr = null;

    private HDateSuperChooser _hDateChooser = new HDateSuperChooser( false );

    private StringDataConverter _converter = new StringDataConverter();
    
    private JPanel _dataPanel = new JPanel( new GridBagLayout() );
    private LabeledTextField _lidLTF = new LabeledTextField( "Lid:", "", "Location ID", 8 );
    private LabeledComboBox _peLCB = null;
    private LabeledComboBox _durLCB = null;
    private LabeledComboBox _tsLCB = null;
    private LabeledComboBox _extremumLCB = null;
    private LabeledComboBox _probLCB = null;
    private LabeledTextField _janLTF = new LabeledTextField( "Jan:" , "", "Monthly Value for January", 10 );
    private LabeledTextField _febLTF = new LabeledTextField( "Feb:" , "", "Monthly Value for February", 10 );
    private LabeledTextField _marLTF = new LabeledTextField( "Mar:" , "", "Monthly Value for March", 10 );
    private LabeledTextField _aprLTF = new LabeledTextField( "Apr:" , "", "Monthly Value for April", 10 );
    private LabeledTextField _mayLTF = new LabeledTextField( "May:" , "", "Monthly Value for May", 10 );
    private LabeledTextField _junLTF = new LabeledTextField( "Jun:" , "", "Monthly Value for June", 10 );
    private LabeledTextField _julLTF = new LabeledTextField( "Jul:" , "", "Monthly Value for July", 10 );
    private LabeledTextField _augLTF = new LabeledTextField( "Aug:" , "", "Monthly Value for August", 10 );
    private LabeledTextField _sepLTF = new LabeledTextField( "Sep:" , "", "Monthly Value for September", 10 );
    private LabeledTextField _octLTF = new LabeledTextField( "Oct:" , "", "Monthly Value for October", 10 );
    private LabeledTextField _novLTF = new LabeledTextField( "Nov:" , "", "Monthly Value for November", 10 );
    private LabeledTextField _decLTF = new LabeledTextField( "Dec:" , "", "Monthly Value for December", 10 );
    private LabeledTextField _calcDateLTF = new LabeledTextField( "CalcDate:", "", "Date monthly values were calculated", 10 );
   
    private List _peCBStringList = new ArrayList();
    private List _tsCBStringList = new ArrayList();
    private Map _peCBStringToPeMap = new HashMap();
    private Map _durCBStringToDurMap = new HashMap();
    private Map _durCBStringToIDurMap = new HashMap();
    private Map _probCBStringToProbMap = new HashMap();
    private Map _tsCBStringToTSMap = new HashMap();
    private Map _extremumCBStringToExtremumMap = new HashMap();

    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _saveAndCloseButton = new JButton( "Save and Close" );
    private JButton _saveButton = new JButton( "Save" );
    private JButton _closeButton = new JButton( "Close" );
    private JButton _deleteButton = new JButton( "Delete from Database" );
    
    private JPopupMenu _popupMenu = new JPopupMenu();
    
    public AverageEditor( JFrame frame, RaxBaseDataMgr dataMgr, String lid, String title )
    {
        super( frame, "Average Editor - " + title, true );
        _dataMgr = dataMgr;
        _lid = lid;
    }

    public void displayGUI()
    {
        setPreferredSize( new Dimension ( 620, 550 ) );
        initGUI();
    }

    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        updateAverageList();
        
        updateSelectionComboBox();
        initComboBoxes();
        initDataPanel();
        initButtonPanel();
        initFrameComponents();
        addListeners();
        populateDataPanel( _selectedAverage );
        pack();
        setVisible( true );
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
    
    private void initDataPanel()
    {
        _dataPanel.setBorder( BorderFactory.createTitledBorder( "Selected Entry" ) );
        _calcDateLTF.setEditTextField( false );
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

//        ComponentHelper.addPanelComponent( _dataPanel, _pLTF,           0,   5,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _janLTF,         0,   6,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _febLTF,         0,   7,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _marLTF,         0,   8,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _aprLTF,         0,   9,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _mayLTF,         0,  10,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _junLTF,         0,  11,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _julLTF,         0,  12,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _augLTF,         0,  13,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _sepLTF,         0,  14,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _octLTF,         0,  15,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _novLTF,         0,  16,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _decLTF,         0,  17,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addPanelComponent( _dataPanel, _calcDateLTF,    0,  18,    2,     1, 1, 1, GridBagConstraints.NONE );
    }
    
    private void initFrameComponents()
    {
//                                                                                 X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _selectionLCB,      0,   0,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _dialogContentPane, _dataPanel,         0,   1,    2,     1, 1, 1, GridBagConstraints.NONE );
        ComponentHelper.addFrameComponent( _dialogContentPane, _buttonPanel,       0,   2,    2,     1, 1, 1, GridBagConstraints.NONE );
    }
    
    private void populateDataPanel( Average average )
    {
        if ( average != null )
        {
            _lidLTF.setTextField( average.getLid() );

            ShefPE shefPE = (ShefPE) _dataMgr.getShefPEMap().get( average.getPe() );
            _peLCB.setSelectedItem( getShefPEComboBoxString( shefPE ) );

            ShefDuration shefDur = (ShefDuration) _dataMgr.getShefDurationMap().get( average.getDur() );
            _durLCB.setSelectedItem( getShefDurationComboBoxString( shefDur ) );

            ShefTS shefTS = (ShefTS) _dataMgr.getShefTSMap().get( average.getTs() );
            _tsLCB.setSelectedItem( getShefTSString( shefTS ) );

            ShefExtremum shefEx = (ShefExtremum) _dataMgr.getShefExtremumMap().get( average.getExtremum() );
            _extremumLCB.setSelectedItem( getShefExtremumComboBoxString( shefEx ) );

            ShefProb shefProb = (ShefProb) _dataMgr.getShefProbMap().get( average.getP() );
            _probLCB.setSelectedItem( getShefProbComboBoxString( shefProb ) );
            
            _janLTF.setTextField( average.getJan() );
            _febLTF.setTextField( average.getFeb() );
            _marLTF.setTextField( average.getMar() );
            _aprLTF.setTextField( average.getApr() );
            _mayLTF.setTextField( average.getMay() );
            _junLTF.setTextField( average.getJun() );
            _julLTF.setTextField( average.getJul() );
            _augLTF.setTextField( average.getAug() );
            _sepLTF.setTextField( average.getSep() );
            _octLTF.setTextField( average.getOct() );
            _novLTF.setTextField( average.getNov() );
            _decLTF.setTextField( average.getDec() );
            _calcDateLTF.setTextField( _converter.getDateStringFromDateLong( average.getCalcdate() ) );
        }
    }

    
    private void initComboBoxes()
    {
        List shefPeList = _dataMgr.getShefPeList();
        List shefDurationList = _dataMgr.getShefDurationList();
        List shefTypeSourceList = _dataMgr.getShefTSList();
        List shefExtremumList = _dataMgr.getShefExtremumList();
        List shefProbList = _dataMgr.getShefProbList();
        
        List shefPeStringList = new ArrayList();
        List shefDurationStringList = new ArrayList();
        List shefTypeSourceStringList = new ArrayList();
        List shefExtremumStringList = new ArrayList();
        List shefProbStringList = new ArrayList();
        
        for ( int i = 0; i < shefPeList.size(); i++ )
        {
            ShefPE shefPE = (ShefPE) shefPeList.get( i );
            shefPeStringList.add( getShefPEComboBoxString( shefPE ) );
            _peCBStringList.add( getShefPEComboBoxString( shefPE ) );
            _peCBStringToPeMap.put( getShefPEComboBoxString( shefPE ), shefPE.getPe() );
        }
        
        for ( int i = 0; i < shefDurationList.size(); i++ )
        {
            ShefDuration shefDur = (ShefDuration) shefDurationList.get( i );
            shefDurationStringList.add( getShefDurationComboBoxString( shefDur ) );
            _durCBStringToDurMap.put( getShefDurationComboBoxString( shefDur ), shefDur.getDuration() );
            _durCBStringToIDurMap.put( getShefDurationComboBoxString( shefDur ), shefDur.getIduration() );
        }
        
        for ( int i = 0; i < shefProbList.size(); i++ )
        {
            ShefProb shefProb = (ShefProb) shefProbList.get( i );
            shefProbStringList.add( getShefProbComboBoxString( shefProb ) );
            _probCBStringToProbMap.put( getShefProbComboBoxString( shefProb ), shefProb.getP() );
        }
        
        for ( int i = 0; i < shefTypeSourceList.size(); i++ )
        {
            ShefTS shefTS = (ShefTS) shefTypeSourceList.get( i );
            shefTypeSourceStringList.add( getShefTSString( shefTS ) );
            _tsCBStringList.add( getShefTSString( shefTS ) );
            _tsCBStringToTSMap.put( getShefTSString( shefTS ), shefTS.getTs() );
        }
        
        for ( int i = 0; i < shefExtremumList.size(); i++ )
        {
            ShefExtremum shefEx = (ShefExtremum) shefExtremumList.get( i );
            shefExtremumStringList.add( getShefExtremumComboBoxString(shefEx ) );
            _extremumCBStringToExtremumMap.put( getShefExtremumComboBoxString( shefEx ), shefEx.getExtremum() );
        }
        
        _peLCB = new LabeledComboBox( "PE:", shefPeStringList );
        _durLCB = new LabeledComboBox( "Duration:", shefDurationStringList );
        _tsLCB = new LabeledComboBox( "Type Source:", shefTypeSourceStringList );
        _extremumLCB = new LabeledComboBox( "Extremum:", shefExtremumStringList );
        _probLCB = new LabeledComboBox( "Probability:", shefProbStringList );
    }
    
    private String getShefExtremumComboBoxString( ShefExtremum shefEx )
    {
        String shefExtremumString = shefEx.getName() + " (" + shefEx.getExtremum() + ")";
        
        return shefExtremumString;
    }
    
    private String getShefTSString( ShefTS shefTS )
    {
        String shefTSString = shefTS.getName() + " (" + shefTS.getTs() + ")";
        
        return shefTSString;
    }
    
    private String getShefPEComboBoxString( ShefPE shefPE )
    {
        String shefPEString = shefPE.getPe() + " " + shefPE.getName();
        
        return shefPEString;
    }
    
    private String getShefDurationComboBoxString( ShefDuration shefDuration )
    {
        String shefDurationString = shefDuration.getName() + " ( " + shefDuration.getIduration() + "/" + shefDuration.getDuration() + " )";

        return shefDurationString;
    }

    private String getShefProbComboBoxString( ShefProb shefProb )
    {
        String shefProbString = "";
        
        if ( shefProb != null )
        {
            shefProbString = shefProb.getP() + "/" + shefProb.getProbability();
        }
        
        return shefProbString;
    }
    
    private void updateSelectionComboBox()
    {
        Average average = null;
        List selectionComboBoxStringList = new ArrayList();
        String dateString = null;
        
        _selectionStringToAverageMap.clear();
        _selectionLCB.getComboBox().removeAllItems();
        
        for ( int i = 0; i < _averageList.size(); i++ )
        {
            average = (Average) _averageList.get( i );
            selectionComboBoxStringList.add( getSelectedAverageString( average ) );
            _selectionStringToAverageMap.put( getSelectedAverageString( average ), average );
        }
        
        if ( selectionComboBoxStringList.isEmpty() )
        {
            selectionComboBoxStringList.add( "No entries found" );
        }
        _selectionLCB.setComboBoxFromStringArray( selectionComboBoxStringList.toArray() );
        _selectionLCB.setLabel( "Select Average Entry:" );
        _selectionLCB.setLabelPreferredSize( new Dimension( 150, 15 ) );
        _selectionLCB.setComboBoxPreferredSize( new Dimension( 375, 15 ) );

        String selectedItem = _selectionLCB.getSelectedCBItem();
        
        _selectedAverage = (Average) _selectionStringToAverageMap.get( selectedItem );
    }
    
    private String getSelectedAverageString( Average average )
    {
        return "PE = " + average.getPe() + " | Dur = " + average.getDur() + " | TS = " + 
               average.getTs() + " | Extremum = " + average.getExtremum() + " | P = " + average.getP();
    }

    private void updateAverageList()
    {
        _averageList = _dataMgr.getAverageList( _lid );
    }
    
    private void deleteAverage()
    {
        String selectedItem = _selectionLCB.getSelectedCBItem();
        
        Average average = (Average) _selectionStringToAverageMap.get( selectedItem );

        if ( DialogHelper.displayConfirmDialog( this, "Are you sure you want to delete " + average.keyString(), "Delete Average" ) )
        {
            boolean success = _dataMgr.deleteAverageFromDatabase( average );
            if ( ! success )
            {
                DialogHelper.displayErrorDialog( this, "Unable to delete Average entry", "Delete Average Error" );
            }
            updateAverageList();
            updateSelectionComboBox();
        }
    }
    
    private boolean saveAverage()
    {
        boolean saved = false;
        
        Average average = new Average();
        
        String pe = (String) _peLCB.getSelectedItem();
        String ts = (String) _tsLCB.getSelectedItem();
        String extremum = (String) _extremumLCB.getSelectedItem();
        String duration = (String) _durLCB.getSelectedItem();
        String prob = (String) _probLCB.getSelectedItem();
        short iDuration = (Short) _durCBStringToIDurMap.get( duration );

        
        average.setLid( _lidLTF.getTextFieldText() );
        average.setPe( (String) _peCBStringToPeMap.get( pe ) );
        average.setDur( (String) _durCBStringToDurMap.get( duration ) );
        average.setIdur( iDuration );
        average.setTs( (String) _tsCBStringToTSMap.get( ts ) );
        average.setExtremum( (String) _extremumCBStringToExtremumMap.get( extremum ) );
        average.setP( (String) _probCBStringToProbMap.get( prob ) );
        average.setJan( _converter.getDoubleValue( _janLTF.getTextFieldText() ) );
        average.setFeb( _converter.getDoubleValue( _febLTF.getTextFieldText() ) );
        average.setMar( _converter.getDoubleValue( _marLTF.getTextFieldText() ) );
        average.setApr( _converter.getDoubleValue( _aprLTF.getTextFieldText() ) );
        average.setMay( _converter.getDoubleValue( _mayLTF.getTextFieldText() ) );
        average.setJun( _converter.getDoubleValue( _junLTF.getTextFieldText() ) );
        average.setJul( _converter.getDoubleValue( _julLTF.getTextFieldText() ) );
        average.setAug( _converter.getDoubleValue( _augLTF.getTextFieldText() ) );
        average.setSep( _converter.getDoubleValue( _sepLTF.getTextFieldText() ) );
        average.setOct( _converter.getDoubleValue( _octLTF.getTextFieldText() ) );
        average.setNov( _converter.getDoubleValue( _novLTF.getTextFieldText() ) );
        average.setDec( _converter.getDoubleValue( _decLTF.getTextFieldText() ) );
        average.setCalcdate( _converter.getLongDateValue( _calcDateLTF.getTextFieldText() ) );
        
        saved = _dataMgr.saveAverage( average );
        updateAverageList();
        updateSelectionComboBox();
        
        return saved;
    }

    private void launchDateWindow()
    {
        GregorianCalendar calendar = new GregorianCalendar();
        long dayInMillis = 60*60*24*1000;
        long dateLong = System.currentTimeMillis();

        if ( ! _calcDateLTF.getTextFieldText().equalsIgnoreCase( "" ) )
        {
            dateLong = _converter.getLongDateValue( _calcDateLTF.getTextFieldText() ) + dayInMillis;
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
            dateString = DateManager.getDateStringFromCalendar( calendar );
        }
        
        _calcDateLTF.setTextField( dateString );
    }

    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();
        _selectionLCB.addComboBoxActionListener( new ItemSelectionListener() );

        _closeButton.addActionListener( windowCloser );
        _saveButton.addActionListener( new SaveButtonListener() );
        _saveAndCloseButton.addActionListener( new SaveAndCloseButtonListener() );
        _deleteButton.addActionListener( new DeleteButtonListener() );
        
        _calcDateLTF.addTextFieldMouseListener( new DateFieldMouseListener() );
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
            deleteAverage();
        }
    }
    private class SaveButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            saveAverage();
        }
    }
    
    private class SaveAndCloseButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            if ( saveAverage() )
            {
                closeWindow();
            }
            else
            {
                DialogHelper.displayErrorDialog( AverageEditor.this, "Unable to save the Average entry", "Error Saving Average" );
            }
        }
    }
    
    private class ItemSelectionListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e)
        {
            String selectedItem = _selectionLCB.getSelectedCBItem();
            
            Average average = (Average) _selectionStringToAverageMap.get( selectedItem );
            
            if ( average != null )
            {
                populateDataPanel( average );
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
        

        AverageEditor averageEditor = new AverageEditor( frame, dataMgr, "ALEC2", title );
        averageEditor.displayGUI();
    }
}
