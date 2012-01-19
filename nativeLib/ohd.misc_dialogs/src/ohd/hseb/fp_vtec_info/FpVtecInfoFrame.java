package ohd.hseb.fp_vtec_info;

import java.awt.*;
import java.awt.event.*;

import javax.swing.border.Border;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.*;

import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.OutputStream;

import java.io.PrintWriter;
import ohd.hseb.gui.util.ScreenCaptureActionListener;
import ohd.hseb.measurement.MeasuringUnit;
import ohd.hseb.util.gui.DialogHelper;
//import ohd.hseb.util.gui.WindowResizingManager;
import ohd.hseb.util.gui.drawing.TsPaintableCanvas;
import ohd.hseb.util.gui.DateTimeTextField;
import ohd.hseb.db.DbTimeHelper;


//------------------------------------------------------------------------------

public class FpVtecInfoFrame extends JFrame
{
    //------------------------------------------------------------------------------
    //private static final int MILLIS_PER_HOUR = 1000 * 60 * 60;  
    private static final int MILLIS_PER_SECOND = 1000;
    private static long _dragged_line;
    //private static final int HOURS_TO_ROUND = 6;
    private static final long MISSING_INT = -9999;
    private static final String MISSING_STR = "MSG";
    private Dimension _minSize = new Dimension(250, 250);
    private Dimension _maxSize = new Dimension(1000,1000);
    
    protected static final int STILL_OPENED = 0;
    protected static final int CLOSED_BUT_NOT_SAVED = -1;
    
    private OutputStream _outputStream = null;
    //private boolean _fileIsOpen = false;
    private PrintWriter _writer = null;
    private static long _lastUpdateTime = STILL_OPENED;
    private long _currentTime = 0;
    
    //window sizing variables
    
    private Dimension _initialSize = new Dimension(1250, 550);
    
    private JMenuBar _menuBar = null;  
    private JPanel _mainPanel = null;
    private JPanel _tslPanel = null;
    private JPanel _curvtecPanel = null;
    private JPanel _prevvtecPanel = null;
    private JPanel _windowButtonPanel = null;
    private JPanel _buttonPanel = null;
    private Border _panelBorder = BorderFactory.createLineBorder(Color.black);
    
    // in _curvtecPanel
    private JLabel _curvtecPanelLabel = null;
    private JLabel _curvtecActionComboBoxLabel = null;
   
    private String[] _curvtecActionComboBoxString = {"CAN", "CON", "COR", "EXP", "EXT", 
                                                     "NEW", "ROU"};
    
    private JComboBox _curvtecActionComboBox = new JComboBox(_curvtecActionComboBoxString);
    private JLabel _curvtecPhenomComboBoxLabel = null;
    private String[] _curvtecPhenomComboBoxString = {"FL", "HY"};
    private JComboBox _curvtecPhenomComboBox = new JComboBox(_curvtecPhenomComboBoxString);
    private JLabel _curvtecSignifComboBoxLabel = null;
    private String[] _curvtecSignifComboBoxString = {"A (wAtch)", "S (Statement)", 
    		                                         "W (Warning)", "Y (advisorY)" };
    private JComboBox _curvtecSignifComboBox = new JComboBox(_curvtecSignifComboBoxString);
    private JLabel _curvtecSeverityComboBoxLabel = null;
    private String[] _curvtecSeverityComboBoxString = {"0 (N/A)", "1 (Minor)", "2 (Moderate)", 
    		                                           "3 (Major)", "N (None)", "U (Unknown)" };
    private JComboBox _curvtecSeverityComboBox = new JComboBox(_curvtecSeverityComboBoxString);
   
    private JLabel _curvtecICComboBoxLabel = null;
   
    private String[] _curvtecICComboBoxString = {"DM (Dam Fail)","DR (Dam Rel)",
    		                                     "ER (Exc Rain)", "ET (Flow Tidal)",
    		                                     "FS (Surge)", "FT (Fld/Tide)",
    		                                     "GO (Glacial)", "IC (Ice Mix)","IJ (Ice Jam)",
    		                                     "MC (Multiple)", "OT (Other)",
    		                                     "RS (Rain/Snow)", "SM (Snow Melt)",
    		                                     "UU (Unknown)","WT (Wind/Tidal)"};
    		                                                                  
    private JComboBox _curvtecICComboBox = new JComboBox(_curvtecICComboBoxString);
    private JLabel _curvtecRecordComboBoxLabel = null;
    private String[] _curvtecRecordComboBoxString = {"NO (NotExp)", "NR (Near)",
    		                                         "OO (N/A)", "UU (None)"};
    private JComboBox _curvtecRecordComboBox = new JComboBox(_curvtecRecordComboBoxString);
    
    private JLabel _curvtecBeginTimeLabel = null;
    private DateTimeTextField _curvtecBeginTimeTextField = null;
    private JRadioButton _curvtecBeginTimeSetMissingRadioButton = null;
    private JRadioButton _curvtecBeginTimeSetCurrentRadioButton = null;
    private JRadioButton _curvtecBeginTimeRadioButton = null;
    
    private JLabel _curvtecEndTimeLabel = null;
    private DateTimeTextField _curvtecEndTimeTextField = null;
    private JRadioButton _curvtecEndTimeSetMissingRadioButton = null;
    private JRadioButton _curvtecEndTimeSetCurrentRadioButton = null;
    private JRadioButton _curvtecEndTimeRadioButton = null;
    
    private JLabel _curvtecRiseTimeLabel = null;
    private DateTimeTextField _curvtecRiseTimeTextField = null;
    private JCheckBox _curvtecRiseTimeSetMissingCheckBox = null;
    private JLabel _curvtecCrestTimeLabel = null;
    private DateTimeTextField _curvtecCrestTimeTextField = null;
    private JCheckBox _curvtecCrestTimeSetMissingCheckBox = null;
    private JLabel _curvtecCrestValueLabel = null;
    private JTextField _curvtecCrestValueTextField = null;
    private String  _curCrestValueStr = null;
    private JLabel _curvtecFallTimeLabel = null;
    private DateTimeTextField _curvtecFallTimeTextField = null;
    private JCheckBox _curvtecFallTimeSetMissingCheckBox = null;
   // private JButton _curvtecUpdateGraphButton = null;
    private JButton _curvtecSaveChangesButton = null;
    private JButton _curvtecResetButton = null;
       
    // in _prevvtecPanel
    private JLabel     _prevvtecpanelLabel = null;    
    private JLabel     _prevvtecActionLabel = null;
    private JTextField _prevvtecActionTextField = null;
    private JLabel     _prevvtecPhenomLabel = null;
    private JTextField _prevvtecPhenomTextField = null;
    private JLabel     _prevvtecSignifLabel = null;
    private JTextField _prevvtecSignifTextField = null;
    private JLabel     _prevvtecSeverityLabel = null;
    private JTextField _prevvtecSeverityTextField = null;
    private JLabel     _prevvtecICLabel = null;
    private JTextField _prevvtecICTextField = null;
    private JLabel     _prevvtecRecordLabel = null;
    private JTextField _prevvtecRecordTextField = null;
    private JLabel     _prevvtecBeginTimeLabel = null;
    private JTextField _prevvtecBeginTimeTextField = null;
    private JLabel     _prevvtecEndTimeLabel = null;
    private JTextField _prevvtecEndTimeTextField = null;
    private JLabel     _prevvtecRiseTimeLabel = null;
    private JTextField _prevvtecRiseTimeTextField = null;
    private JLabel     _prevvtecRiseTypeLabel = null;
    private JTextField _prevvtecRiseTypeTextField = null;
    private JLabel     _prevvtecCrestTimeLabel = null;
    private JTextField _prevvtecCrestTimeTextField = null;    
    private JLabel     _prevvtecCrestValueLabel = null;
    private JTextField _prevvtecCrestValueTextField = null;
    private JLabel     _prevvtecCrestTypeLabel = null;
    private JTextField _prevvtecCrestTypeTextField = null;
    private JLabel     _prevvtecFallTimeLabel = null;
    private JTextField _prevvtecFallTimeTextField = null;
    private JLabel     _prevvtecFallTypeLabel = null;
    private JTextField _prevvtecFallTypeTextField = null;
    private JLabel     _prevvtecProductTimeLabel = null;
    private JTextField _prevvtecProductTimeTextField = null;
    
    // Booleans related to check boxes and radio button
    private boolean _forceSetRiseTimeMissing = false;
    private boolean _forceSetCrestTimeMissing = false;
    private boolean _forceSetFallTimeMissing = false;
    private boolean _forceSetBeginTimeMissing = false;
    private boolean _forceSetBeginTimeCurrent = false;
    private boolean _forceSetBeginTime = false;
    private boolean _forceSetEndTimeMissing = false;
    private boolean _forceSetEndTimeCurrent = false;
    private boolean _forceSetEndTime = false;
    
    // in _initWindowButtonPanel
    private JButton _closeButton = null;
    
    private FpCurPrevVtec _fpCurPrevVtecData = null;
    
    // for canvas
    private TsPaintableCanvas _canvas = null;
    //private ValueMapper _rightAxisValueMapper = null;
    
    private boolean _exitOnClose = false;    
    private FpVtecInfoDataManager _dataMgr = null;
    
    //define object for FpVtecInfo
    private FpVtecInfo _application;
    
    // keep the original proposed VTEC information
    private String _selected_curAction = null;
    private String _selected_curPhenom = null;
    private String _selected_curSignif_str = null;   
    private String _selected_curSeverity_str = null;  
    private String _selected_curCause_str = null;  
    private String _selected_curRecord_str = null;
    private String _selected_curCrestValue_str = null;
    private long   _selected_curBeginTime = 0;
    private long   _selected_curEndTime = 0;
    private long   _selected_curRiseTime = 0;
    private long   _selected_curCrestTime = 0;
    private long   _selected_curFallTime = 0;
    
    private boolean _graphicalChange = false;
    
    //------------------------------------------------------------------------------
    
    public FpVtecInfoFrame(FpCurPrevVtec fpCurPrevVtecRecord,
    		               FpVtecInfoDataManager dataMgr)
    {     	
        // get the Fp/vtec information from fpcurprevvtecrecord
    	
    	_fpCurPrevVtecData = fpCurPrevVtecRecord;    	
    	_dataMgr = dataMgr;
    	    	
    	initGui();
        
        return;
    }
   
    // ---------------------------------------------------------------------------------
    
    public void setTimeWindow(long startTime, long endTime)
    {
        _canvas.setTimeWindow(startTime, endTime);
        
        return;
    }
    
    //------------------------------------------------------------------------------
    public TsPaintableCanvas getCanvas()
    {
     
        return _canvas;
        
    }
    
    //------------------------------------------------------------------------------
    
    public void setExitOnClose(boolean exitOnClose)
    {
        _exitOnClose = exitOnClose;    
        
        return;
    }
    //------------------------------------------------------------------------------
       
    private void initGui()
    {
        //setAlwaysOnTop(true);
        
        
        // canvas needs to be initialized before the menu bar,
        // because of the screen capturer
        initCanvas(0, 0,
                _initialSize.width, 
                _initialSize.height);
        
        String dbName = _dataMgr.get_db().getDatabaseName();
        String windowTitle = "TimeSeries/Valid Time Event Code Information Window on " + dbName;
        
        this.setTitle(windowTitle);
        
        initMenuBar();
       
        //limit the range of the resizing
        // this object can be local because it remains in memory as a listener
        //WindowResizingManager manager = new WindowResizingManager(this, _minSize, _maxSize);
             
        _tslPanel = new JPanel();
        _tslPanel.setBackground(Color.white);
         
        settslPanelLayout();
                       
        addtslPanelListeners();
       
        // init curvtec info Panel
        initCurVtecPanel();
        
        // init prevvtec info Panel
        initPrevVtecPanel();
        
        // create window control button
        initWindowButtonPanel();
        
        // create the _buttonPanel including _curvtecPanel, _prevvtecPanel and
        // _windowButtonPanel        
        _buttonPanel = new JPanel();        
        
        GridBagConstraints buttonGbc = new GridBagConstraints();
        buttonGbc.fill = GridBagConstraints.BOTH;
                
        buttonGbc.insets = new Insets(2,2,2,2);
        buttonGbc.anchor = GridBagConstraints.NORTHWEST;
		buttonGbc.weightx = 1;
		buttonGbc.weighty = 1;
		_buttonPanel.setPreferredSize(new Dimension(1200,280));
		
		_buttonPanel.setLayout(new GridBagLayout());
        
		addComponent(_buttonPanel, _curvtecPanel, buttonGbc,      0,  0,  1,  3,  1, 0);
        addComponent(_buttonPanel, _prevvtecPanel, buttonGbc,     1,  0,  1,  3,  3, 0);
        addComponent(_buttonPanel, _windowButtonPanel, buttonGbc, 0,  10, 5,  1,  1, 1);
        
        //create main panel including _tslPanel(canvas) and _buttonPanel        
        _mainPanel = new JPanel();
			
        GridBagLayout layoutMgr = new GridBagLayout();
		
		GridBagConstraints mainGbc = new GridBagConstraints();
		mainGbc.fill = GridBagConstraints.BOTH;
		mainGbc.anchor = GridBagConstraints.NORTHWEST;
		mainGbc.weightx = 1;
		mainGbc.weighty = 1;
		//set size for the _mainPanel including the _tslPanel and _buttonPanel 
		_mainPanel.setPreferredSize(new Dimension(1200,920));
		
		_mainPanel.setLayout(layoutMgr);
		 
        // add components to the main panel
		
		//setJMenuBar(_menuBar);
		addComponent(_mainPanel, _tslPanel, mainGbc,    0, 0,  3, 9, 1, 1);
		addComponent(_mainPanel, _buttonPanel, mainGbc, 0, 10, 3, 3,  1, 0);
		
        //add the _mainPanel to the Frame and initialize the frame
		this.getContentPane().add(_mainPanel);
		
        // pack the two panels to a prefererred size and layouts
        this.pack();
       
        return;
    }
    
    //---------------------------------------------------------------------
    private void initCurVtecPanel()
    {
       String toolTipText = null;       
       String selected_curSignif = null;       
       String selected_curSeverity = null;      
       String selected_curCause = null;      
       String selected_curRecord = null;      
       
       Dimension panelDimension = new Dimension(500, 250);
                 
       GridBagConstraints gbc = new GridBagConstraints();
       gbc.fill = GridBagConstraints.BOTH;
       gbc.anchor = GridBagConstraints.NORTHWEST;
       gbc.insets = new Insets(2, 2, 2, 2); 
       gbc.weightx = 1;
       gbc.weighty = 1;

       // get _currentTime;
       
       _currentTime = System.currentTimeMillis();
       
       _curvtecPanel = new JPanel();
       _curvtecPanel.setPreferredSize(panelDimension);

       JPanel panel = _curvtecPanel;
       panel.setLayout(new GridBagLayout());
       panel.setBorder(_panelBorder);

       _curvtecPanelLabel = new JLabel("Proposed VTEC Information:");
       
       // init Action ComboBoxapplication
       _curvtecActionComboBoxLabel = new JLabel("Action:");
       _selected_curAction = _fpCurPrevVtecData.getcurAction();
       _curvtecActionComboBox.setSelectedItem(_selected_curAction);
           
       // init Phenom ComboBoxapplication
       _curvtecPhenomComboBoxLabel = new JLabel("Phenom:");
       _selected_curPhenom = _fpCurPrevVtecData.getcurPhenom();
       _curvtecPhenomComboBox.setSelectedItem(_selected_curPhenom);
       
       // init Signif ComboBoxapplication
       _curvtecSignifComboBoxLabel = new JLabel("    Signif:");
       selected_curSignif = _fpCurPrevVtecData.getcurSignif();
       if (selected_curSignif.equals("A"))
    	   _selected_curSignif_str = "A (wAtch)";
       else if (selected_curSignif.equals("S"))
    	   _selected_curSignif_str = "S (Statement)";
       else if (selected_curSignif.equals("W"))
    	   _selected_curSignif_str = "W (Warning)";
       else if (selected_curSignif.equals("Y"))
    	   _selected_curSignif_str = "Y (advisorY)";
       
       _curvtecSignifComboBox.setSelectedItem(_selected_curSignif_str);
       
       // init Severity ComboBoxapplication
       _curvtecSeverityComboBoxLabel = new JLabel("Severity:");
       selected_curSeverity = _fpCurPrevVtecData.getcurSeverity();
       if (selected_curSeverity.equals("0"))
    	   _selected_curSeverity_str = "0 (N/A)";
       else if (selected_curSeverity.equals("1"))
    	   _selected_curSeverity_str = "1 (Minor)";
       else if (selected_curSeverity.equals("2"))
    	   _selected_curSeverity_str = "2 (Moderate)";
       else if (selected_curSeverity.equals("3"))
    	   _selected_curSeverity_str = "3 (Major)";
       else if (selected_curSeverity.equals("N"))
    	   _selected_curSeverity_str = "N (None)";
       else if (selected_curSeverity.equals("U"))
    	   _selected_curSeverity_str = "U (Unknown)";
       
       _curvtecSeverityComboBox.setSelectedItem(_selected_curSeverity_str);
       
       // init Immediate Cause ComboBox
       _curvtecICComboBoxLabel = new JLabel("Cause:");
       selected_curCause = _fpCurPrevVtecData.getcurCause();
   
       if (selected_curCause.equals("DM"))
    	   _selected_curCause_str = "DM (Dam Fail)";
       if (selected_curCause.equals("DR"))
    	   _selected_curCause_str = "DR (Dam Rel)";
       if (selected_curCause.equals("ER"))
    	   _selected_curCause_str = "ER (Exc Rain)";
       if (selected_curCause.equals("ET"))
    	   _selected_curCause_str = "ET (Flow Tidal)";
       if (selected_curCause.equals("FS"))
    	   _selected_curCause_str = "FS (Surge)";
       if (selected_curCause.equals("FT"))
    	   _selected_curCause_str = "FT (Fld/Tide)";
       if (selected_curCause.equals("GO"))
    	   _selected_curCause_str = "GO (Glacial)";
       if (selected_curCause.equals("IC"))
    	   _selected_curCause_str = "IC (Ice Mix)";
       if (selected_curCause.equals("IJ"))
    	   _selected_curCause_str = "IJ (Ice Jam)";
       if (selected_curCause.equals("MC"))
    	   _selected_curCause_str = "MC (Multiple)";
       if (selected_curCause.equals("OT"))
    	   _selected_curCause_str = "OT (Other)";
       if (selected_curCause.equals("RS"))
    	   _selected_curCause_str = "RS (Rain/Snow)";
       if (selected_curCause.equals("SM"))
    	   _selected_curCause_str = "SM (Snow Melt)";
       if (selected_curCause.equals("UU"))
    	   _selected_curCause_str = "UU (Unknown)";
       if (selected_curCause.equals("WT"))
    	   _selected_curCause_str = "WT (Wind/Tidal)";
       
       _curvtecICComboBox.setSelectedItem(_selected_curCause_str);
       
       // init Record ComboBoxapplication
       _curvtecRecordComboBoxLabel = new JLabel("    Record:");
       selected_curRecord = _fpCurPrevVtecData.getcurRecord();
       if (selected_curRecord.equals("NO"))
    	   _selected_curRecord_str = "NO (NotExp)";
       else if (selected_curRecord.equals("NR"))
    	   _selected_curRecord_str = "NR (Near)";
       else if (selected_curRecord.equals("OO"))
    	   _selected_curRecord_str = "OO (N/A)";
       else if (selected_curRecord.equals("UU"))
    	   _selected_curRecord_str = "UU (None)";
       
       _curvtecRecordComboBox.setSelectedItem(_selected_curRecord_str);             
       
       // init Current VTEC BeginTime textfield
       toolTipText = "Proposed VTEC Event Begin Time. Click on Text Box to edit.";
       _curvtecBeginTimeLabel = new JLabel("Begin Time:");
       _curvtecBeginTimeLabel.setToolTipText(toolTipText);
       // this constructor to get the date/time in minute.
       _curvtecBeginTimeTextField = new DateTimeTextField(getCurVtecBeginTime(), this,
    	                                              "Begin", 30, true);
       _selected_curBeginTime = getCurVtecBeginTime();
       if (_forceSetBeginTimeMissing == true)
       {  
    	   _curvtecBeginTimeTextField.setText("MSG");
    	  
       }
       else if (_forceSetBeginTimeCurrent == true)
       {
    	   _curvtecBeginTimeTextField.setText("CURRENT");
    	  
       }
       
       _curvtecBeginTimeTextField.getDocument().addDocumentListener(new UpdateVtecTimeDocumentListener());
       
      
                         
       // init radio button for set begintime as missing
       _curvtecBeginTimeSetMissingRadioButton = new JRadioButton("Set MSG",
    		                                              _forceSetBeginTimeMissing);   
       _curvtecBeginTimeSetMissingRadioButton.addActionListener(new SelectBeginTimeMissingRadioButtonListener());
       
       // init radio button for set begintime as current
       _curvtecBeginTimeSetCurrentRadioButton = new JRadioButton("Set CURRENT",
    		                                              _forceSetBeginTimeCurrent);
       
       _curvtecBeginTimeSetCurrentRadioButton.addActionListener(new SelectBeginTimeCurrentRadioButtonListener());
       
       // init radio button for set begintime as regular
       _curvtecBeginTimeRadioButton = new JRadioButton("Set Date/Time",
    		                                           _forceSetBeginTime);  
       _curvtecBeginTimeRadioButton.addActionListener(new SelectBeginTimeRadioButtonListener());
       
       // create ButtonGroup
       ButtonGroup curvtecBeginTimeButtonGroup = new ButtonGroup();
       curvtecBeginTimeButtonGroup.add(_curvtecBeginTimeSetMissingRadioButton);
       curvtecBeginTimeButtonGroup.add(_curvtecBeginTimeSetCurrentRadioButton);
       curvtecBeginTimeButtonGroup.add(_curvtecBeginTimeRadioButton);
    
       // init Current VTEC EndTime textfield
       toolTipText = "Proposed VTEC Event End Time. Click on Text Box to edit.";
       _curvtecEndTimeLabel = new JLabel("End Time:");    
       _curvtecEndTimeLabel.setToolTipText(toolTipText);
       _curvtecEndTimeTextField = new DateTimeTextField(getCurVtecEndTime(), this,
    		                                            "End", 30, true);
       _selected_curEndTime = getCurVtecEndTime();      
      
       if (_forceSetEndTimeMissing == true)
       {  
    	   _curvtecEndTimeTextField.setText("MSG");    	  
       }
       else if (_forceSetEndTimeCurrent == true)
       {
    	   _curvtecEndTimeTextField.setText("CURRENT");
    	 
       }
       
       _curvtecEndTimeTextField.getDocument().addDocumentListener(new UpdateVtecTimeDocumentListener());    
       
       
       
       // init radio button for set end time as missing
       _curvtecEndTimeSetMissingRadioButton = new JRadioButton("Set MSG",
    		                                              _forceSetEndTimeMissing);   
       _curvtecEndTimeSetMissingRadioButton.addActionListener(new SelectEndTimeMissingRadioButtonListener());
       
       // init radio button for set end time as current
       _curvtecEndTimeSetCurrentRadioButton = new JRadioButton("Set CURRENT",
    		                                              _forceSetEndTimeCurrent);
       _curvtecEndTimeSetCurrentRadioButton.addActionListener(new SelectEndTimeCurrentRadioButtonListener());
       
       // init radio button for set end time as regular
       _curvtecEndTimeRadioButton = new JRadioButton("Set Date/Time",
    		                                           _forceSetEndTime);  
       
       _curvtecEndTimeRadioButton.addActionListener(new SelectEndTimeRadioButtonListener());
      
       
       // create ButtonGroup
       ButtonGroup curvtecEndTimeButtonGroup = new ButtonGroup();
       curvtecEndTimeButtonGroup.add(_curvtecEndTimeSetMissingRadioButton);
       curvtecEndTimeButtonGroup.add(_curvtecEndTimeSetCurrentRadioButton);
       curvtecEndTimeButtonGroup.add(_curvtecEndTimeRadioButton);
       
       // init Current VTEC event rise above fs time
       toolTipText = "Proposed VTEC Event Rise Above Flood Stage Time. Click on Text Box to edit.";
       _curvtecRiseTimeLabel = new JLabel("AbvFld Time:");
       _curvtecRiseTimeLabel.setToolTipText(toolTipText);              
       _curvtecRiseTimeTextField = new DateTimeTextField(getCurVtecRiseTime(), this,
                                                        "AbvFld", 
                                                        30, true);
       _selected_curRiseTime = getCurVtecRiseTime();
       if (_forceSetRiseTimeMissing == true)
       {
           _curvtecRiseTimeTextField.setText("MSG");	 
       }
     
       _curvtecRiseTimeTextField.getDocument().addDocumentListener(new UpdateVtecTimeDocumentListener());            
       
       // init checkbox for set risetime as missing
       _curvtecRiseTimeSetMissingCheckBox = new JCheckBox("Set MSG",
    		                                              _forceSetRiseTimeMissing);  
       
       _curvtecRiseTimeSetMissingCheckBox.addActionListener(new ChangeRiseTimeMissingCheckBoxListener());
       
       // init Current VTEC event crest time
       toolTipText = "Proposed VTEC Event Crest Time. Click on Text Box to edit.";
       _curvtecCrestTimeLabel = new JLabel("Crest Time:");
       _curvtecCrestTimeLabel.setToolTipText(toolTipText);
       _curvtecCrestTimeTextField = new DateTimeTextField(getCurVtecCrestTime(), this,
                                                          "Crest",
                                                          30, true);
       _selected_curCrestTime = getCurVtecCrestTime();
       if (_forceSetCrestTimeMissing == true)
       {
           _curvtecCrestTimeTextField.setText("MSG");           
       }
       _curvtecCrestTimeTextField.getDocument().addDocumentListener(new UpdateVtecTimeDocumentListener());                           
       
       // init checkbox for set cresttime as missing
       _curvtecCrestTimeSetMissingCheckBox = new JCheckBox("Set MSG",
    		                                              _forceSetCrestTimeMissing);   
       
       _curvtecCrestTimeSetMissingCheckBox.addActionListener(new ChangeCrestTimeMissingCheckBoxListener());
       
       // init Crest Value textfield
       toolTipText = "Proposed VTEC Crest Value. Click on Text Box to edit. Record and Severity" +
                     " fields may automatically change if crest value is changed";
       _curvtecCrestValueLabel = new JLabel("Crest Value:");
       _curvtecCrestValueLabel.setToolTipText(toolTipText);
       _curvtecCrestValueTextField = new JTextField(getCurVtecCrestValue());
       _curCrestValueStr = getCurVtecCrestValue().trim();
       _selected_curCrestValue_str = _curCrestValueStr;
       
       // init Current VTEC event fall time
       toolTipText = "Proposed VTEC Event Fall Below Flood Stage Time. Click on Text Box to edit.";
       _curvtecFallTimeLabel = new JLabel("BlwFld Time:");
       _curvtecFallTimeLabel.setToolTipText(toolTipText);
       _curvtecFallTimeTextField = new DateTimeTextField(getCurVtecFallTime(), this,
                                                        "BlwFld", 
                                                        40, true);
       _selected_curFallTime = getCurVtecFallTime();
       if (_forceSetFallTimeMissing == true)
       {
           _curvtecFallTimeTextField.setText("MSG");        
       }
       _curvtecFallTimeTextField.getDocument().addDocumentListener(new UpdateVtecTimeDocumentListener());
      
       // init checkbox for set falltime as missing
       _curvtecFallTimeSetMissingCheckBox = new JCheckBox("Set MSG",
    		                                              _forceSetFallTimeMissing);   
      
       _curvtecFallTimeSetMissingCheckBox.addActionListener(new ChangeFallTimeMissingCheckBoxListener());             
       
       //JButton _curvtecSaveChangesButton
       _curvtecSaveChangesButton = new JButton("Save Changes");
       _curvtecSaveChangesButton.setEnabled(true);
       _curvtecSaveChangesButton.addActionListener( new SaveChangeListener());
      
       //JButton  _curvtecResetButton
       _curvtecResetButton = new JButton("Reset");
       _curvtecResetButton.setEnabled(true);
       _curvtecResetButton.addActionListener( new ResetListener());
       
       // Separate
       JSeparator separator = new JSeparator(JSeparator.HORIZONTAL);
       
       // add all component to panel
       //                                                     col,  row  numCols numRows  Wcol  wrow        
       
       addComponent(panel, _curvtecPanelLabel,           gbc,  0,    0,   2,      1,       1,   0);
       addComponent(panel, _curvtecActionComboBoxLabel,  gbc,  0,    1,   1,      1,       1,   0);
       addComponent(panel, _curvtecActionComboBox,       gbc,  1,    1,   1,      1,       1,   0);       
       addComponent(panel, _curvtecPhenomComboBoxLabel,  gbc,  2,    1,   1,      1,       1,   0);
       addComponent(panel, _curvtecPhenomComboBox,       gbc,  3,    1,   1,      1,       1,   0);
       addComponent(panel, _curvtecSignifComboBoxLabel,  gbc,  4,    1,   1,      1,       1,   0);
       addComponent(panel, _curvtecSignifComboBox,       gbc,  5,    1,   1,      1,       1,   0);
       

       addComponent(panel, _curvtecSeverityComboBoxLabel,gbc,  0,    2,   1,      1,       1,   0);
       addComponent(panel, _curvtecSeverityComboBox,     gbc,  1,    2,   1,      1,       1,   0);
       addComponent(panel, _curvtecICComboBoxLabel,      gbc,  2,    2,   1,      1,       1,   0);
       addComponent(panel, _curvtecICComboBox,           gbc,  3,    2,   1,      1,       1,   0);
       addComponent(panel, _curvtecRecordComboBoxLabel,  gbc,  4,    2,   1,      1,       1,   0);
       addComponent(panel, _curvtecRecordComboBox,       gbc,  5,    2,   1,      1,       1,   0);
       
       addComponent(panel, _curvtecBeginTimeLabel,       gbc,  0,    3,   1,      1,       1,   0);
       addComponent(panel, _curvtecBeginTimeTextField,   gbc,  1,    3,   2,      1,       1,   0);
       addComponent(panel, _curvtecBeginTimeSetMissingRadioButton,gbc,3,3,1,      1,       1,   0);
       addComponent(panel, _curvtecBeginTimeSetCurrentRadioButton,gbc,4,3,1,      1,       1,   0);
       addComponent(panel, _curvtecBeginTimeRadioButton, gbc,  5,    3,   1,      1,       1,   0);
       
       addComponent(panel, _curvtecEndTimeLabel,         gbc,  0,    4,   1,      1,       1,   0);
       addComponent(panel, _curvtecEndTimeTextField,     gbc,  1,    4,   2,      1,       1,   0);
       addComponent(panel, _curvtecEndTimeSetMissingRadioButton,gbc,3,4,  1,      1,       1,   0);
       addComponent(panel, _curvtecEndTimeSetCurrentRadioButton,gbc,4,4,  1,      1,       1,   0);
       addComponent(panel, _curvtecEndTimeRadioButton,   gbc,  5,    4,   1,      1,       1,   0);
       
       addComponent(panel, _curvtecRiseTimeLabel,        gbc,  0,    5,   1,      1,       1,   0);
       addComponent(panel, _curvtecRiseTimeTextField,    gbc,  1,    5,   2,      1,       1,   0);
       addComponent(panel, _curvtecRiseTimeSetMissingCheckBox, gbc, 3, 5, 1,      1,       1,   0);
       
       addComponent(panel, _curvtecCrestTimeLabel,       gbc,  0,    6,   1,      1,       1,   0);
       addComponent(panel, _curvtecCrestTimeTextField,   gbc,  1,    6,   2,      1,       1,   0);
       addComponent(panel, _curvtecCrestTimeSetMissingCheckBox, gbc, 3, 6,1,      1,       1,   0);
       
       addComponent(panel, _curvtecCrestValueLabel,      gbc,  4,    6,   1,      1,       1,   0);
       addComponent(panel, _curvtecCrestValueTextField,  gbc,  5,    6,   1,      1,       1,   0);
       
       addComponent(panel, _curvtecFallTimeLabel,        gbc,  0,    7,   1,      1,       1,   0);
       addComponent(panel, _curvtecFallTimeTextField,    gbc,  1,    7,   2,      1,       1,   0);
       addComponent(panel, _curvtecFallTimeSetMissingCheckBox, gbc, 3, 7, 1,      1,       1,   0);
       addComponent(panel, separator,                    gbc,  0,    8,   6,      1,       1,   0);
       //addComponent(panel, _curvtecUpdateGraphButton,    gbc,  0,    9,   2,      1,       1,   0);
       addComponent(panel, _curvtecResetButton,          gbc,  1,    9,   2,      1,       1,   0);       
       addComponent(panel, _curvtecSaveChangesButton,    gbc,  3,    9,   2,      1,       1,   0);
       
       return;
       
    }
    
    //---------------------------------------------------------------------
    private void initPrevVtecPanel()
    {
       Dimension panelDimension = new Dimension(550, 250);
     
    
       GridBagConstraints gbc = new GridBagConstraints();
       gbc.fill = GridBagConstraints.BOTH;
       gbc.anchor = GridBagConstraints.NORTHWEST;
       gbc.insets = new Insets(4, 4, 4, 4); 
       gbc.weightx = 1;
       gbc.weighty = 1;

       _prevvtecPanel = new JPanel();
       _prevvtecPanel.setPreferredSize(panelDimension);

       JPanel panel = _prevvtecPanel;
       panel.setLayout(new GridBagLayout());
       panel.setBorder(_panelBorder);

       // Just display previous event's VTEC info, not allow customize
       _prevvtecpanelLabel = new JLabel("Previous VTEC Information:");       
       
       _prevvtecActionLabel = new JLabel("Action:");
       _prevvtecActionTextField = new JTextField(getPrevVtecAction());
       _prevvtecActionTextField.setEditable(false);
       
       _prevvtecICLabel = new JLabel("Cause:");      
       _prevvtecICTextField = new JTextField(getPrevVtecIC());
       _prevvtecICTextField.setEditable(false);
       
       _prevvtecRecordLabel = new JLabel("Record:");
       _prevvtecRecordTextField = new JTextField(getPrevVtecRecord());
       _prevvtecRecordTextField.setEditable(false);
       
       _prevvtecPhenomLabel = new JLabel("Phenom:");
       _prevvtecPhenomTextField = new JTextField(getPrevVtecPhenom());
       _prevvtecPhenomTextField.setEditable(false);
       
       _prevvtecSignifLabel = new JLabel("Signif:");
       _prevvtecSignifTextField = new JTextField(getPrevVtecSignif());
       _prevvtecSignifTextField.setEditable(false);
       
       _prevvtecSeverityLabel = new JLabel("Severity:");
       _prevvtecSeverityTextField = new JTextField(getPrevVtecSeverity());
       _prevvtecSeverityTextField.setEditable(false);
       
       // init previous VTEC BeginTime textfield
       _prevvtecBeginTimeLabel = new JLabel("Begin Time:");
       _prevvtecBeginTimeTextField = new JTextField(getPrevVtecBeginTime());
       _prevvtecBeginTimeTextField.setEditable(false);
       
       // init previous VTEC EndTime textfield
       _prevvtecEndTimeLabel = new JLabel("End Time:");
       _prevvtecEndTimeTextField = new JTextField(getPrevVtecEndTime());
       _prevvtecEndTimeTextField.setEditable(false);
       
       // init pervious VTEC event rise above fs time
       _prevvtecRiseTimeLabel = new JLabel("AbvFld Time:");
       _prevvtecRiseTimeTextField = new JTextField(getPrevVtecRiseTime());
       _prevvtecRiseTimeTextField.setEditable(false);
       
       // init previous VTEC event rise type
       _prevvtecRiseTypeLabel = new JLabel("AbvFld Type:");
       _prevvtecRiseTypeTextField = new JTextField(getPrevVtecRiseType());
       _prevvtecRiseTypeTextField.setEditable(false);
              
       // init previous VTEC event crest time
       _prevvtecCrestTimeLabel = new JLabel("Crest Time:");
       _prevvtecCrestTimeTextField = new JTextField(getPrevVtecCrestTime());
       _prevvtecCrestTimeTextField.setEditable(false);
       
       // init previous VTEC event crest value
       _prevvtecCrestValueLabel = new JLabel("Crest Value:");
       _prevvtecCrestValueTextField = new JTextField(getPrevVtecCrestValue());      
       _prevvtecCrestValueTextField.setEditable(false);
       
       // init previous VTEC event crest type
       _prevvtecCrestTypeLabel = new JLabel("Crest Type:");
       _prevvtecCrestTypeTextField = new JTextField(getPrevVtecCrestType());
       _prevvtecCrestTypeTextField.setEditable(false);
       
       // init previous VTEC event fall time
       _prevvtecFallTimeLabel = new JLabel("BlwFld Time:");
       _prevvtecFallTimeTextField = new JTextField(getPrevVtecFallTime());
       _prevvtecFallTimeTextField.setEditable(false);
       
       // init previous VTEC event fall type
       _prevvtecFallTypeLabel = new JLabel("BlwFld Type:");
       _prevvtecFallTypeTextField = new JTextField(getPrevVtecFallType());
       _prevvtecFallTypeTextField.setEditable(false);
       
       //init previous VTEC event product time
       _prevvtecProductTimeLabel = new JLabel("Product Time:");
       _prevvtecProductTimeTextField = new JTextField(getPrevVtecProductTime());
       _prevvtecProductTimeTextField.setEditable(false);
       
     
       // add components into panel
       //                                                     col,  row  numCols numRows  Wcol  wrow 
       addComponent(panel, _prevvtecpanelLabel,         gbc,  0,    0,    2,      1,       1,   0);
       addComponent(panel, _prevvtecActionLabel,        gbc,  0,    1,    1,      1,       1,   0);
       addComponent(panel, _prevvtecActionTextField,    gbc,  1,    1,    1,      1,       1,   0);
       addComponent(panel, _prevvtecPhenomLabel,        gbc,  2,    1,    1,      1,       1,   0);
       addComponent(panel, _prevvtecPhenomTextField,    gbc,  3,    1,    1,      1,       1,   0);
       addComponent(panel, _prevvtecSignifLabel,        gbc,  4,    1,    1,      1,       1,   0);
       addComponent(panel, _prevvtecSignifTextField,    gbc,  5,    1,    1,      1,       1,   0);
       
       addComponent(panel, _prevvtecSeverityLabel,      gbc,  0,    2,    1,      1,       1,   0);
       addComponent(panel, _prevvtecSeverityTextField,  gbc,  1,    2,    1,      1,       1,   0);
       addComponent(panel, _prevvtecICLabel,            gbc,  2,    2,    1,      1,       1,   0);
       addComponent(panel, _prevvtecICTextField,        gbc,  3,    2,    1,      1,       1,   0);
       addComponent(panel, _prevvtecRecordLabel,        gbc,  4,    2,    1,      1,       1,   0);
       addComponent(panel, _prevvtecRecordTextField,    gbc,  5,    2,    1,      1,       1,   0);
       
       
       addComponent(panel, _prevvtecBeginTimeLabel,     gbc,  0,    3,    1,      1,       1,   0);
       addComponent(panel, _prevvtecBeginTimeTextField, gbc,  1,    3,    3,      1,       1,   0);
       addComponent(panel, _prevvtecEndTimeLabel,       gbc,  0,    4,    1,      1,       1,   0);
       addComponent(panel, _prevvtecEndTimeTextField,   gbc,  1,    4,    3,      1,       1,   0);
       addComponent(panel, _prevvtecRiseTimeLabel,      gbc,  0,    5,    1,      1,       1,   0);
       addComponent(panel, _prevvtecRiseTimeTextField,  gbc,  1,    5,    3,      1,       1,   0);
       addComponent(panel, _prevvtecRiseTypeLabel,      gbc,  4,    5,    1,      1,       1,   0);
       addComponent(panel, _prevvtecRiseTypeTextField,  gbc,  5,    5,    1,      1,       1,   0);
       addComponent(panel, _prevvtecCrestTimeLabel,     gbc,  0,    6,    1,      1,       1,   0);
       addComponent(panel, _prevvtecCrestTimeTextField, gbc,  1,    6,    3,      1,       1,   0);
       addComponent(panel, _prevvtecCrestTypeLabel,    gbc,  4,    6,    1,      1,       1,   0);
       addComponent(panel, _prevvtecCrestTypeTextField,gbc,  5,    6,    1,      1,       1,   0);
       addComponent(panel, _prevvtecCrestValueLabel,     gbc,  6,    6,    1,      1,       1,   0);
       addComponent(panel, _prevvtecCrestValueTextField, gbc,  7,    6,    1,      1,       1,   0);
       addComponent(panel, _prevvtecFallTimeLabel,      gbc,  0,    7,    1,      1,       1,   0);
       addComponent(panel, _prevvtecFallTimeTextField,  gbc,  1,    7,    3,      1,       1,   0);
       addComponent(panel, _prevvtecFallTypeLabel,      gbc,  4,    7,    1,      1,       1,   0);
       addComponent(panel, _prevvtecFallTypeTextField,  gbc,  5,    7,    1,      1,       1,   0);
       addComponent(panel, _prevvtecProductTimeLabel,   gbc,  0,    8,    1,      1,       1,   0);
       addComponent(panel, _prevvtecProductTimeTextField,gbc, 1,    8,    3,      1,       1,   0);
    
       return;
       
    }
    
    //---------------------------------------------------------------------
    private void initWindowButtonPanel()
    {
       Dimension panelDimension = new Dimension(980, 100);
     
       GridBagConstraints gbc = new GridBagConstraints();
       gbc.fill = GridBagConstraints.BOTH;
       gbc.anchor = GridBagConstraints.CENTER;
       gbc.insets = new Insets(2, 2, 2, 2); 
       gbc.weightx = 1;
       gbc.weighty = 1;

       _windowButtonPanel = new JPanel();
       _windowButtonPanel.setPreferredSize(panelDimension);

       JPanel panel = _windowButtonPanel;
       panel.setLayout(new GridBagLayout());
       panel.setBorder(_panelBorder);

       _closeButton = new JButton("Close");
       _closeButton.addActionListener(new CloseListener());
       
       //     add components into panel
       //                                       col,  row  numCols numRows Wcol wrow 
       addComponent(panel, _closeButton,   gbc,  0,    0,  0,      0,       0,   0);
       
       return;
       
    }
    //-----------------------------------------------------------------
    private void settslPanelLayout()
    {
        _tslPanel.setLayout(new GridBagLayout());
        GridBagConstraints mainPanelGbc = new GridBagConstraints();
        mainPanelGbc.fill = GridBagConstraints.BOTH;
        
        if (_canvas == null)
        {
            System.out.println("Canvas is null");    
        }
        
        //      add components into panel
        //                                                 col,  row  numCols numRows  Wcol  wrow 
        addComponent(_tslPanel,  _canvas,  mainPanelGbc,    0,    0,   1,      1,       1,   1); 
 
        return;
    }
       
    //-----------------------------------------------------------------
    private void addComponent(Container container,
                              Component component,
                              GridBagConstraints gbc,
                              int column, int row,
                              int columnCells, int rowCells,
                              int weightX, int weightY)
    {

        // how much it can grow in the X and Y directions   
        gbc.weightx = weightX;
        gbc.weighty  = weightY;

        // what row and column it starts in
        gbc.gridx = column;
        gbc.gridy = row;

        //the number of columns and rows it takes up
        gbc.gridwidth = columnCells;
        gbc.gridheight = rowCells;
        
        container.add(component, gbc);
        
        return;
    } 
    
    //-----------------------------------------------------------------
    
    
    private void initMenuBar()
    {
         _menuBar = new JMenuBar();
         
         // for screencapture button
         ScreenCaptureActionListener screenCaptureActionListener = 
                     new  ScreenCaptureActionListener(this, FpVtecInfoFrame.this);
         
          
        JMenu menu = null;
        JMenuItem menuItem = null;
  
        //File Menu
        menu = new JMenu("File");
        menu.setMnemonic(KeyEvent.VK_F);
        menu.getAccessibleContext().setAccessibleDescription(
                         "Access File Menus");
        _menuBar.add(menu);
        
            
        menuItem = new JMenuItem("Close Window");
        menuItem.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_C, ActionEvent.ALT_MASK));
        menuItem.getAccessibleContext().setAccessibleDescription(
                "Close this Window.");
        menuItem.addActionListener(new CloseListener());
        menu.add(menuItem); 
        
        
        // create a button directly on the menuBar
        
        Color menuBarColor = _menuBar.getBackground();
        Border invisibleBorder = BorderFactory.createLineBorder(menuBarColor);

        JButton saveButton = new JButton("   Save Screen  ");
        saveButton.addActionListener(screenCaptureActionListener);
        saveButton.setBorder(invisibleBorder);
        saveButton.setSize(60, 30);
        String saveButtonTooltipText = "Capture the screen to a file.";
        saveButton.setToolTipText(saveButtonTooltipText);
        _menuBar.add(saveButton);
        
       
        HelpActionListener helpActionListener = new HelpActionListener();
        JButton helpButton = new JButton("Help");
        helpButton.addActionListener(helpActionListener);
        helpButton.setBorder(invisibleBorder);
        helpButton.setSize(60, 30);
        _menuBar.add(helpButton);       
        
        
        //add to the Frame
          setJMenuBar(_menuBar);
        
        return;
    }
   
    //-----------------------------------------------------------------  
      
    private void addtslPanelListeners()
    {
        
        //allow the frame to close when the user presses the close-box
        addWindowListener(new FrameCloseWindowListener());
        
    }
    //-----------------------------------------------------------------\
    
    private void close()
    {       
        if (_exitOnClose)
        {
            FpVtecInfoFrame.this.setVisible(false);
            
            dispose();
            
        	//System.exit(0); comment out, just close the SeriesTime/VTEC window, not
        	// main application
        }
    }
    
    private void closeWithoutSaving()
    {
              
    	_lastUpdateTime = CLOSED_BUT_NOT_SAVED;
    	
        if (_exitOnClose)
        {
            FpVtecInfoFrame.this.setVisible(false);
            dispose();
            
        	//System.exit(0); comment out, just close the SeriesTime/VTEC window, not
        	// main application
        }
    }
    
    //-----------------------------------------------------------------
    
    private class CloseListener implements ActionListener
    { 
        public void actionPerformed(ActionEvent evt)
        {
            closeWithoutSaving();
        }
    }
    
    //-----------------------------------------------------------------

    
    private class FrameCloseWindowListener extends WindowAdapter
    {
        public void windowClosing(WindowEvent evt)
        {
            closeWithoutSaving();
        }

    }      
    
    public void showMessage()
    {
    	long obsCutOffTime = _application.getStartTime();
    	long fcstCutOffTime = _application.getEndTime();
    	String obsCutOffTimeString = "";
    	String fcstCutOffTimeString = "";
    	
    	if (obsCutOffTime > 0 && fcstCutOffTime > 0)
    	{
    		 obsCutOffTimeString = DbTimeHelper.getDateTimeStringFromLongTime(obsCutOffTime);
    		 fcstCutOffTimeString = DbTimeHelper.getDateTimeStringFromLongTime(fcstCutOffTime);
    	}
    	
    	String message = "The YELLOW time series is observed data.\n" +
         "The GREEN time series is forecast data. \n" +
         "The vertical WHITE line with 'N' is current time.\n" +
         "The vertical BLUE line with 'B' is proposed vtec event's begin time.\n" +
         "The vertical GREEN line with 'E' is proposed vtec event's end time. \n" +
         "The vertical ORANGE line with 'R' is proposed vtec event's rise above flood level time.\n" +
         "The vertical RED line with 'C' is proposed vtec event's crest time.\n" + 
         "The vertical MEGENTA line with 'F' is proposed vtec event's fall below flood level time.\n" +
         "The horizontal YELLOW line is action level.\n" +
         "The horizontal ORANGE line is flood level.\n" +
         "The horizontal RED line is moderate flood level. \n" +
         "The horizontal MEGENTA line is major flood level. \n" +
         "The timeseries window starts from " + obsCutOffTimeString + "Z to " +
         fcstCutOffTimeString + "Z";         

        DialogHelper.displayMessageDialog(this, message, "TimeSeries/VTEC Help");
    }
    // ------------------------------------------------------------------------------------
    
    private class HelpActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
           showMessage();
        }

    }  
    
    // -------------------------------------------------------------------------------------
    // action for the button "Update  "
    /*private class UpdateGraphListener implements ActionListener
    {
    	public void actionPerformed(ActionEvent evt)
    	{   
    		
    		updateGraph();
    		return;
    	}
    } */
    // ------------------------------------------------------------------------------------
    public void updateGraph()
    {
    	// get the time from time text fields, set them to
		//_fpCurPrevVtecData, and TimeHolder
		    	
    	long currentTime = System.currentTimeMillis();
    			
    	// for the begintime
    	if (_curvtecBeginTimeTextField.getText().equals("MSG"))
    	{	
		   _fpCurPrevVtecData.setcurBeginTime(MISSING_INT);
		   _application.getCurVtecBeginTimeHolder().setTime(MISSING_INT);
    	}
    	else if (_curvtecBeginTimeTextField.getText().equals("CURRENT"))
    	{
    		_fpCurPrevVtecData.setcurBeginTime(-1);
    		_application.getCurVtecBeginTimeHolder().setTime(currentTime);
    	}
    	else
    	{                	
    		_fpCurPrevVtecData.setcurBeginTime(_curvtecBeginTimeTextField.getTime()/MILLIS_PER_SECOND);
    		_application.getCurVtecBeginTimeHolder().setTime(_curvtecBeginTimeTextField.getTime());    		    	
    	}
			
		
		// for the end time
    	if (_curvtecEndTimeTextField.getText().equals("MSG"))
    	{	
		   _fpCurPrevVtecData.setcurEndTime(MISSING_INT);
		   _application.getCurVtecEndTimeHolder().setTime(MISSING_INT);
    	}
    	else if (_curvtecEndTimeTextField.getText().equals("CURRENT"))
    	{
    		_fpCurPrevVtecData.setcurEndTime(-1);
    		_application.getCurVtecEndTimeHolder().setTime(currentTime);
    	}
    	else
    	{
    		_fpCurPrevVtecData.setcurEndTime(_curvtecEndTimeTextField.getTime()/MILLIS_PER_SECOND);
    		_application.getCurVtecEndTimeHolder().setTime(_curvtecEndTimeTextField.getTime());    		
    	}
    	
		// for the rise time
    	if (_curvtecRiseTimeTextField.getText().equals("MSG"))
    	{	
		   _fpCurPrevVtecData.setcurRiseTime(MISSING_INT);
		   _application.getCurVtecRiseTimeHolder().setTime(MISSING_INT);
    	}    	
    	else
    	{
    		_fpCurPrevVtecData.setcurRiseTime(_curvtecRiseTimeTextField.getTime()/MILLIS_PER_SECOND);
    		_application.getCurVtecRiseTimeHolder().setTime(_curvtecRiseTimeTextField.getTime());              	
    	}    	    	
    	
        // for the crest time
    	if (_curvtecCrestTimeTextField.getText().equals("MSG"))
    	{	
		   _fpCurPrevVtecData.setcurCrestTime(MISSING_INT);
		   _application.getCurVtecCrestTimeHolder().setTime(MISSING_INT);
    	}    	
    	else
    	{
    		_fpCurPrevVtecData.setcurCrestTime(_curvtecCrestTimeTextField.getTime()/MILLIS_PER_SECOND);
    		_application.getCurVtecCrestTimeHolder().setTime(_curvtecCrestTimeTextField.getTime());    		               		
    	}		    
    	
        // for the fall time
    	if (_curvtecFallTimeTextField.getText().equals("MSG"))
    	{	
		   _fpCurPrevVtecData.setcurFallTime(MISSING_INT);
		   _application.getCurVtecFallTimeHolder().setTime(MISSING_INT);
    	}    	
    	else
    	{
    		_fpCurPrevVtecData.setcurFallTime(_curvtecFallTimeTextField.getTime()/MILLIS_PER_SECOND);
    		_application.getCurVtecFallTimeHolder().setTime(_curvtecFallTimeTextField.getTime());    		                	
    	}				
    	
		// repaint the canvas    		
		_application.redrawCanvas();    		              
	    
    }
    
   
    //  -----------------------------------------------------------------------
    public void updateCurDateTimeTextField()
    {
    	long new_curVTECBeginTime = 0;
    	long new_curVTECEndTime = 0;
    	long new_curVTECRiseTime = 0;
    	long new_curVTECCrestTime = 0;
    	long new_curVTECFallTime = 0;
    	
    	_graphicalChange = true;    	   
    	
    	new_curVTECBeginTime = (_application.getCurVtecBeginTimeHolder()).getTime(); 
    	if (new_curVTECBeginTime == MISSING_INT) 
    	{    		
    	    _curvtecBeginTimeSetMissingRadioButton.setSelected(true);
    	    _curvtecBeginTimeTextField.setText("MSG");
    	}  
    	else if (_curvtecBeginTimeTextField.getText().equals("CURRENT") &&
                (_dragged_line != TimeLineAdjustmentListener.BEGIN_DRAGGED))
    	{
    	    _curvtecBeginTimeSetCurrentRadioButton.setSelected(true);	
    	    _curvtecBeginTimeTextField.setText("CURRENT");
    	}  
    	else
    	{
    	    _curvtecBeginTimeRadioButton.setSelected(true);	
    	    _curvtecBeginTimeTextField.setTime(new_curVTECBeginTime);
    	}
    	
    	new_curVTECEndTime = (_application.getCurVtecEndTimeHolder()).getTime();  
    	if (new_curVTECEndTime == MISSING_INT) 
    	{
    	    _curvtecEndTimeSetMissingRadioButton.setSelected(true);
    	    _curvtecEndTimeTextField.setText("MSG");
    	}  
    	else if (_curvtecEndTimeTextField.getText().equals("CURRENT") &&
    			(_dragged_line != TimeLineAdjustmentListener.END_DRAGGED ))
    	{
    	    _curvtecEndTimeSetCurrentRadioButton.setSelected(true);	
    	    _curvtecEndTimeTextField.setText("CURRENT");
    	}  
    	else
    	{
    	    _curvtecEndTimeRadioButton.setSelected(true);
    	    _curvtecEndTimeTextField.setTime(new_curVTECEndTime);    	
    	}
    	
    	new_curVTECRiseTime = (_application.getCurVtecRiseTimeHolder()).getTime();
    	if (new_curVTECRiseTime == MISSING_INT) 
    	{
    		_curvtecRiseTimeSetMissingCheckBox.setSelected(true);
    	    _curvtecRiseTimeTextField.setText("MSG");
    	}      	
    	else
    	{
    		_curvtecRiseTimeSetMissingCheckBox.setSelected(false);    	
    	    _curvtecRiseTimeTextField.setTime(new_curVTECRiseTime);
    	}
    	
    	new_curVTECCrestTime = (_application.getCurVtecCrestTimeHolder()).getTime();    	
    	if (new_curVTECCrestTime == MISSING_INT) 
    	{
    		_curvtecCrestTimeSetMissingCheckBox.setSelected(true);
    	    _curvtecCrestTimeTextField.setText("MSG");
    	}      	
    	else
    	{
    		_curvtecCrestTimeSetMissingCheckBox.setSelected(false);   
    	    _curvtecCrestTimeTextField.setTime(new_curVTECCrestTime);
    	}
    	
    	new_curVTECFallTime = (_application.getCurVtecFallTimeHolder()).getTime();    	
    	if (new_curVTECFallTime == MISSING_INT) 
    	{
    		_curvtecFallTimeSetMissingCheckBox.setSelected(true);
    	    _curvtecFallTimeTextField.setText("MSG");
    	}      	
    	else
    	{
    		_curvtecFallTimeSetMissingCheckBox.setSelected(false);       	
    	    _curvtecFallTimeTextField.setTime(new_curVTECFallTime);
    	}
    	
      	_graphicalChange = false;
    }	
    
    public void setDraggedLine(long dragged_line)
    {
    	_dragged_line = dragged_line;
    	
    }
    public void resetCurVtecPanel()
    {      	
        _curvtecActionComboBox.setSelectedItem(_selected_curAction);    	        
        _curvtecPhenomComboBox.setSelectedItem(_selected_curPhenom);
        _curvtecSignifComboBox.setSelectedItem(_selected_curSignif_str);    	        
        _curvtecSeverityComboBox.setSelectedItem(_selected_curSeverity_str);
        _curvtecICComboBox.setSelectedItem(_selected_curCause_str);    	        
        _curvtecRecordComboBox.setSelectedItem(_selected_curRecord_str);
        
        // reset to original event begin time
        
        if (_selected_curBeginTime == MISSING_INT)
        {	
        	_curvtecBeginTimeSetMissingRadioButton.setSelected(true);
        	_curvtecBeginTimeTextField.setText("MSG");
        }	
		else if (_selected_curBeginTime == -1)
		{	
			_curvtecBeginTimeSetCurrentRadioButton.setSelected(true);
			_curvtecBeginTimeTextField.setText("CURRENT");
		}	
		else
		{
			_curvtecBeginTimeRadioButton.setSelected(true);
			_curvtecBeginTimeTextField.setTime(_selected_curBeginTime);
		}
        
        // reset to original event end time
        
        if (_selected_curEndTime == MISSING_INT)
        {	
        	_curvtecEndTimeSetMissingRadioButton.setSelected(true);
        	_curvtecEndTimeTextField.setText("MSG");
        }	
		else if (_selected_curEndTime == -1)
		{	
			_curvtecEndTimeSetCurrentRadioButton.setSelected(true);
			_curvtecEndTimeTextField.setText("CURRENT");
		}	
		else
		{
			_curvtecEndTimeRadioButton.setSelected(true);
			_curvtecEndTimeTextField.setTime(_selected_curEndTime);
		}
        
        // reset to original event rise above flood level time
        
        if (_selected_curRiseTime == MISSING_INT)
        {	        	
        	_curvtecRiseTimeSetMissingCheckBox.setSelected(true);
        	_curvtecRiseTimeTextField.setText("MSG");
        }			
		else
		{
			_curvtecRiseTimeSetMissingCheckBox.setSelected(false);
			_curvtecRiseTimeTextField.setTime(_selected_curRiseTime);
		}
        
       // reset to original event crest above flood level time
        
        if (_selected_curCrestTime == MISSING_INT)
        {	        	
        	_curvtecCrestTimeSetMissingCheckBox.setSelected(true);
        	_curvtecCrestTimeTextField.setText("MSG");
        }			
		else
		{
			_curvtecCrestTimeSetMissingCheckBox.setSelected(false);
			_curvtecCrestTimeTextField.setTime(_selected_curCrestTime);
		}
        
        // reset to original event fall below flood level time
        
        if (_selected_curFallTime == MISSING_INT)
        {	        	
        	_curvtecFallTimeSetMissingCheckBox.setSelected(true);
        	_curvtecFallTimeTextField.setText("MSG");
        }			
		else
		{
			_curvtecFallTimeSetMissingCheckBox.setSelected(false);
			_curvtecFallTimeTextField.setTime(_selected_curFallTime);
		}
		
		
        
        // reset to original crest value
        _curvtecCrestValueTextField.setText(_selected_curCrestValue_str);
      
        
        updateGraph();
    	
    }
    
    /*public void resetCurVtecTime()
    {
    	
    }
    */
    // ----------------------------------------------------------------------------------------
    private class ResetListener implements ActionListener
    {
    	public void actionPerformed(ActionEvent evt)
    	{ 
    		
    	   FpVtecInfoFrame.this.resetCurVtecPanel(); 
    	}
    }
    // -----------------------------------------------------------------------------------------
    private class SaveChangeListener implements ActionListener
    {
    	public void actionPerformed(ActionEvent evt)
    	{   
    		String header = "SaveChangeListener.actionPerformed():";
    		Double curCrestValue = -9999.0;
    		Boolean curCrestValueEdit = false;
    		StringBuffer outputStringBuffer = new StringBuffer();
    		
    		 if ( DialogHelper.displayConfirmDialog( FpVtecInfoFrame.this, "Are you sure you want to save the edits (will update the VTEC Settings window),  " +
    				                                "and close the TimeSeries/VTEC window?", "Save Changes and Close" ) )
    		 {	 
                   // update file curvteceditfile.dat
    	    		String outputfilePath = FpVtecInfoFrame.this.get_dataMgr().get_rpfdataFilePath();
    	    		
    	    		String fpvtecinfoOutputFileName = outputfilePath + "curvteceditsaved.dat" ;
    	    		    	    		
    	    		openOutputFile( fpvtecinfoOutputFileName);
    	    		
    	    		long curTime = System.currentTimeMillis();
    	    		outputStringBuffer.append( "The output file with edits for proposed VTEC information is created on:"
    	    				                    + DbTimeHelper.getDateTimeStringFromLongTime(curTime) + "\n" );
    	    		outputStringBuffer.append("###lid,locname,action,phenom,signif,severity,cause,record,etn,crest_value," +
    	    				                  "begintime,endtime,risetime,cresttime,falltime###" + "\n");
    	    		
    	    		String lid = _fpCurPrevVtecData.getLocId();
    	    		String locName = _fpCurPrevVtecData.getLocName();
    	    		String curAction = (String) _curvtecActionComboBox.getSelectedItem();
    	           
    	    		String curPhenom = (String) _curvtecPhenomComboBox.getSelectedItem();
    	    		
    	    		String curSignif = (String) _curvtecSignifComboBox.getSelectedItem();
    	    		String sub_curSignif = curSignif.substring(0, 1);    	   
    	    		
    	    		String curSeverity = (String) _curvtecSeverityComboBox.getSelectedItem();
    	    		String sub_curSeverity = curSeverity.substring(0, 1);
    	    		
    	    		
    	            String curCause = (String)_curvtecICComboBox.getSelectedItem(); 
    	            String sub_curCause = curCause.substring(0, 2);
    	            
    	            String curRecord = (String)_curvtecRecordComboBox.getSelectedItem();
    	            String sub_curRecord = curRecord.substring(0, 2);
    	            
    	            int    curEtn = _fpCurPrevVtecData.getcurEtn();   
    	            String curCrestString = _curvtecCrestValueTextField.getText().trim();
    	            
    	            if (curCrestString.equals(_curCrestValueStr))
    	            	curCrestValueEdit = false;
    	            else
    	            	curCrestValueEdit = true;
    	            
    	            _curCrestValueStr = curCrestString;
    	            
    	            if (curCrestString == null || curCrestString.equals(""))
    	            {
    	            	DialogHelper.displayErrorDialog( FpVtecInfoFrame.this, "No crest value entered", "Unable to save" );
    	            }
    	            else
    	                curCrestValue = Double.valueOf(curCrestString);
    	            
    	            String curBeginTime = _curvtecBeginTimeTextField.getText().trim();
    	            String curEndTime = _curvtecEndTimeTextField.getText().trim();
    	            String curRiseTime = _curvtecRiseTimeTextField.getText().trim();
    	            String curCrestTime = _curvtecCrestTimeTextField.getText().trim();
    	            String curFallTime = _curvtecFallTimeTextField.getText().trim();
    	            
    	            /*String curBeginTime = DbTimeHelper.getDateTimeStringFromLongTime(_application.getCurVtecBeginTimeHolder().getTime()).trim();
    	            String curEndTime = DbTimeHelper.getDateTimeStringFromLongTime(_application.getCurVtecEndTimeHolder().getTime()).trim();
    	            String curRiseTime = DbTimeHelper.getDateTimeStringFromLongTime(_application.getCurVtecRiseTimeHolder().getTime()).trim();
    	            String curCrestTime = DbTimeHelper.getDateTimeStringFromLongTime(_application.getCurVtecCrestTimeHolder().getTime()).trim();
    	            String curFallTime = DbTimeHelper.getDateTimeStringFromLongTime(_application.getCurVtecFallTimeHolder().getTime()).trim();
    	            */
    	            
    	            
    	            if (curBeginTime == null || curEndTime == null || curRiseTime == null 
    	            		|| curCrestTime == null || curFallTime == null)
    	            {
    	            	DialogHelper.displayErrorDialog( FpVtecInfoFrame.this, "No time entered", "Unable to save" );
    	            }
    	            	
    	           
    	    		outputStringBuffer.append(lid + "," + locName + "," + curAction + "," +
    	    				                  curPhenom + "," + sub_curSignif + "," + sub_curSeverity +
    	    				                  "," + sub_curCause + "," + sub_curRecord + "," + curEtn + 
    	    				                  "," + curCrestValue + "," + curCrestValueEdit + "," + curBeginTime + "," +
    	    				                  curEndTime +"," + curRiseTime +"," + curCrestTime +
    	    				                  "," + curFallTime);
    	    		
    	    		_writer.println( outputStringBuffer);
    	    		_writer.flush();
    	    		
    	    		// update the outputfile timestamp
    	    		setLastUpdateTimeToCurrentTime();
    	    		System.out.println(header + "_lastUpdateTime = " + FpVtecInfoFrame.getLastUpdateTime());
    	    		
    	    		// close the current TimeSeries/VTEC window
    	    		
    	    		close();
    	    		
    		 }
    		
    	}
    }
// ----------------------------------------------------------------
    private static void setLastUpdateTimeToCurrentTime()
    {
        //return the time in seconds, so divide by 1000
        _lastUpdateTime = System.currentTimeMillis()/1000;
        
        return;
    }

    protected static void setLastUpdateTime(long lastUpdateTimeInSeconds)
    {
        //return the time in seconds, so divide by 1000
        _lastUpdateTime = lastUpdateTimeInSeconds;
        
        return;
    }
    // ---------------------------------------------------------------
    public static long getLastUpdateTime()
    {
        return _lastUpdateTime;
    }
    
//  ------------------------------------------------------------------------

    private void openOutputFile(String fileName)
    {
         try
         {
             if (fileName != null)
             {
                 _outputStream = new BufferedOutputStream( new FileOutputStream(fileName, false) );
                 
          //       _fileIsOpen = true;
             }
             else //fileName == null
             {
                 _outputStream = System.out;
             }

             _writer = new PrintWriter( _outputStream );
         }
         catch (java.io.IOException e)
         {
             System.out.println( e );
             
         }
    }
    
    // ------------------------------------------------------------------------------------
    private void initCanvas(int x, int y,
                            int width, int height)
    
    {      
        MeasuringUnit measuringUnit = MeasuringUnit.feet;
        
        _canvas = new TsPaintableCanvas(
                                        measuringUnit,
                                        x, y, 
                                        width, height);
          
        _canvas.setPreferredSize(_initialSize);
       // _canvas.setMinimumSize(_minSize);
     
        return;
        
     } //end initCanvas
    
    
    // ---------------------------------------------------------------------------------
    /*
     * get the current event's begin time for the specified locationId
     */
    private long getCurVtecBeginTime()
    {
    	long time = -2;
    	
    	try
    	{   
    		if (_fpCurPrevVtecData.getcurBeginTime() == MISSING_INT)
    		{	
    			_forceSetBeginTimeMissing = true;
    			time = MISSING_INT;
    		}	
    		else if (_fpCurPrevVtecData.getcurBeginTime() == -1)
    		{	
    			_forceSetBeginTimeCurrent = true;
    			time = -1;
    		}	
    		else
    		{	
    		    _forceSetBeginTime = true;
    			
    		    time = _fpCurPrevVtecData.getcurBeginTime() * MILLIS_PER_SECOND;
    		}   
    	}
    	catch (Throwable t)
    	{
    		System.out.println("FpVtecInfoFrame().getCurVtecBeginTime():" + "begintime =" + time);
    	}
    	return time;
    }
  
    //  ---------------------------------------------------------------------------------
    /*
     * get the current event's end time for the specified locationId
     */
    private long getCurVtecEndTime()
    {
    	long time = -2;
    	
    	try
    	{    		
    		if (_fpCurPrevVtecData.getcurEndTime() == MISSING_INT)  
    		{	
    			_forceSetEndTimeMissing = true;
    			time = MISSING_INT;
    		}	
    		else if (_fpCurPrevVtecData.getcurEndTime() == -1)
    		{	
    			_forceSetEndTimeCurrent = true;
    			time = -1;
    		}	
    		else
    		{	
    		    _forceSetEndTime = true;
    			time = _fpCurPrevVtecData.getcurEndTime() * MILLIS_PER_SECOND;
    		}	
    	}
    	catch (Throwable t)
    	{
    		System.out.println("FpVtecInfoFrame().getCurVtecEndTime():" + "endtime =" + time);
    	}
    	return time;
    }
    
   //  ---------------------------------------------------------------------------------
    /*
     * get the current event's rise above flood stage time for the specified locationId
     */
    private long getCurVtecRiseTime()
    {
    	long time = -2;
    	
    	try
    	{    	
    		if (_fpCurPrevVtecData.getcurRiseTime() == MISSING_INT)
    		{
    			_forceSetRiseTimeMissing = true;
    			time = MISSING_INT;
    		}	
    		else
    		{
    			_forceSetRiseTimeMissing = false;    		
    			time = _fpCurPrevVtecData.getcurRiseTime() * MILLIS_PER_SECOND;
    		}	
    	}
    	catch (Throwable t)
    	{
    		System.out.println("FpVtecInfoFrame().getCurVtecRiseTime():" + "risetime =" + time);
    	}
    	return time;
    }   
    
    //  ---------------------------------------------------------------------------------
    /*
     * get the current event's crest time for the specified locationId
     */
    private long getCurVtecCrestTime()
    {
    	long time = -2;
    	
    	try
    	{    	    		
    		if (_fpCurPrevVtecData.getcurCrestTime() == MISSING_INT)
    		{	
    			_forceSetCrestTimeMissing = true;
    			time = MISSING_INT;
    		}	
    		else
    		{
    			_forceSetCrestTimeMissing = false;
    			time =  _fpCurPrevVtecData.getcurCrestTime() * MILLIS_PER_SECOND;
    		}	
    	}
    	catch (Throwable t)
    	{
    		System.out.println("FpVtecInfoFrame().getCurVtecCrestTime():" + "cresttime =" + time);
    	}
    	return time;
    }
    
    //  ---------------------------------------------------------------------------------
    /*
     * get the current event's crest value for the specified locationId
     */
    private String getCurVtecCrestValue()
    {
    	double value = -9999.0;
    	String valuestring = null;
    	
    	try
    	{
    		value = _fpCurPrevVtecData.getcurCrestValue();
    		valuestring = Double.toString(value);
    		
    	}
    	catch (Throwable t)
    	{
    		System.out.println("FpVtecInfoFrame().getCurVtecCrestValue():" + "crest value =" + value);
    	}
    	return valuestring;
    }
    //  ---------------------------------------------------------------------------------
    /*
     * get the current event's fall below of flood stage time for the specified locationId
     */
    private long getCurVtecFallTime()
    {
    	long time = -2;
    	
    	try
    	{    		
    		if ( _fpCurPrevVtecData.getcurFallTime() == MISSING_INT)
    		{
    			_forceSetFallTimeMissing = true;
    			time = MISSING_INT;
    		}	
    		else
    		{
    			_forceSetFallTimeMissing = false;    		
    			time = _fpCurPrevVtecData.getcurFallTime() * MILLIS_PER_SECOND;
    		}	
    	}
    	catch (Throwable t)
    	{
    		System.out.println("FpVtecInfoFrame().getCurVtecFallTime():" + "falltime =" + time);
    	}
    	return time;
    }
    
    //  ---------------------------------------------------------------------------------
    /*
     * get the previous event's action code for the specified locationId
     */
    private String getPrevVtecAction()
    {
    	String prevVtecAction = "";
    	
    	try
    	{
    		prevVtecAction = _fpCurPrevVtecData.getprevAction();
    	}
    	catch (Throwable t)
    	{
    		System.out.println("FpVtecInfoFrame().getPrevVtecAction():" + "Action =" + prevVtecAction);
    	}
    	return prevVtecAction;
    }
    
    //  ---------------------------------------------------------------------------------
    /*
     * get the previous event's Record for the specified locationId
     */
    private String getPrevVtecRecord()
    {
    	String prevVtecRecord = "";
    	
    	try
    	{
    		prevVtecRecord = _fpCurPrevVtecData.getprevRecord();
    	}
    	catch (Throwable t)
    	{
    		System.out.println("FpVtecInfoFrame().getPrevVtecRecord():" + "Record =" + prevVtecRecord);
    	}
    	return prevVtecRecord;
    }
    
    //  ---------------------------------------------------------------------------------
    /*
     * get the previous event's rise type for the specified locationId
     */
    private String getPrevVtecRiseType()
    {
    	String prevVtecRiseType = "";
    	
    	try
    	{
    		prevVtecRiseType = _fpCurPrevVtecData.getprevRiseType();
    	}
    	catch (Throwable t)
    	{
    		System.out.println("FpVtecInfoFrame().getPrevVtecRiseType():" + "Rise Type =" + prevVtecRiseType);
    	}
    	return prevVtecRiseType;
    }
    
    //  ---------------------------------------------------------------------------------
    /*
     * get the previous event's crest type for the specified locationId
     */
    private String getPrevVtecCrestType()
    {
    	String prevVtecCrestType = "";
    	
    	try
    	{
    		prevVtecCrestType = _fpCurPrevVtecData.getprevCrestType();
    	}
    	catch (Throwable t)
    	{
    		System.out.println("FpVtecInfoFrame().getPrevVtecCrestType():" + "Crest Type =" + prevVtecCrestType);
    	}
    	return prevVtecCrestType;
    }
    
    //  ---------------------------------------------------------------------------------
    /*
     * get the previous event's fall type for the specified locationId
     */
    private String getPrevVtecFallType()
    {
    	String prevVtecFallType = "";
    	
    	try
    	{
    		prevVtecFallType = _fpCurPrevVtecData.getprevFallType();
    	}
    	catch (Throwable t)
    	{
    		System.out.println("FpVtecInfoFrame().getPrevVtecFallType():" + "Fall Type =" + prevVtecFallType);
    	}
    	return prevVtecFallType;
    }
    //  ---------------------------------------------------------------------------------
    /*
     * get the previous event's action code for the specified locationId
     */
    private String getPrevVtecIC()
    {
    	String prevVtecIC = "";
    	
    	
    	try
    	{
    		prevVtecIC = _fpCurPrevVtecData.getprevCause();
    	}
    	catch (Throwable t)
    	{
    		System.out.println("FpVtecInfoFrame().getPrevVtecIC():" + "Immediate Cause =" + prevVtecIC);
    	}
    	return prevVtecIC;
    }
    
//  ---------------------------------------------------------------------------------
    /*
     * get the previous event's phenom code for the specified locationId
     */
    private String getPrevVtecPhenom()
    {
    	String prevVtecPhenom = "";
    	
    	try
    	{
    		prevVtecPhenom = _fpCurPrevVtecData.getprevPhenom();
    	}
    	catch (Throwable t)
    	{
    		System.out.println("FpVtecInfoFrame().getPrevVtecPhenom():" + "Phenom =" + prevVtecPhenom);
    	}
    	return prevVtecPhenom;
    }
    
//  ---------------------------------------------------------------------------------
    /*
     * get the previous event's Signif code for the specified locationId
     */
    private String getPrevVtecSignif()
    {
    	String prevVtecSignif = "";
    	
    	try
    	{
    		prevVtecSignif = _fpCurPrevVtecData.getprevSignif();
    	}
    	catch (Throwable t)
    	{
    		System.out.println("FpVtecInfoFrame().getPrevVtecSignif():" + "Signif =" + prevVtecSignif);
    	}
    	return prevVtecSignif;
    }
    
//  ---------------------------------------------------------------------------------
    /*
     * get the previous event's severity code for the specified locationId
     */
    private String getPrevVtecSeverity()
    {
    	String prevVtecSeverity = "";
    	
    	try
    	{
    		prevVtecSeverity = _fpCurPrevVtecData.getprevSeverity();
    	}
    	catch (Throwable t)
    	{
    		System.out.println("FpVtecInfoFrame().getPrevVtecSeverity():" + "Severity =" + prevVtecSeverity);
    	}
    	return prevVtecSeverity;
    }
    //  ---------------------------------------------------------------------------------
    /*
     * get the previous event's Begin Time for the specified locationId
     */
    private String getPrevVtecBeginTime()
    {
    	long time = -1;
    	String timestring = "";
    	
    	try
    	{
    		 
    		      
    		time = _fpCurPrevVtecData.getprevBeginTime();
    		if (time == MISSING_INT)
    		{
    			 timestring = "MSG";
    		}
    		else if (time != 0)
    		{
    			time = time * MILLIS_PER_SECOND;    		
    		    //timestring = DbTimeHelper.getDateTimeStringFromLongTime(time);
    		    timestring = DbTimeHelper.getDateTimeToMinutesStringFromLongTime(time );
    			
    		}    
    	}
    	catch (Throwable t)
    	{
    		System.out.println("FpVtecInfoFrame().getPrevVtecBeginTime():" + "begintime =" + time);
    	}
    	return timestring;
    }
    
    //  ---------------------------------------------------------------------------------
    /*
     * get the previous event's end time for the specified locationId
     */
    private String getPrevVtecEndTime()
    {
    	long time = -1;
    	String timestring = "";
    	
    	try
    	{
    		time = _fpCurPrevVtecData.getprevEndTime();
    		if (time == MISSING_INT)
    		{
    			timestring = "MSG";
    		}
    		else if (time != 0)
    		{
    			time = time * MILLIS_PER_SECOND;
    			//timestring = DbTimeHelper.getDateTimeStringFromLongTime(time);
    			timestring = DbTimeHelper.getDateTimeToMinutesStringFromLongTime(time );
    			
    		}
    	}
    	catch (Throwable t)
    	{
    		System.out.println("FpVtecInfoFrame().getPrevVtecEndTime():" + "endtime =" + time);
    	}
    	return timestring;
    }
    
    //  ---------------------------------------------------------------------------------
    /*
     * get the previous event's rise abpve flood stage time for the specified locationId
     */
    private String getPrevVtecRiseTime()
    {
    	long time = -1;
    	String timestring = "";
    	
    	try
    	{
    		time = _fpCurPrevVtecData.getprevRiseTime();
    		if (time == MISSING_INT )
    		{
    			timestring = "MSG";
    		}
    		else if (time != 0)
    		{
    			time = time * MILLIS_PER_SECOND;
    			//timestring = DbTimeHelper.getDateTimeStringFromLongTime(time);
    			timestring = DbTimeHelper.getDateTimeToMinutesStringFromLongTime(time );
    			
    		}
    	}
    	catch (Throwable t)
    	{
    		System.out.println("FpVtecInfoFrame().getPrevVtecRiseTime():" + "risetime =" + time);
    	}
    	return timestring;
    }
    
    //  ---------------------------------------------------------------------------------
    /*
     * get the previous event's crest time for the specified locationId
     */
    private String getPrevVtecCrestTime()
    {
    	long time = -1;
    	String timestring = "";
    	try
    	{
    		time = _fpCurPrevVtecData.getprevCrestTime();
    		if (time == MISSING_INT)
    		{
    			timestring = "MSG";
    		}
    		else if (time != 0)
    		{
    			time = time * MILLIS_PER_SECOND;
    			//timestring = DbTimeHelper.getDateTimeStringFromLongTime(time);
    			timestring = DbTimeHelper.getDateTimeToMinutesStringFromLongTime(time );
    			
    		}
    	}
    	catch (Throwable t)
    	{
    		System.out.println("FpVtecInfoFrame().getPrevVtecCrestTime():" + "cresttime =" + time);
    	}
    	return timestring;
    }
    
    //  ---------------------------------------------------------------------------------
    /*
     * get the previous event's crest value for the specified locationId
     */
    private String getPrevVtecCrestValue()
    {
    	double value = -9999.0;
    	String valuestring = null;
    	
    	try
    	{    		
    		value = _fpCurPrevVtecData.getprevCrestValue();
    	    if (value != 0.0)
    		   valuestring = Double.toString(value);
    		
    	}
    	catch (Throwable t)
    	{
    		System.out.println("FpVtecInfoFrame().getPrevVtecCrestValue():" + "crest value =" + value);
    	}
    	    
    	return valuestring;
    }
    

    
    //  ---------------------------------------------------------------------------------
    /*
     * get the previous event's fall time for the specified locationId
     */
    private String getPrevVtecFallTime()
    {
    	long time = -1;
    	String timestring = "";
    	
    	try
    	{
    		time = _fpCurPrevVtecData.getprevFallTime();
    		if (time == MISSING_INT)
    		{
    			timestring = "MSG";
    		}
    		else if ( time != 0)
    		{
    			time = time * MILLIS_PER_SECOND;
    			//timestring = DbTimeHelper.getDateTimeStringFromLongTime(time);
    			timestring = DbTimeHelper.getDateTimeToMinutesStringFromLongTime(time );
    		  
    		}
    	}
    	catch (Throwable t)
    	{
    		System.out.println("FpVtecInfoFrame().getPrevVtecFallTime():" + "falltime =" + time);
    	}
    	return timestring;
    }
    
    //  ---------------------------------------------------------------------------------
    /*
     * get the previous event's product time for the specified locationId
     */
    private String getPrevVtecProductTime()
    {
    	long time = -1;
    	String timestring = "";
    	
    	try
    	{
    		time = _fpCurPrevVtecData.getprevProductTime();
    		if (time == MISSING_INT)
    		{
    			timestring = "MSG";
    		}
    		else if ( time != 0)
    		{
    			time = time * MILLIS_PER_SECOND;
    			timestring = DbTimeHelper.getDateTimeStringFromLongTime(time);
    		}
    	}
    	catch (Throwable t)
    	{
    		System.out.println("FpVtecInfoFrame().getPrevVtecProductTime():" + "producttime =" + time);
    	}
    	return timestring;
    }  
        
    // ------------------------------------------------------------------------------
	public FpVtecInfo getApplication() {
		return _application;
	}
	
	// --------------------------------------------------------------------------------

	public void setApplication(FpVtecInfo application) {
		this._application = application;
	}
	
   //----------------------------------------------------------------------------------
	private class UpdateVtecTimeDocumentListener implements DocumentListener   
    {
	    
		public void insertUpdate(DocumentEvent arg0)
		{			
			
			if (! _graphicalChange)
			{
				System.out.println("UpdateVtecTimeDocumentListener.insertUpdate() event used.");
				updateGraph();
			}
			else
			{
				System.out.println("UpdateVtecTimeDocumentListener.insertUpdate() event ignored.");
				
			}
			
		}
		
		public void removeUpdate(DocumentEvent arg0)
		{
  		
		}
		
		public void changedUpdate(DocumentEvent arg0)
		{
      		
		}
        
    }

	public FpVtecInfoDataManager get_dataMgr() {
		return _dataMgr;
	}

	public void set_dataMgr(FpVtecInfoDataManager mgr) {
		_dataMgr = mgr;
	}
	
	// action for check box _curvtecRiseTimeSetMissingCheckBox
	private class ChangeRiseTimeMissingCheckBoxListener implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			AbstractButton abstractButton = (AbstractButton) evt.getSource();
			boolean selected = abstractButton.getModel().isSelected();
			if (selected == true)
			{
				_curvtecRiseTimeTextField.setText("MSG");				
		    }
		    else
		    { 		   	 		   	    
		   	    _curvtecRiseTimeTextField.setTime(_currentTime);
		   	             		   	   
		    }
			
			updateGraph();
						
		}
	}
	
   // action for check box _curvtecCrestTimeSetMissingCheckBox
	private class ChangeCrestTimeMissingCheckBoxListener implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			AbstractButton abstractButton = (AbstractButton) evt.getSource();
			boolean selected = abstractButton.getModel().isSelected();
			if (selected == true)
			{
				_curvtecCrestTimeTextField.setText("MSG");
				 
		    }
		    else
		    { 		   	    	
		   	    _curvtecCrestTimeTextField.setTime(_currentTime);
		   	    		   	   
		    }
			
			updateGraph();
						
		}
	}
	
    // action for check box _curvtecFallTimeSetMissingCheckBox
	private class ChangeFallTimeMissingCheckBoxListener implements ActionListener
	{			
		public void actionPerformed(ActionEvent evt)
		{
			AbstractButton abstractButton = (AbstractButton) evt.getSource();
			boolean selected = abstractButton.getModel().isSelected();
			if (selected == true)
			{
				_curvtecFallTimeTextField.setText("MSG");				
				
		    }
		    else
		    { 		   	    
		   	    _curvtecFallTimeTextField.setTime(_currentTime);
		   	    		   	 
		    }
			
			updateGraph();
						
		}
		
	}
	
	// action for radio button _curvtecBeginTimeSetMissingRadioButton
	private class SelectBeginTimeMissingRadioButtonListener implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			AbstractButton abstractButton = (AbstractButton) evt.getSource();
			boolean selected = abstractButton.getModel().isSelected();
			if (selected == true)
			{
				_curvtecBeginTimeTextField.setText("MSG");
				updateGraph();							
		        
		    }		 
						
		}
	}
	
	
	// action for radio button _curvtecBeginTimeSetCurrentRadioButton
	private class SelectBeginTimeCurrentRadioButtonListener implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			AbstractButton abstractButton = (AbstractButton) evt.getSource();
			boolean selected = abstractButton.getModel().isSelected();
			if (selected == true)
			{
				_curvtecBeginTimeTextField.setText("CURRENT");
				updateGraph();							
		    }
		  					
		}
	}
	
   // action for radio button _curvtecBeginTimeRadioButton
	private class SelectBeginTimeRadioButtonListener implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			AbstractButton abstractButton = (AbstractButton) evt.getSource();
			boolean selected = abstractButton.getModel().isSelected();
			if (selected == true)
			{									    
				_curvtecBeginTimeTextField.setTime(_currentTime);
				updateGraph();
				
		    }		   
						
		}
	}
	
//	 action for radio button _curvtecEndTimeSetMissingRadioButton
	private class SelectEndTimeMissingRadioButtonListener implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			AbstractButton abstractButton = (AbstractButton) evt.getSource();
			boolean selected = abstractButton.getModel().isSelected();
			if (selected == true)
			{
				_curvtecEndTimeTextField.setText("MSG");
				updateGraph();
				
		    }		 
						
		}
	}
	
	
	// action for radio button _curvtecEndTimeSetCurrentRadioButton
	private class SelectEndTimeCurrentRadioButtonListener implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			AbstractButton abstractButton = (AbstractButton) evt.getSource();
			boolean selected = abstractButton.getModel().isSelected();
			if (selected == true)
			{
				_curvtecEndTimeTextField.setText("CURRENT");
				updateGraph();
				
		    }		    
						
		}
	}
	
   // action for radio button _curvtecEndTimeRadioButton
	private class SelectEndTimeRadioButtonListener implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			AbstractButton abstractButton = (AbstractButton) evt.getSource();
			boolean selected = abstractButton.getModel().isSelected();
			if (selected == true)
			{								
				_curvtecEndTimeTextField.setTime(_currentTime);	
				updateGraph();
				
		    }
		    					
		}
	}
	
}
