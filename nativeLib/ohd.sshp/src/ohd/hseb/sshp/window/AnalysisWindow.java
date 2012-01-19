/*
 * Created on Jul 18, 2003
 *
 * 
 */
package ohd.hseb.sshp.window;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.BufferedImage;

import javax.imageio.*;
import javax.imageio.stream.*;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.filechooser.FileFilter;

import java.io.*;
import java.text.*;
import java.util.*;
import java.util.List;

import ohd.hseb.sshp.*;
import ohd.hseb.sshp.gui.GuiHelper;
import ohd.hseb.sshp.gui.PrecipPainter;
import ohd.hseb.sshp.gui.PrecipTotalTextPainter;
import ohd.hseb.sshp.gui.SettingPanel;
import ohd.hseb.sshp.gui.SigStageLinePainter;
import ohd.hseb.sshp.gui.StagePainter;
import ohd.hseb.sshp.gui.TimeLinePainter;
import ohd.hseb.sshp.gui.TsBackgroundPainter;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.model.*;
import ohd.hseb.model.sacsma.SacSmaState;
import ohd.hseb.measurement.*;
import ohd.hseb.util.*;
import ohd.hseb.util.gui.drawing.TsPaintableCanvas;
import ohd.hseb.util.gui.DateTimeTextField;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.ExtensionFileFilter;
import ohd.hseb.util.gui.StandardImageIcons;
import ohd.hseb.util.gui.WindowResizingManager;
import ohd.hseb.util.io.RGBTextFileReader;


/**
 * @author Chip Gobs
 *
 * Main Frame for the Precip and Stage Canvases and the application itself.
 */
public class AnalysisWindow extends JFrame 
{
    private static final String _className = "AnalysisWindow";
    
    private static final double MIN_PRECIP_IN_INCHES_PER_HOUR = 0.0;
    private static final double MAX_PRECIP_IN_INCHES_PER_HOUR = 20.0;
    
    private static final double MIN_STAGE_IN_FEET = -10.0;
    private static final double MAX_STAGE_IN_FEET = 40000.0;
    
    private static final double MIN_EVAP_PER_HOUR = 0.0;
    private static final double MAX_EVAP_PER_HOUR = 100.0;
    
    private static final double MIN_RUNOFF_IN_INCHES_PER_HOUR = 0.0;
    private static final double MAX_RUNOFF_IN_INCHES_PER_HOUR = MAX_PRECIP_IN_INCHES_PER_HOUR;
  
    private static final long MILLIS_PER_HOUR = 1000 * 60 * 60;
    
    
    // forecast length and spinner  constants
    //see StreamModel.java for the default length in hours
    private static final int FORECAST_LENGTH_SPINNER_STEPS = 6;
     
    private static final MeasuringUnit _stageUnit = MeasuringUnit.feet;
    
    private static final String ALLOW_LIVE_SAC_STATE_ADJUSTMENT_TOKEN_NAME =
                                "sshp_allow_live_state_adjustment";
    
    
    //determines whether to default to showing the simulated time series or not
    private static final String SHOW_SIMULATED_TS_TOKEN_NAME =
                                "sshp_show_simulated_timeseries";
    private static final boolean _default_show_simulated_timeseries = true;

    private static final String SHOW_UNADJUSTED_STATES_TOKEN_NAME =
                                "sshp_show_unadjusted_states";
    private static final boolean _default_show_unadjusted_states = false;
    
    
    // application variables
  //  private StreamModel _streamModel = null;
    private ModelManager _modelManager = null;
    private AppController _controller = null;

    
    
    // image storage directory
    private String _whfs_image_directoryString = null;
    private String _sshp_precip_directoryString = null;
    
    
    // window sizing data
    private Dimension _maxDimension = new Dimension(1280, 1024);
    private Dimension _initialDimension = new Dimension(1200, 944);
    private Dimension _minDimension = new Dimension(800, 700);
     

	// the canvases		
	private TsPaintableCanvas _precipCanvas = null;
	private TsPaintableCanvas _stageCanvas = null;
    
    // data to be used with the canvases
    private int _hoursToShowBeforeModelStartTime = 24;
    private int _hoursToShowAfterModelStartTime = 72;
    
    // split pane and its direct contents
    private JSplitPane _splitPane = null;
     
    // canvas value mappers
    private ValueMapper _precipValueMapper = null;
    private ValueMapper _stageValueMapper = null; 
    
    // these Painters are in this class because of the ability to modify the way the canvases are
    // painted.  The modifications are made in this class.
    // The painters have special options which are invoked.  Then, upon repainting, the
    // painter paints in the way dictated by those options.
    private SigStageLinePainter _floodStageLinePainter = null;
    private SigStageLinePainter _majorFloodStageLinePainter = null;
	private StagePainter _observedStageTsPainter = null;
    private StagePainter _simulatedStageTsPainter = null;
    private StagePainter _forecastPainter = null;
    private StagePainter _priorForecastPainter = null;

    
    private PrecipPainter _precipPainter = null;
    private PrecipTotalTextPainter _precipTextTotalPainter = null;
    private TsBackgroundPainter _stageBackgroundPainter = null;
    private TsBackgroundPainter _precipBackgroundPainter = null;
    
    private Font _buttonFont = null;
    private Border _panelBorder = BorderFactory.createLineBorder(Color.black);
         
	
    //vcr buttons
    private JPanel    _timeButtonPanel = null;
	private JButton   _forwardButton = null;
	private JButton   _forward1DayButton = null;
	private JButton   _backButton = null;
	private JButton   _back1DayButton = null;
    private JButton   _recenterButton = null;
    
  
    //action buttons
	private JButton   _closeButton = null;
    private JButton   _windowLauncherButton = null;
    private JPanel    _windowButtonPanel = null;
	private JButton   _screenCaptureButton = null;
	
	
    // model control panel
    private JPanel _modelControlPanel = null;
    private JLabel _modelControlPanelLabel = null;
    private JLabel _modelTypeComboBoxLabel = null;
    private JComboBox _modelTypeComboBox = null;
    private String[]  _modelTypeNameArray = null;
    private JLabel _uhgComboBoxLabel = null;
    private JComboBox _uhgComboBox = null;
      
    private JLabel    _adjustmentCheckBoxLabel = null;
    private JCheckBox _adjustmentCheckBox = null; 
    private JLabel    _adjustmentLastObsInputTimeLabel = null;
    private DateTimeTextField _adjustmentLastObsInputTimeTextField = null;
    private JLabel    _blendPeriodSpinnerLabel = null;    
    private JSpinner  _blendPeriodSpinner = null;
    
     
    //These share the same spot on the model control panel, depending on
    // the model being used
    private JComboBox _sacStartingStateComboBox = null;
    private JButton   _sacStateListRefreshButton = null;
    private JComboBox    _apiMkcStartingStateComboBox = null; 
    private JButton      _apiMkcStateRefreshButton = null;
    
    private JLabel   _forecastHoursSpinnerLabel = null;
    private JSpinner _forecastHoursSpinner = null;
    

    // loose on the button panel 
    private JLabel    _modelRunStartTimeLabel = null;
    private JLabel    _modelRunStartTimeValueLabel = null;
    
     
    // SAC_SMA control panel   
    private JPanel    _sacControlPanel = null;
    private JLabel    _sacControlPanelLabel = null;
    private JButton   _sacParamsButton = null;
    private JButton   _sacStateButton = null;
//    private JButton   _sacMonthlyMapeValuesEditorButton = null;
    
    private JButton   _sacLiveStateAdjustmentButton = null;
    private boolean   _allowLiveSacAdjustment = false;
    
    private JCheckBox _sacShowAlternateForecastsCheckBox = null;
    private JCheckBox _sacShowPastForecastsCheckBox = null;
    
    private boolean   _showAlternateForecasts = false;
    private boolean   _showPastForecasts = false;
    
    private JButton _sacSetVarInitialStateButton = null;
    
     // time series editing and saving panel
    private JMenuBar  _menuBar = null;
    
    private boolean _showUnadjustedStates = true;
    
    
  
    //API-MKC variable setting/resetting buttons for model settings
    private JPanel       _apiMkcControlPanel = null;
    private JLabel       _apiMkcSettingsLabel = null;
    private JLabel       _modelStateLabel = null;
 
    private SettingPanel _apiMkcInitialStagePanel = null; 
    private SettingPanel _apiMkcFfhPanel = null;
    private SettingPanel _apiMkcThreshRPanel = null;
    
    private JCheckBox    _apiMkcUseCustomModelRunTimeCheckBox = null;
    private DateTimeTextField   _apiMkcCustomModelRunDateTimeTextField = null;
    private JButton      _apiMkcApplySettingsButton = null;
    

    // associated API-MKC variables
    private boolean      _apiMkcUseCustomModelRunTime = false;
    
    //used to help control how events are processed
    private boolean      _reloadingApiMkcStartingStateComboBox = false;
    private boolean      _reloadingSacStartingStateComboBox = false;
 
    
    // graphing controls
    private JPanel      _graphControlPanel = null;
    private JLabel      _graphControlPanelLabel = null;
    
    private JCheckBox   _showFloodStageCheckBox = null;
    private JCheckBox   _showMajorFloodStageCheckBox = null;
    private JCheckBox   _showObsHeightTsCheckBox = null;
    private JCheckBox   _showPrecipAmountsCheckBox = null;
    private JCheckBox   _delayRerunCheckBox = null;
    private JCheckBox   _showMinorPrecipTicksCheckBox = null;
    private JCheckBox   _showMinorStageTicksCheckBox = null;
    private JCheckBox   _showSimulatedStageCheckBox = null;
    
    // booleans related to visual controls
    private boolean _forceShowFloodStage = true;
    private boolean _forceShowMajorFloodStage = false;
    private boolean _showPrecipAmounts = true; 
    private boolean _showObservedHeightData = true;
    private boolean _delayRerunModelWhileDrawing = false;
    private boolean _showMinorPrecipTicks = false;
    private boolean _showMinorStageTicks = true;
    private boolean _showSimulatedStage = true;
 
    //main window layer   
	private JPanel    _mainPanel = null;
    private JPanel    _buttonPanel = null;
    
    // misc
    private JFileChooser _imageFileChooser = null;
    private FileChooserHelper _precipFileChooserHelper = null;
    private FileChooserHelper _imageFileChooserHelper = null;   
 
    // data
    private TimeHolder _modelStartTimeHolder = null;	
    private int _intervalInHours = 1;

	private int _millisPerHour = 1000 * 60 * 60;
    
    private String _defaultFormatString = "0.00";
    private double _defaultMissingValue = -9999.0;


    // model starting state descriptors
    private SacSmaStateDescriptor _selectedSacStateDescriptor = null;
    private FFHDescriptor _selectedFFHDescriptor = null;

    
    //debugging
    private CodeTracer _tracer = null;
    
    
    private Map<String, Color> _colorMap = null;
    private String _colorFilePath = null;
    
	// ----constructors-------------------------------------
	
	public AnalysisWindow(ModelManager modelManager, AppController controller)
	{
		//init the other application objects that this object talks to
		//_streamModel = streamModel;
        _modelManager = modelManager;
				
        _controller = controller;

        _tracer = new CodeTracer();

		_modelStartTimeHolder = getPrimaryStreamModel().getModelStartTimeHolder();
          
        getAppsDefaults();
	
        RGBTextFileReader reader = new RGBTextFileReader();       
        _colorMap = reader.read(_colorFilePath); 
     
        
		initGui();
	
	} //AnalysisWindow

	// ------------------------------------------------------------
    private void getAppsDefaults()
    {
        AppsDefaults ad = new AppsDefaults();
        
        // determine if the secret adjustment window should be available
        if (ad.getToken(ALLOW_LIVE_SAC_STATE_ADJUSTMENT_TOKEN_NAME) != null )
        {
            _allowLiveSacAdjustment = true;
        }
        
        // default value for the show simulated stage time series toggle button
        _showSimulatedStage = ad.getBoolean(SHOW_SIMULATED_TS_TOKEN_NAME, 
                                            _default_show_simulated_timeseries);
        
        _showUnadjustedStates = ad.getBoolean(SHOW_UNADJUSTED_STATES_TOKEN_NAME,
                                                _default_show_unadjusted_states);
              
        _sshp_precip_directoryString = ad.getToken("sshp_precip_dir");
        
        _whfs_image_directoryString = ad.getToken("whfs_image_dir");
        
        _colorFilePath = ad.getToken("color_file_path", "/usr/lib/X11/rgb.txt");
        
    }
    
    //  ------------------------------------------------------------
    
	private void initGui()
	{
        
        String header = "AnalysisWindow.initGui(): ";
       // System.out.println(header + "starting.");
		int x = 10;
		int y = 10;
		int width = 700;
		int height = 200;
        
        int horizontalMargin = 50;
        int verticalMargin = 40;
    
        Dimension prefSplitPaneSize =
                  new Dimension(width + horizontalMargin,
                               (2*height) + verticalMargin);
        
        initTitle();

        initMenuBar();
     
        //set up the 2 canvases and all of their containers
		initPrecipCanvas(x, y, width, height, _modelStartTimeHolder);
		initStageCanvas(x, y, width, height, _modelStartTimeHolder);                   
  
        _splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, 
                                    _precipCanvas,
                                    _stageCanvas);
                                    
                                           
        _splitPane.setOneTouchExpandable(true);
        _splitPane.setPreferredSize(prefSplitPaneSize);
       // _splitPane.setDividerLocation(.50);
        
    
        //init  "loose" GUI items
        initStatusAndReloadControls();
        
        // init  model control panel
        initModelControlPanel();
     
        // create the window control button(s)   
        initWindowButtonPanel();
           
        // SAC gui launch buttons       
        initSacControlPanel();
         
         // init the KC-API model parameter controls
        initApiMkcControlPanel();
             
        // visual controls
        initGraphControlPanel();
       
		// add the buttons to the _buttonPanel
		_buttonPanel = new JPanel();
		_buttonPanel.setLayout(new GridBagLayout());
		GridBagConstraints buttonGbc = new GridBagConstraints();
		
		
		//panel for the model run start time Labels
		JPanel modelRunStartTimePanel = new JPanel();
		modelRunStartTimePanel.add(_modelRunStartTimeLabel);
		modelRunStartTimePanel.add(_modelRunStartTimeValueLabel);
		
		
//                                                                          col, row   numCols numRows  Wcol wrow        
        //row 0		
        buttonGbc.insets = new Insets(5,2,5,2);
         
        addComponent(_buttonPanel,  _modelControlPanel, buttonGbc,          0,     0,   1,      3,       0,   0);        
        addComponent(_buttonPanel,  _graphControlPanel, buttonGbc,          1,     0,   1,      3,       0,   0); 
        //only 1 of the following panels shows at a time

         
        
        addComponent(_buttonPanel,  _apiMkcControlPanel, buttonGbc,         2,     0,    1,    3,       0,   0);  
        addComponent(_buttonPanel,  _sacControlPanel, buttonGbc,            2,     0,    1,    3,       0,   0);  
        
        addComponent(_buttonPanel,  modelRunStartTimePanel, buttonGbc,      0,     4,   1,      1,       0,   0);        
        
      
                                                                       //  col, row   numCols numRows  Wcol wrow      
     
        //row 6

        buttonGbc.insets = new Insets(5,5,5,5);
        buttonGbc.anchor = GridBagConstraints.WEST;
        addComponent(_buttonPanel, _windowButtonPanel,    buttonGbc,         1,   4,    1,      1,      1,   0);
    
        
		//create the main panel
		_mainPanel = new JPanel();
		
		//add the components to the panel
        GridBagLayout layoutMgr = new GridBagLayout();
		
		GridBagConstraints mainGbc = new GridBagConstraints();
		mainGbc.fill = GridBagConstraints.BOTH;
		mainGbc.anchor = GridBagConstraints.NORTHWEST;
		mainGbc.weightx = 1;
		mainGbc.weighty = 1;
		
		_mainPanel.setLayout(layoutMgr);
		 
        // add components to the main panel
        
	   				 	       				    //       col, row   numCols numRows  Wcol wrow
		
		
		setJMenuBar(_menuBar);
		addComponent(_mainPanel, _splitPane,   mainGbc,  0,   0,    3,		10,       1,   1);
		addComponent(_mainPanel, _buttonPanel, mainGbc,  0,   10,   1,   	2,        0,   0);
      
	
        //set up initial bounds limitations
        //see WindowResize listener for the minimum setting

        Rectangle maxBoundsRectangle = new Rectangle(_maxDimension);
        this.setMaximizedBounds(maxBoundsRectangle);

      
		//add the panel to the Frame and initialize the frame
		this.getContentPane().add(_mainPanel);
		this.pack();


        Rectangle initialBoundsRectangle = new Rectangle(_initialDimension);
        this.setBounds(initialBoundsRectangle);
 
        //set up the KC-API settings
        resetInitialStageMeasurementFromModel();
        resetFfhFromModel();
        resetThresholdRunoffFromModel();
    
        
        //make sure that the appropriate controls are visible
        setComponentVisibility();
  
        //add all of the event handlers
        addListeners();
 
        //select the RainfallRunoffModel
        initModelChoice();  
	}
    
    //  ------------------------------------------------------------
    private String getCenteredText(String origText)
    {
        String centeredText = "<HTML><CENTER>" + origText +  "</CENTER></HTML>";
       
        return centeredText;     
    }
//  ------------------------------------------------------------
    private String getVerticalText(String origText)
    {
        char[] charArray = origText.toCharArray();
        StringBuffer buffer = new StringBuffer();
        
        buffer.append("<HTML>");
        for (int i = 0; i < charArray.length; i++)
        {
            buffer.append(charArray[i]);
            
            //don't do a break for the last one
            if (i < charArray.length-1)
            {
                buffer.append("<BR>");
            }
        }
        buffer.append("</HTML>");
        
        String verticalText = buffer.toString();
       
        return verticalText;     
    }
    //  ------------------------------------------------------------
  
    private void initTimeButtonPanel()
    {
        Dimension dimension = new Dimension(24, 28);
        
        Color buttonColor = this.getBackground();
        Color imageColor = Color.BLUE;
    
        ImageIcon rightArrowIcon = StandardImageIcons.getImageIcon(StandardImageIcons.RIGHT_ARROW, 
                                      imageColor, buttonColor, dimension);
        ImageIcon leftArrowIcon = StandardImageIcons.getImageIcon(StandardImageIcons.LEFT_ARROW, 
                                      imageColor, buttonColor, dimension);
  
   
        ImageIcon doubleRightArrowIcon = StandardImageIcons.getImageIcon(StandardImageIcons.DOUBLE_RIGHT_ARROW, 
                                      imageColor, buttonColor, dimension);
                                     
        ImageIcon doubleLeftArrowIcon = StandardImageIcons.getImageIcon(StandardImageIcons.DOUBLE_LEFT_ARROW, 
                                      imageColor, buttonColor, dimension);
  
        ImageIcon circleIcon  = StandardImageIcons.getImageIcon(StandardImageIcons.CIRCLE, 
                                      imageColor, buttonColor, dimension);
 
    
        _forwardButton = GuiHelper.getJButton(rightArrowIcon);
        _forwardButton.setToolTipText("Slide display forward 1 hour.") ;                                                                      
        
        _forward1DayButton = GuiHelper.getJButton(doubleRightArrowIcon);
        _forward1DayButton.setToolTipText("Slide display forward 24 hours.");                                                                       
       
        _recenterButton = GuiHelper.getJButton(circleIcon);
        _recenterButton.setToolTipText("Recenter display around the model run start time.");                                                                       
      
        _backButton = GuiHelper.getJButton(leftArrowIcon);
        _backButton.setToolTipText("Slide display back 1 hour.");                                                                       
       
        _back1DayButton = GuiHelper.getJButton(doubleLeftArrowIcon);
        _back1DayButton.setToolTipText("Slide display back 24 hours.");                                                                       
       
    
        _timeButtonPanel = new JPanel();
      //  _timeButtonPanel.setBorder(_panelBorder); 
        _timeButtonPanel.add(_back1DayButton);
        _timeButtonPanel.add(_backButton);
        _timeButtonPanel.add(_recenterButton);
        _timeButtonPanel.add(_forwardButton);
        _timeButtonPanel.add(_forward1DayButton);
        
    } //end initTimeButtonPanel
    
    //----------------------------------------------------------------------------------------------------
    private void initGraphControlPanel()
    {
        
       // String header = "AnalysisWindow.initGraphControlPanel(): ";
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weightx = 1;
        gbc.weighty = 1;
        
         gbc.insets = new Insets(2,2,2,2);
  
   
        _graphControlPanel = new JPanel();
       
        JPanel panel =  _graphControlPanel;
        panel.setBorder(_panelBorder);
  
        
        panel.setLayout(new GridBagLayout());
           
    
        _graphControlPanelLabel = new JLabel(getCenteredText("Graph Controls:"));

        initTimeButtonPanel();
        
        //init the option buttons and the painters that are associated with them         
        _showFloodStageCheckBox =  new JCheckBox("Scale To Flood Stage",
                                           _forceShowFloodStage);
        String text = "Display Horizontal Line to represent the flood stage";
        _showFloodStageCheckBox.setToolTipText(text);  
        
    
        if (_floodStageLinePainter != null)
        {
            _floodStageLinePainter.setShouldPaint(_forceShowFloodStage);
        }
        
        //init the option buttons and the painters that are associated with them         
        _showMajorFloodStageCheckBox =  new JCheckBox("Scale To Major Flood Stage",
                                                _forceShowMajorFloodStage);
        text = "Display Horizontal Line to represent the major flood stage";
        _showMajorFloodStageCheckBox.setToolTipText(text);  
        if (_majorFloodStageLinePainter != null)
        {
            _majorFloodStageLinePainter.setShouldPaint(_forceShowMajorFloodStage);
        }
        
         
        //init the option buttons and the painters that are associated with them  
        
        //precip totals
        _showPrecipAmountsCheckBox =  new JCheckBox("Show Precip Amounts",
                                                    _showPrecipAmounts);
        text = "Display precip totals (in inches) on the precip bars";
        _showPrecipAmountsCheckBox.setToolTipText(text);  
        _precipPainter.setShowPrecipAmounts(_showPrecipAmounts);
        _precipTextTotalPainter.setShouldPaint(_showPrecipAmounts);
        
        //observed stages
        _showObsHeightTsCheckBox = new JCheckBox("Show Obs Stages",
                                           _showObservedHeightData);
        text = "Toggle Observed Stage Time Series";
        _showObsHeightTsCheckBox.setToolTipText(text);  
        _observedStageTsPainter.setShouldPaint(_showObservedHeightData);           

       
        // add simulated stage checkbox
        _showSimulatedStageCheckBox = new JCheckBox("Show Simulated Stages", _showSimulatedStage);
        text = "Toggle Simulated Stage Time Series";
        _showSimulatedStageCheckBox.setToolTipText(text);  
        _simulatedStageTsPainter.setShouldPaint(_showSimulatedStage);
 
      
        //add delay button
        _delayRerunCheckBox = new JCheckBox("Delay Rerun While Drawing",
                                              _delayRerunModelWhileDrawing);
        text = "For graphical precip editing, postpones rerunning the model until after the button is released.";
        _delayRerunCheckBox.setToolTipText(text);         
         
         
        //add minor precip tick checkbox
        _showMinorPrecipTicksCheckBox = new JCheckBox("Show Minor Precip Lines", _showMinorPrecipTicks);
        text = "Toggle minor precip lines";
        _showMinorPrecipTicksCheckBox.setToolTipText(text);  
        _precipBackgroundPainter.setShowMinorTicks(_showMinorPrecipTicks); 
         
        // add minor stage tick checkbox
        _showMinorStageTicksCheckBox = new JCheckBox("Show Minor Stage Lines", _showMinorStageTicks);
        text = "Toggle minor stage lines";
        _showMinorStageTicksCheckBox.setToolTipText(text);  
        _stageBackgroundPainter.setShowMinorTicks(_showMinorStageTicks);
        
        
      
        
        JPanel verticalSpacer = new JPanel();
            verticalSpacer.setPreferredSize(new Dimension(30, 34));
   
   
       // JPanel verticalSpacer2 = new JPanel();
       //     verticalSpacer2.setPreferredSize(new Dimension(30, 8));
   
                                                            //    col, row   numCols numRows  Wcol wrow
        addComponent(panel, _graphControlPanelLabel,     gbc,   0,     0,  2,      1,       0,   1);
        
     
        addComponent(panel, _timeButtonPanel,             gbc,  0,     1,  2,      1,       0,   1);
    
     //   addComponent(panel, verticalSpacer,                gbc, 0,     2,  2,      1,       0,   1);
  
        addComponent(panel, _showObsHeightTsCheckBox,  gbc,     0,     2,  1,      1,       0,   1);
        addComponent(panel, _showSimulatedStageCheckBox,  gbc,  1,     2,  1,      1,       0,   1);
        
        
        addComponent(panel, _delayRerunCheckBox,  gbc,          0,     3,  1,      1,       0,   1);
        addComponent(panel, _showFloodStageCheckBox,  gbc,      1,     3,  1,      1,       0,   1); 
        
         
        addComponent(panel, _showPrecipAmountsCheckBox,  gbc,   0,     4,  1,      1,       0,   1);
        addComponent(panel, _showMajorFloodStageCheckBox,  gbc, 1,     4,  1,      1,       0,   1);
        
        
        addComponent(panel, _showMinorPrecipTicksCheckBox,  gbc,0,     5,  1,      1,       0,   1);
        addComponent(panel, _showMinorStageTicksCheckBox,  gbc, 1,     5,  1,      1,       0,   1);
        
        
        
       // addComponent(panel, verticalSpacer2,                gbc, 0,     6,  2,      1,       0,   1);
        
    
           
        return;        
    } //end initVisualControlPanel
    
    //----------------------------------------------------------------------------------------------------
     
    
    private void initWindowButtonPanel()
    {
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weightx = 1;
        gbc.weighty = 1;
   
        _windowLauncherButton =  GuiHelper.getJButton("Control Window...");
        
        _screenCaptureButton = GuiHelper.getJButton("Capture Screen");
           
        _closeButton = GuiHelper.getJButton("Close");
        
        _windowButtonPanel = new JPanel();
      //  _windowButtonPanel.setPreferredSize(new Dimension(150, 40));
        
        JPanel panel =  _windowButtonPanel;
        
//        JPanel spacingPanel = new JPanel();
//        spacingPanel.setPreferredSize(new Dimension(160, 40));
        
 //       spacingPanel.setBackground(Color.blue);
      //  panel.setBackground(Color.yellow);
        
                                                         //    col, row   numCols numRows  Wcol wrow
     
        addComponent(panel, _windowLauncherButton,   gbc,      0,     0,  1,      1,       1,   0);
        addComponent(panel, _screenCaptureButton,    gbc,      1,     0,  1,      1,       1,   0); 
        addComponent(panel, _closeButton,            gbc,      2,     0,  1,      1,       1,   0);
//      addComponent(panel, spacingPanel,   gbc,      1,     0,  1,      1,       1,   1);
               
        return;        
    }
      
    //----------------------------------------------------------------------------------------------------
    
    private void initApiMkcControlPanel()
    {
        Dimension panelDimension = new Dimension(500, 120);
        String toolTipText = null;
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.insets = new Insets(2, 2, 2, 2); 
        gbc.ipadx = 5;
        
        _apiMkcControlPanel = new JPanel();
         JPanel panel = _apiMkcControlPanel;
         panel.setPreferredSize(panelDimension);
      
        panel.setLayout(new GridBagLayout());
        panel.setBorder(_panelBorder);
  
       
        //settings panels for KC-API     
        _apiMkcSettingsLabel = new JLabel(getCenteredText("API-MKC Settings:"));
           
       //  _apiMkcBaseflowPanel = new SettingPanel("Baseflow (cfs):", 6, "Reload Baseflow");
        _apiMkcInitialStagePanel = new SettingPanel("Initial Stage(feet):", 6, "Reload", "Reload the initial stage from the time series.");      
        toolTipText = "Initial stage in feet is converted to baseflow in cfs during model run.";
        _apiMkcInitialStagePanel.getLabel().setToolTipText(toolTipText);
   
        
       // _apiMkcFfhPanel =      new SettingPanel("FFH (inches):", 6, "Reload FFH");
        _apiMkcFfhPanel =      new SettingPanel("FFH(inches):", 6, "Reload", "Reload the headwater guidance from the database.");
        toolTipText = "Headwater guidance value in inches.";
        _apiMkcFfhPanel.getLabel().setToolTipText(toolTipText);
        
        //Threshold Runoff SettingPanel
       // _apiMkcThreshRPanel =  new SettingPanel("Threshold Runoff (inches):", 6, "Reload T. Runoff");
        _apiMkcThreshRPanel =  new SettingPanel("T.Runoff(inches):", 6, "Reload", "Reload the threshold runoff.");
        toolTipText = "Threshold Runoff value in inches.";
        _apiMkcThreshRPanel.getLabel().setToolTipText(toolTipText);
  
        
        _apiMkcUseCustomModelRunTimeCheckBox = new JCheckBox("Use Custom Time", 
                                                             _apiMkcUseCustomModelRunTime);  
        _apiMkcUseCustomModelRunTimeCheckBox.setToolTipText("Sets the Model Run Start Time by overriding the selected FFH time.");                                                     
                                                             
        long latestHourTime = TimeHelper.truncateTimeInMillisToNearestHour(System.currentTimeMillis(),1);                                                                                                              
        _apiMkcCustomModelRunDateTimeTextField = new DateTimeTextField(latestHourTime, 
                                                                 AnalysisWindow.this, 
                                                                 "Model Run Start Time", 20);
        _apiMkcCustomModelRunDateTimeTextField.setAllowOnlyEvenHours(true);
        _apiMkcCustomModelRunDateTimeTextField.setPreferredSize(new Dimension(150, 20));
        _apiMkcCustomModelRunDateTimeTextField.setMaximumSize(new Dimension(150, 20));
        _apiMkcCustomModelRunDateTimeTextField.setMinimumSize(new Dimension(150, 20));
        
            
        //apply button
        _apiMkcApplySettingsButton = GuiHelper.getJButton(getVerticalText("Apply"));
        _apiMkcApplySettingsButton.setToolTipText("Applies manual changes to settings on the left.");
        
        
        //create a panel for vertical spacing purposes
        JPanel verticalSpacer = new JPanel();
        Dimension verticalSpacerDimension = new Dimension (30, 15);
        verticalSpacer.setPreferredSize(verticalSpacerDimension);
        verticalSpacer.setMaximumSize(verticalSpacerDimension);
        verticalSpacer.setMinimumSize(verticalSpacerDimension);
 
      
        // JPanel for layout purposes of the dateTimeTextField
        JPanel dateTimeTextFieldPanel = new JPanel();
        dateTimeTextFieldPanel.setLayout(new GridBagLayout());
      //  dateTimeTextFieldPanel.setBackground(Color.yellow);
        dateTimeTextFieldPanel.setPreferredSize(new Dimension (209, 30));
        
        JPanel spacer1 = new JPanel();
       // spacer1.setBackground(Color.red);
        Dimension spacer1Dimension = new Dimension (50, 30);
        spacer1.setPreferredSize(spacer1Dimension);
        spacer1.setMaximumSize(spacer1Dimension);
        spacer1.setMinimumSize(spacer1Dimension);
        
         // add components to the special dateTimeTextFieldPanel       
  																//               col,  row numCols numRows  Wcol wrow        
        addComponent(dateTimeTextFieldPanel, 
              _apiMkcCustomModelRunDateTimeTextField,               gbc,          0,     0,  3,      1,       1,   0);  
        addComponent(dateTimeTextFieldPanel, spacer1,               gbc,          3,     0,  1,      1,       0,   0);  
    
        
        
                                                                  //  col, row   numCols numRows  Wcol wrow
      
        addComponent(panel, _apiMkcSettingsLabel,           gbc,       0,     0,  2,      1,       0,   0);  
             
        addComponent(panel,
                 _apiMkcUseCustomModelRunTimeCheckBox, gbc,            0,     2,  1,      1,       0,   0);
        addComponent(panel,
                  dateTimeTextFieldPanel, gbc,                         1,     2,  3,      1,       0,   0);
    
      
        addComponent(panel, 
                _apiMkcInitialStagePanel.getLabel(),      gbc,         0,     3,  1,      1,       0,   0);
        addComponent(panel, 
                _apiMkcInitialStagePanel.getTextField(),  gbc,         1,     3,  1,      1,       0,   0);
        addComponent(panel, 
                _apiMkcInitialStagePanel.getButton(),     gbc,         3,     3,  1,      1,       0,   0);
    
        addComponent(panel, _apiMkcFfhPanel.getLabel(),       gbc,     0,     4,  1,      1,       0,   0);
        addComponent(panel, _apiMkcFfhPanel.getTextField(), gbc,       1,     4,  1,      1,       0,   0);
        addComponent(panel, _apiMkcFfhPanel.getButton(),       gbc,    3,     4,  1,      1,       0,   0);
    
        addComponent(panel, _apiMkcThreshRPanel.getLabel(),    gbc,    0,     5,  1,      1,       0,   0);
        addComponent(panel, _apiMkcThreshRPanel.getTextField(),gbc,    1,     5,  1,      1,       0,   0);
        addComponent(panel, _apiMkcThreshRPanel.getButton(),   gbc,    3,     5,  1,      1,       0,   0);
    
      //  gbc.insets = new Insets(3, 3, 3, 3); 
        addComponent(panel, _apiMkcApplySettingsButton,      gbc,      2,     3,  1,      3,       0,   0);
       
        addComponent(panel, verticalSpacer,                  gbc,      1,     6,  1,      1,       0,   0);
        
        
    } //end initApiModelPanel()
    
    //  ----------------------------------------------------------------------------------------------------
  
    private void initApiMkcStateComboBox(JComboBox comboBox)
    {
        String header = "AnalysisWindow.initApiMkcTimeComboBox(): ";
        //System.out.println(header + "starting");
        
        _reloadingApiMkcStartingStateComboBox = true;
        
        //load all of the possible values from the db
        List ffhDescriptorList = getPrimaryStreamModel().getFFHDescriptorList();
        boolean foundMatch = false;
        
        FFHDescriptor localSelectedDescriptor = null;

        // get a list of strings for this descriptor list
         
        comboBox.removeAllItems();
        
        

        // add the FFHDescriptors as items to the list
        
       // System.out.println(header + "initially selectedFFHDescriptor = " + _selectedFFHDescriptor);

        for (int i = 0; i < ffhDescriptorList.size(); i++)
        {
            FFHDescriptor descriptor = (FFHDescriptor) ffhDescriptorList.get(i);


            // if the item being added equals the previously selected item,
            // note the item.
            if (_selectedFFHDescriptor != null)
            {
                //System.out.println(header + "selectedFFHDescriptor = " + _selectedFFHDescriptor);
                //System.out.println(header + "testing equality against Descriptor = " + descriptor);
                if  
                    (
                    (!foundMatch) && 
                    (_selectedFFHDescriptor.equals(descriptor))
                    )
               
                {
                    localSelectedDescriptor = descriptor;
                    foundMatch = true;
                    //System.out.println(header + "localSelectedDescriptor = " +  localSelectedDescriptor);
                }
            }
            
            // add it to the list
            comboBox.addItem(descriptor);       
        }

         _reloadingApiMkcStartingStateComboBox = false;
  
        // select the already selected item, if there was one
        if (localSelectedDescriptor != null)
        {
            comboBox.setSelectedItem(localSelectedDescriptor);
        }
        
               
        return;

    } //end initKcApiTimeComboBox()

  //  ----------------------------------------------------------------------------------------------------
 /* 
    private long getModelRunTimeFromKcApiComboBox()
    {
        long time = 0;
        FFHDescriptor descriptor =(FFHDescriptor)_apiMkcStartingStateComboBox.getSelectedItem();
        
        if (descriptor != null)
        {
            time = descriptor.getTime();
        }

        return time;
    }
 */   
    //  ----------------------------------------------------------------------------------------------------

    private void initSacStateComboBox(JComboBox comboBox)
    {
        String header = "AnalysisWindow.initSacStateComboBox(): ";
        final int ROWS_IN_COMBOBOX_BEFORE_SCROLLING = 30;
        
        //prevent selection of the added items until it is done loading
        _reloadingSacStartingStateComboBox = true;

        comboBox.removeAllItems();
     
        List<SacSmaStateDescriptor> sacSmaStateDescriptorList = _modelManager.getSacSmaStateDescriptorListWithBogusStateIfEmpty(_showUnadjustedStates);
        
        SacSmaStateDescriptor localSelectedSacStateDescriptor = null;
        boolean foundMatch = false;
   
        //load each descriptor into the comboBox.  Also when you find the
        //selected descriptor store a reference to it.
        for (SacSmaStateDescriptor descriptor : sacSmaStateDescriptorList)
        {
            //System.out.println(header + "descriptor = " + descriptor);
 
            comboBox.addItem(descriptor);
            
            // look for the currently selected item  
            if ( (!foundMatch) &&
                    (_selectedSacStateDescriptor != null) && 
                    (descriptor.equals(_selectedSacStateDescriptor))
            )
            {
                foundMatch = true;
                localSelectedSacStateDescriptor = descriptor;    
            }        
        } //end for

        
        // set the comboBox to the selected descriptor, if it exists
        _reloadingSacStartingStateComboBox = false;

        if (localSelectedSacStateDescriptor != null)
        {
            comboBox.setSelectedItem(localSelectedSacStateDescriptor);
        }
        else
        {
            comboBox.setSelectedIndex(0);   
        }
        
        comboBox.setMaximumRowCount(ROWS_IN_COMBOBOX_BEFORE_SCROLLING);

        return;        

    }
    //----------------------------------------------------------------------------------
    private void initStatusAndReloadControls()
    {
        /* init the following controls 
        _modelRunStartTimeLabel
        _modelRunStartTimeValueLabel
        */
        Dimension buttonDimension = new Dimension(120, 30);
        _modelRunStartTimeLabel = new JLabel("Model Run Start Time: ");
        
        //initialize the model run start time value label
        JPanel panel = _buttonPanel;
        
        _modelRunStartTimeValueLabel = new JLabel();
        String dateTimeString = getDateTimeStringToMinutes(_modelStartTimeHolder.getTime());
        _modelRunStartTimeValueLabel.setText(dateTimeString);
       
        return;
    }
    //----------------------------------------------------------------------------------
    
    private void initModelControlPanel()
    {
        Dimension panelDimension = new Dimension(400, 250);
        Dimension buttonDimension = new Dimension(120, 30);
        String text = null;
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.insets = new Insets(2, 2, 2, 2); 
        gbc.weightx = 1;
        gbc.weighty = 1;
    
        _modelControlPanel = new JPanel();
        _modelControlPanel.setPreferredSize(panelDimension);
    
        JPanel panel = _modelControlPanel;
        panel.setLayout(new GridBagLayout());
        panel.setBorder(_panelBorder);

        _modelControlPanelLabel = new JLabel(getCenteredText("Model Controls:"));
        
        // init ComboBox panel
       // _modelTypeComboBoxLabel = new JLabel("Rainfall-Runoff Model:");
        _modelTypeComboBoxLabel = new JLabel("Rainfall-Runoff Model:");
        initModelComboBox();
      //  _modelTypeComboBox.setPreferredSize(new Dimension(100, 35));
        
        _uhgComboBoxLabel = new JLabel("UHG:");
        initUHGComboBox();
  
        
        // API-MKC model state comboBox related controls
        
        //state refresh controls
        
    //    _modelStateLabel = new JLabel("State:"); 
        _apiMkcStartingStateComboBox = GuiHelper.getJComboBox();
        initApiMkcStateComboBox(_apiMkcStartingStateComboBox);
        
        
        _apiMkcStateRefreshButton = GuiHelper.getJButton("Refresh State List"); 
        text = "Reload available API-MKC states from the database.";
        _apiMkcStateRefreshButton.setToolTipText(text);
        _apiMkcStateRefreshButton.setPreferredSize(buttonDimension);
        
        
        //SAC-SMA model state comboBox related controls
        _sacStartingStateComboBox = GuiHelper.getJComboBox();
        initSacStateComboBox(_sacStartingStateComboBox);
        _sacStateListRefreshButton = GuiHelper.getJButton("Refresh State List");
        text = "Reload available SAC-SMA states from the database.";
        _sacStateListRefreshButton.setToolTipText(text);
        _sacStateListRefreshButton.setPreferredSize(buttonDimension);
  
  //      JSeparator separator1 = new JSeparator();
        
        
        // The forecast length spinner
//      set up the spinner control for time
        
         
        _forecastHoursSpinnerLabel = new JLabel("Fcst Hours:");
        
		int initialValue = getPrimaryStreamModel().getForecastLengthInHours();
	    int min = 1;
        int max = getPrimaryStreamModel().getMaxForecastLengthInHours();
        int step = FORECAST_LENGTH_SPINNER_STEPS;
  
        SpinnerModel forecastHoursSpinnerModel = 
                 new SpinnerNumberModel(initialValue, min, max, step);
        
        _forecastHoursSpinner = new JSpinner(forecastHoursSpinnerModel);
        
        JPanel spinnerPanel = new JPanel(); //used to get the controls to stick together nicely      
        spinnerPanel.add(_forecastHoursSpinnerLabel);
        spinnerPanel.add(_forecastHoursSpinner);
        
        
        //------- The forecast adjustment controls
        
        // Forecast Adjustment ON/OFF controls
        _adjustmentCheckBoxLabel = new JLabel("Adjust by Observed");
        _adjustmentCheckBox = new JCheckBox();
        _adjustmentCheckBox.setSelected(getPrimaryStreamModel().isAdjustmentOn());
        
        JPanel adjustmentCheckBoxPanel = new JPanel();
        adjustmentCheckBoxPanel.add(_adjustmentCheckBoxLabel);
        adjustmentCheckBoxPanel.add(_adjustmentCheckBox);
        
        
        
        //Blending period hours Spinner
        _blendPeriodSpinnerLabel = new JLabel("Blend Ahead Hours");  
        String toolTipText =  "Note: Zero hours blended can still result in 1 \"extra\" non-blended adjustment (by pairing).";
        _blendPeriodSpinnerLabel.setToolTipText(toolTipText);
        
        int initialAdjustmentHoursValue = getPrimaryStreamModel().getBlendingHours();
        
        // Initialize spinner to select the blending hours
        min = 0;
        max = getPrimaryStreamModel().getMaxBlendingHours();
        step = 6;
        
        if (initialAdjustmentHoursValue < 0)
        {
            initialAdjustmentHoursValue = 0;
        }
        if (initialAdjustmentHoursValue > max)
        {
            max = initialAdjustmentHoursValue;
        }
     
        SpinnerModel blendSpinnerModel = new SpinnerNumberModel(initialAdjustmentHoursValue,
                min, max, step);
        _blendPeriodSpinner = new JSpinner(blendSpinnerModel);
        _blendPeriodSpinner.setToolTipText("The number of hours after the last observed input time to blend.\n" +
        "Zero hours will blend the last observed input hour if there is a match with the forecast.");
        
        
        JPanel adjustmentSpinnerPanel = new JPanel();
        adjustmentSpinnerPanel.add(_blendPeriodSpinnerLabel);
        adjustmentSpinnerPanel.add(_blendPeriodSpinner);
        
        
        // init the DateTimeTextField (our custom class)
        
        toolTipText = "Last Observed Input Time For Adjustment. Click on Text Box to edit.";
        _adjustmentLastObsInputTimeLabel = new JLabel("Last Obs. Time For Adjust");
        _adjustmentLastObsInputTimeLabel.setToolTipText(toolTipText);
        
        _adjustmentLastObsInputTimeTextField = new DateTimeTextField(getLastObservedStageTime(), 
                AnalysisWindow.this, 
                "Last Observed Input Time For Adjustment", 20);
        
        _adjustmentLastObsInputTimeTextField.setPreferredSize(new Dimension(100, 30));
        
         
   //     JSeparator separator2 = new JSeparator();
        
        JPanel modelTypePanel = new JPanel();
        
        modelTypePanel.add(_modelTypeComboBoxLabel);
        modelTypePanel.add(_modelTypeComboBox);
        modelTypePanel.add(_uhgComboBoxLabel);
        modelTypePanel.add(_uhgComboBox);
     
        modelTypePanel.setPreferredSize(new Dimension(100, 40));
       //_modelTypeComboBox.setPreferredSize(new Dimension(100, 40));
       // _modelTypeComboBoxLabel.setPreferredSize(new Dimension(50, 40));
        
                                                     //    col, row   numCols numRows  Wcol wrow
        addComponent(panel, _modelControlPanelLabel,   gbc,  0,    0,  1,      1,       1,   0);
    
        //addComponent(panel, _modelTypeComboBoxLabel,   gbc,  0,    1,  1,      1,       0,   0);
        //addComponent(panel, _modelTypeComboBox,        gbc,  1,    1,  2,      1,       0,   0);
        addComponent(panel, modelTypePanel,          gbc,   0,    1,  4,      1,       0,   0);
        
       // addComponent(panel, _uhgComboBoxLabel,         gbc,   3,    1,  1,      1,       0,   0);
      //  addComponent(panel, _uhgComboBox,              gbc,   4,    1,  1,      1,       0,   0);
       
         
        //API_MKC controls    
      
        addComponent(panel, _apiMkcStartingStateComboBox, gbc,0,     2,  2,      1,       0,   0); 
        addComponent(panel,  _apiMkcStateRefreshButton,gbc,   0,     3,  1,      1,       0,   0); 
        
        ///SAC-SMA controls
        addComponent(panel, _sacStartingStateComboBox,  gbc,  0,     2,  2,      1,       0,   0);
        addComponent(panel, _sacStateListRefreshButton, gbc,  0,     3,  1,      1,       0,   0);
   
        
        addComponent(panel, spinnerPanel, 				gbc,  1,     3,  1,      1,       0,   0);
        
        
        //    addComponent(panel,  _modelStateLabel,       gbc,    0,     3,  1,      1,       0,   0); 
        //one or the other will be visible    
        
        JSeparator separator = new JSeparator(JSeparator.HORIZONTAL);
        addComponent(panel, separator,  gbc,                  0,     4,  2,      1,       0,   0); 
        
        
        //adjustment controls
        addComponent(panel, adjustmentCheckBoxPanel,  gbc,    0,     5,  1,      1,       0,   0); 
         
        addComponent(panel, adjustmentSpinnerPanel,  gbc,    1,      5,  1,      1,       0,   0); 
        
    
        addComponent(panel,
                     _adjustmentLastObsInputTimeLabel,  gbc,   0,    6,  1,      1,       0,   0); 
        addComponent(panel,
                    _adjustmentLastObsInputTimeTextField, gbc, 1,    6,  1,      1,       0,   0); 
               
  
        return;
 
    }
 
    //----------------------------------------------------------------------------------
    private void initSacControlPanel()
    {
        Dimension panelDimension = new Dimension(500, 300);
            
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.insets = new Insets(3, 3, 3, 3); 
        gbc.weightx = 1;
        gbc.weighty = 1;
        
        
        _sacControlPanel = new JPanel();
        
        String toolTipText = null;
        
        JPanel panel = _sacControlPanel;
        panel.setPreferredSize(new Dimension(325, 300));
        panel.setLayout(new GridBagLayout());
        panel.setBorder(_panelBorder);
        
        panel.setPreferredSize(panelDimension);
            
      //  _sacControlPanelLabel = new JLabel(getCenteredText("SAC-SMA and VAR Settings:"));
        _sacControlPanelLabel = new JLabel("SAC-SMA and VAR Settings:");
      
        _sacParamsButton = new JButton("Edit Params...");
        _sacParamsButton.setToolTipText("Open the SAC-SMA Parameters Editor");
        
        _sacStateButton = new JButton("Edit States...");
        _sacStateButton.setToolTipText("Open the SAC-SMA State Editor");
        
     //   _sacMonthlyMapeValuesEditorButton = new JButton("MAPE Monthly Values...");
     //   _sacMonthlyMapeValuesEditorButton.setToolTipText("Open the MAPE Monthly Values Editor");
              
        //init the (probably temporary) Setting adjustment button.
       
        _sacLiveStateAdjustmentButton = new JButton("Live Adjust State...");
      //_sacLiveStateAdjustmentButton.setPreferredSize(new Dimension(50, 30));
        _sacLiveStateAdjustmentButton.setToolTipText("Open the Live SAC-SMA State Adjuster.");

        //check box for showing alternate current time forecasts, where current time means, at the
        //same time as the model run start time
        _sacShowAlternateForecastsCheckBox = new JCheckBox("Show Alt. Fcsts");
        _sacShowAlternateForecastsCheckBox.setToolTipText("Show All Alternate forecasts created from model states at the same time.");
   
        _sacShowAlternateForecastsCheckBox.addActionListener(new ShowAlternateForecastsCheckBoxListener());
           
        _sacShowAlternateForecastsCheckBox.setSelected(true);
        _sacShowAlternateForecastsCheckBox.doClick(); //change it back to false
       
        
        _sacShowPastForecastsCheckBox = new JCheckBox("Show Past Fcsts");
        _sacShowPastForecastsCheckBox.setToolTipText("<HTML> Show past forecasts created from all displayed model states. <BR>" +
                                                          "These are to be shown at the selected intervals <BR> up to the selected number of hours in the past. </HTML> ");
        
        _sacShowPastForecastsCheckBox.setSelected(true);
        _sacShowPastForecastsCheckBox.doClick(); //change it back to false
     
        _sacShowPastForecastsCheckBox.addActionListener(new ShowPastForecastsCheckBoxListener());
        
        //Button for making a copy of currently-selected state
        _sacSetVarInitialStateButton = new JButton("Reset VAR State...");
        _sacSetVarInitialStateButton.setToolTipText("Reset the VAR Starting State");
        
        //Hours back setting panel
        toolTipText = "Set the number of hours back in the past\n for which to show forecasts from prior states.";
        SettingPanel hoursBackSettingPanel = new SettingPanel("Fcst Hours In Past:", 3, "Display", toolTipText);
        hoursBackSettingPanel.getTextField().setToolTipText(toolTipText);
        
          
        //Interval setting panel
        toolTipText = "Set the interval in hours\n for which to show forecasts from prior states.";
        SettingPanel intervalSettingPanel = new SettingPanel("Fcst Interval:", 3, "Display", toolTipText);
        intervalSettingPanel.getTextField().setToolTipText(toolTipText);
        
        intervalSettingPanel.getButton().setVisible(false);
     
        
        ActionListener listener = new SetHoursBackAndIntervalHoursForPreviousForecastsActionListener(
                                                                    hoursBackSettingPanel.getTextField(),
                                                                    intervalSettingPanel.getTextField());
        
        hoursBackSettingPanel.getButton().addActionListener(listener);
        
        hoursBackSettingPanel.getTextField().setText(String.valueOf(24));      
        intervalSettingPanel.getTextField().setText(String.valueOf(6));
        //the interval and the hoursBack will share a button, the one that belongs to hoursBackSettingPanel.
        //the interval setting panel's button will be invisible.
        
        //panel to take up space
        
   
//      Note: setMinimumSize() does not work for this purpose, use setPreferredSize() instead
         
        JPanel verticalSpaceFiller1 = new JPanel();
        verticalSpaceFiller1.setPreferredSize(new Dimension(50, 50));
        JPanel verticalSpaceFiller2 = new JPanel();
        verticalSpaceFiller2.setPreferredSize(new Dimension(85, 150));
        JPanel horizontalSpaceFiller1 = new JPanel();
        horizontalSpaceFiller1.setPreferredSize(new Dimension(85, 100));
        
        JPanel verticalSpaceFiller3 = new JPanel();
        verticalSpaceFiller2.setPreferredSize(new Dimension(85, 150));
   
        //just to make things look pretty
      //  gbc.insets = new Insets(5, 5, 5, 5); 
        
        //Panel label at top
                                                                  // col,    row   numCols numRows  Wcol wrow
        addComponent(panel, _sacControlPanelLabel,         gbc,      0,     0,     1,      1,       1,   0);      
        
        
        //check boxes across 2nd row
        addComponent(panel, 
                _sacShowAlternateForecastsCheckBox,        gbc,       0,     1,     1,      1,       1,   0);


        addComponent(panel, 
                   _sacShowPastForecastsCheckBox,          gbc,       1,     1,   1,      1,       1,   0);

           
        //hours back controls  across 3rd row
        JPanel hoursBackPanel = new JPanel();
        hoursBackPanel.setLayout(new GridBagLayout());

        hoursBackSettingPanel.getTextField().setPreferredSize(new Dimension(50, 50));

        addComponent(hoursBackPanel, 
                hoursBackSettingPanel.getLabel(),    gbc,           0,     0,   1,      1,       1,   0);
        addComponent(hoursBackPanel, 
                hoursBackSettingPanel.getTextField(),  gbc,         1,     0,   1,      1,       1,   0);

  
        
        //interval in hours controls
        JPanel intervalPanel = new JPanel();
        
        intervalPanel.setLayout(new GridBagLayout());
        
        intervalSettingPanel.getTextField().setPreferredSize(new Dimension(50, 50));
         
        addComponent(intervalPanel, 
                intervalSettingPanel.getLabel(),     gbc,            0,     0,   1,      1,       1,   0);
        addComponent(intervalPanel, 
                intervalSettingPanel.getTextField(),  gbc,           1,     0,   1,      1,       1,   0);

        
        
        //add panels to the 3rd row
        addComponent(panel,  hoursBackPanel,    gbc,                0,     2,   1,      1,       1,   0);
        addComponent(panel,  intervalPanel,    gbc,                 1,     2,   1,      1,       1,   0);

        
         //add button across 4th row
        
        
         JPanel buttonPanel = new JPanel();
         hoursBackSettingPanel.getButton().setPreferredSize(new Dimension(100, 30));
         JPanel leftFillerPanel = new JPanel();
         leftFillerPanel.setPreferredSize(new Dimension(90, 30));
         JPanel rightFillerPanel = new JPanel();
         rightFillerPanel.setPreferredSize(new Dimension(90, 30));
      
         addComponent(buttonPanel, 
                 leftFillerPanel,                       gbc,         0,     0,   1,      1,       1,   0);
         addComponent(buttonPanel, 
                 hoursBackSettingPanel.getButton(),     gbc,         0,     1,   1,      1,       1,   0);
         addComponent(buttonPanel, 
                 rightFillerPanel,                       gbc,         0,     2,   1,      1,       1,   0);
     
         
         addComponent(panel, 
                 buttonPanel,     gbc,         0,     3,   3,      1,       1,   0);
  
         
                                                      //      col,  row numCols numRows  Wcol wrow       
        addComponent(panel, _sacParamsButton,       gbc,       0,     4,  1,      1,       1,   0);  
              
        addComponent(panel, _sacStateButton,        gbc,       1,     4,  1,      1,       1,   0);
        
      
        
        addComponent(panel,
                _sacSetVarInitialStateButton,        gbc,       0,    5,  1,      1,       1,   0);
  
        
        if (_allowLiveSacAdjustment)
        {
   
            addComponent(panel,
                _sacLiveStateAdjustmentButton,       gbc,      1,     5,  1,      1,       1,   0);
        }        
      
   //     addComponent(panel,
    //            _sacMonthlyMapeValuesEditorButton,   gbc,       0,     5,  1,      1,       1,   0);
         
        
    
    } //end  initSacButtonPanel()
    
    // ------------------------------------------------------------
    // ------------------------------------------------------------
    
    private void initModelChoice()
    {
     //   trace();
        
        String header = "AnalysisWindow.initModelChoice(): ";
        int initialIndex = 0; 
        String initialSelectedTypeName = getPrimaryStreamModel().getRainfallRunoffModelTypeName();

       // System.out.println(header + "initialSelectedTypeName = " + initialSelectedTypeName);

        RainfallRunoffModelType[] modelTypeArray = 
                        RainfallRunoffModelType.getModelTypeArray();
                        
        //find initialSelectedType         
        for (int i = 0; i < modelTypeArray.length; i++)
        {
            if (modelTypeArray[i].getName().equals(initialSelectedTypeName))
            {
               initialIndex = i;
               break;    
            }
        }
                  
        _modelTypeComboBox.setSelectedIndex(initialIndex);
        
        return;
    }
    
    // ------------------------------------------------------------

    private void initModelComboBox()
    {
        // note: at some point might want to separate the presentation of the
        // name of the model from the built-in model type name
           
        RainfallRunoffModelType[] modelTypeArray = 
                    RainfallRunoffModelType.getModelTypeArray();
        
        _modelTypeNameArray = new String[modelTypeArray.length];
        
          
        // add the choices to the string array 
        for (int i = 0; i < modelTypeArray.length; i++)
        {
            _modelTypeNameArray[i] = modelTypeArray[i].getName();    
        }
        
        //create the comboBox with all the choices in it
        _modelTypeComboBox = GuiHelper.getJComboBox(_modelTypeNameArray);
       
     
        return;
    }   
    
    // ------------------------------------------------------------
    private void initUHGChoice()
    {
     //   trace();
        
        String header = "AnalysisWindow.initUHGChoice(): ";
        int initialIndex = 0; 
        String initialSelectedTypeName = getPrimaryStreamModel().getRainfallRunoffModelTypeName();

       // System.out.println(header + "initialSelectedTypeName = " + initialSelectedTypeName);

        RainfallRunoffModelType[] modelTypeArray = 
                        RainfallRunoffModelType.getModelTypeArray();
                        
        //find initialSelectedType         
        for (int i = 0; i < modelTypeArray.length; i++)
        {
            if (modelTypeArray[i].getName().equals(initialSelectedTypeName))
            {
               initialIndex = i;
               break;    
            }
        }
                
        _uhgComboBox.setSelectedIndex(initialIndex);
    
        return;
    }
    
//  -----------------------------------------------------------------
    
    private void initUHGComboBox()
    {
          
        String[] uhgNamesArray = getPrimaryStreamModel().getUnitHydrographNameArray();
                           
        //create the comboBox with all the choices in it
        _uhgComboBox = GuiHelper.getJComboBox(uhgNamesArray);
       
        refreshUhgSelection();
        
     
        return;
    }   
    
	//-----------------------------------------------------------------
    private void refreshUhgSelection()
    {
        String selectedUHGModelName =getPrimaryStreamModel().getUnitHydrographSelectionName();
        
        _uhgComboBox.setSelectedItem(selectedUHGModelName);
    
    }
    //  -----------------------------------------------------------------
    
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
    
	private void initTitle()
    {
        String locationId = getPrimaryStreamModel().getLocationId();
        String locationName = getPrimaryStreamModel().getLocationName();
        String basinId = getPrimaryStreamModel().getBasinId();
        
        String rainfallRunoffModelName = getPrimaryStreamModel().getRainfallRunoffModelTypeName();
        
        String titleString = "SSHP Analysis Window - Modeling " + locationId + ", " + locationName +
                             " With BASIN : " + basinId + " using the " + rainfallRunoffModelName + " model";
        setTitle(titleString);
        
        return;
    }
    
    
    
    //-----------------------------------------------------------------
	private void initPrecipCanvas(int x, int y,
								  int width, int height,
								  TimeHolder modelStartTimeHolder)
	{
		//_precipCanvas = new PrecipCanvas(x, y, width, height);
		_precipCanvas = new TsPaintableCanvas(MeasuringUnit.inches,
									 x, y,
									 width, height);
		
        _precipCanvas.setPreferredSize(new Dimension(width, height));
        _precipCanvas.setMinimumSize(new Dimension(100, 10));
    
	
		MeasuringUnit unit = _precipCanvas.getDisplayedMeasuringUnit();
        
        Color outlineColor = Color.WHITE;
        
        // background painter
		_precipBackgroundPainter =
		        new TsBackgroundPainter(outlineColor, _precipCanvas, "inches", "mm");
        
        _precipValueMapper = new UnitValueMapper(MeasuringUnit.inches,
                                                 MeasuringUnit.mm);
                                                              
        _precipBackgroundPainter.setRightLabelValueMapper(_precipValueMapper);     
        _precipCanvas.addCanvasPainter(_precipBackgroundPainter);
        
        
        
        // precip bar painters ------------------------------
        
//      add the precip painter
        Color barColor = Color.BLUE;
        _precipPainter = new PrecipPainter(barColor,
                                             outlineColor,
                                             getPrimaryStreamModel().getPrecipTimeSeriesHolder(),
                                             _showPrecipAmounts,
                                             _precipCanvas);
        _precipCanvas.addTsCanvasPainter(_precipPainter);
        
        //add the precip text total painter
        _precipTextTotalPainter =
                                new PrecipTotalTextPainter(Color.yellow, 
                                        getPrimaryStreamModel().getPrecipTimeSeriesHolder(),
                                        _showPrecipAmounts,
                                        _precipCanvas);
        _precipCanvas.addCanvasPainter(_precipTextTotalPainter);
        
        
        // add the prior runoff painter
        Color runoffColor = Color.YELLOW;
        PrecipPainter priorRunoffPrecipPainter =  new PrecipPainter(runoffColor,
                                                      outlineColor,
                                                      getPrimaryStreamModel().getPriorRunoffTimeSeriesHolder(),
                                                      false,
                                                      _precipCanvas);
                                                
        priorRunoffPrecipPainter.setShouldDrawBars(false);
                                                
        _precipCanvas.addTsCanvasPainter(priorRunoffPrecipPainter);
        
        
        // add the runoff painter
        Color runoffColor2 = Color.MAGENTA;
        PrecipPainter runoffPrecipPainter =  new PrecipPainter(runoffColor2,
                                                outlineColor,
                                                getPrimaryStreamModel().getRunoffTimeSeriesHolder(),
                                                false,
                                                _precipCanvas);
                                                
        runoffPrecipPainter.setShouldDrawBars(false);
                                                
        _precipCanvas.addTsCanvasPainter(runoffPrecipPainter);
          
          
        // Model time line color
        Color timeLineColor = Color.MAGENTA;
        TimeLinePainter modelStartTimeLinePainter = 
                           new TimeLinePainter(timeLineColor, 
                                               _modelStartTimeHolder,
                                               _precipCanvas);
                                        
        _precipCanvas.addCanvasPainter(modelStartTimeLinePainter);  
          
        
		
		// set the display time window
        setTimeWindowAroundModelStartTime(_precipCanvas);
     

		_precipCanvas.setTitle("1-Hour Mean Areal Precipitation Time Series");

		
	} //end initPrecipCanvas
				
//	-----------------------------------------------------------------
	private void initStageCanvas(int x, int y,
								 int width, int height,
								 TimeHolder modelStartTimeHolder)
	{
		_stageCanvas = new TsPaintableCanvas(MeasuringUnit.feet,
									x, y,
                                    width, height);
                                    
        _stageCanvas.setPreferredSize(new Dimension(width, height));
        _stageCanvas.setMinimumSize(new Dimension(100, 10));
  
		Color outlineColor = Color.WHITE;
        
		// background Painter
		_stageBackgroundPainter =
							new TsBackgroundPainter(outlineColor, _stageCanvas, "feet", "cfs");
                                                                  
                 
        
        _stageValueMapper = getPrimaryStreamModel().getStageToFlowValueMapper();
        
        _stageBackgroundPainter.setRightLabelValueMapper(_stageValueMapper);
		_stageCanvas.addCanvasPainter(_stageBackgroundPainter);


        setUpSigRiverLevelDrawing();
       
        setUpPrimaryHydrographPainters();
		
/*     
		// observed hydrograph painter
		Color obsColor = Color.yellow;
		_observedStageTsPainter = new StagePainter(obsColor,
														getPrimaryStreamModel().getObservedStageTimeSeriesHolder(),
													    _stageCanvas);
		_stageCanvas.addTsCanvasPainter(_observedStageTsPainter);


		//  simulated hydrograph painter
        Color simulationColor = Color.blue;
        _simulatedStageTsPainter = new StagePainter(simulationColor,
                                                        getPrimaryStreamModel().getSimulationStageTimeSeriesHolder(),
                                                        _stageCanvas, true
                                                        );
        _simulatedStageTsPainter.setPointString("X");                                               
        _stageCanvas.addTsCanvasPainter(_simulatedStageTsPainter);
      //  _simulatedStageTsPainter.setShiftAmount(new Point(-4, 3));
        
        

        // forecast hydrograph painter
		Color forecastColor = Color.green;
	    _forecastPainter = new StagePainter(forecastColor,
														getPrimaryStreamModel().getForecastStageTimeSeriesHolder(),
													    _stageCanvas, true
                                                        );
        _forecastPainter.setPointString("X");
                                                       
		_stageCanvas.addTsCanvasPainter(_forecastPainter);
        
        //prior forecast hydrograph painter
       // Color priorForecastColor = new Color(154, 205, 50); //yellow green
        Color priorForecastColor = new Color(160,32,240); //purple
        _priorForecastPainter = new StagePainter(priorForecastColor,
                                                        getPrimaryStreamModel().getPriorForecastStageTimeSeriesHolder(),
                                                        _stageCanvas, true
                                                        );
         _priorForecastPainter.setPointString("X");
        _stageCanvas.addTsCanvasPainter(_priorForecastPainter);
        
  */
        

        // Model time line color
        Color timeLineColor = Color.MAGENTA;
        TimeLinePainter modelStartTimeLinePainter = 
                    new TimeLinePainter(timeLineColor, 
                                        _modelStartTimeHolder,
                                        _stageCanvas);
                                        
        _stageCanvas.addCanvasPainter(modelStartTimeLinePainter);
     
	
		
		//int intervalInHours = 1;

		long startTime = modelStartTimeHolder.getTime() - (24 * _millisPerHour);
		int hoursForward = 72;
		long endTime = startTime + (hoursForward * _millisPerHour);

		_stageCanvas.setTimeWindow(startTime, endTime);

        // set the display time window
        setTimeWindowAroundModelStartTime(_stageCanvas);

		_stageCanvas.setTitle("Forecast Stage Time Series");

	
	} //end initStageCanvas
    
//  -----------------------------------------------------------------
    private void setUpHydrographTimeSeriesPainters()
    {
        
        String header = "AnalysisWindow.setUpHydrographTimeSeriesPainters(): ";
       // System.out.println(header + "begun");

      //  _stageCanvas.removeTsCanvasPaintersBetweenInclusive(_forecastPainter, _priorForecastPainter);
        
        _stageCanvas.removeTsCanvasPaintersBetweenInclusive(_observedStageTsPainter, _priorForecastPainter);
        
        setUpPrimaryHydrographPainters();
        setUpAdditionalHydrographPainters();
    }
//  -----------------------------------------------------------------
    private void setUpPrimaryHydrographPainters()
    {
        String header = "AnalysisWindow.setUpPrimaryHydrographPainters(): ";
        
       // observed hydrograph painter
        Color obsColor = Color.yellow;
            
        _observedStageTsPainter = new StagePainter(obsColor,
                                                        getPrimaryStreamModel().getObservedStageTimeSeriesHolder(),
                                                        _stageCanvas);
        _stageCanvas.addTsCanvasPainter(_observedStageTsPainter);

   //     System.out.println(header + " My _observedStageTsPainter is  " + _observedStageTsPainter);

        

        //  simulated hydrograph painter
        Color simulationColor = Color.blue;
        _simulatedStageTsPainter = new StagePainter(simulationColor,
                                                        getPrimaryStreamModel().getSimulationStageTimeSeriesHolder(),
                                                        _stageCanvas, true
                                                        );
        _simulatedStageTsPainter.setPointString("X");                                               
        _stageCanvas.addTsCanvasPainter(_simulatedStageTsPainter);
      //  _simulatedStageTsPainter.setShiftAmount(new Point(-4, 3));
        
        

        // forecast hydrograph painter
        Color forecastColor = Color.green;
        _forecastPainter = new StagePainter(forecastColor,
                                                        getPrimaryStreamModel().getForecastStageTimeSeriesHolder(),
                                                        _stageCanvas, true
                                                        );
        _forecastPainter.setPointString("X");
                                                       
        _stageCanvas.addTsCanvasPainter(_forecastPainter);
        
        //prior forecast hydrograph painter
       // Color priorForecastColor = new Color(154, 205, 50); //yellow green
        Color priorForecastColor = new Color(160,32,240); //purple
        _priorForecastPainter = new StagePainter(priorForecastColor,
                                                        getPrimaryStreamModel().getPriorForecastStageTimeSeriesHolder(),
                                                        _stageCanvas, true
                                                        );
         _priorForecastPainter.setPointString("X");
        _stageCanvas.addTsCanvasPainter(_priorForecastPainter);
        
    } //end setUpPrimaryHydrographPainters() 
//  -----------------------------------------------------------------
 
    private void setUpAdditionalHydrographPainters()
    {
        String header = "AnalysisWindow.setUpAdditionalHydrographPainters(): " ;
        StreamModel primaryStreamModel = getPrimaryStreamModel();
                 
 //       System.out.println(header + " _forecastPainter " + _forecastPainter + " _priorForecastPainter = " + _priorForecastPainter);
        
        //add in the new alternate painters to the _stageCanvas   
        int index = 0;
        
        StagePainter painterToAddAfter = _forecastPainter;
                
	    for (StreamModel streamModel : _modelManager.getCompleteStreamModelList())
        {
            
            if (streamModel != primaryStreamModel)
            {
                String source = streamModel.getSacState().getSource();
                
                
                Color simulationColor = getColorByIndex(index);
              //  Color simulationColor = getColorBySource(source);
                StagePainter painter = new StagePainter(simulationColor,
                                                        streamModel.getSimulationStageTimeSeriesHolder(),
                                                        _stageCanvas, true);
                
                String pointString = getPointStringByStreamModel(streamModel, index);
                
                //System.out.println(header + " pointString = " + pointString + " for streamModel = " + streamModel);
                
                painter.setPointString(pointString);                                            
                _stageCanvas.addTsCanvasPainterAfter(painter, painterToAddAfter);
                painterToAddAfter = painter;
                
                //System.out.println(header + "adding painter " + painter + " to the stage canvas.");
          
                index++;
            }
        }
        
        
        return;
    }
    
//  -----------------------------------------------------------------
    private String getPointStringByStreamModel(StreamModel streamModel, int index)
    {
        StringBuffer resultBuffer = new StringBuffer();
      
        /*
        if (streamModel.isAlternateStreamModel())
        {
            resultBuffer.append("A"); //alternate
        }
        else
        {
            resultBuffer.append("P"); //primary
        }
        */
        
        //local, adjusted or unadjusted
        String sourceOfState = streamModel.getSacState().getSource();
        
        resultBuffer.append(sourceOfState.substring(0,1) + index);
        
        return resultBuffer.toString();     
    }
//  -----------------------------------------------------------------
	private Color getColorByIndex(int index)
    {
         
	    final Color[] colorArray =  {Color.WHITE, Color.ORANGE, Color.PINK, Color.MAGENTA, Color.CYAN, Color.YELLOW };
        
        int mod = index % colorArray.length;
        
        return colorArray[mod];
    }
//  ----------------------------------------------------------------
	private Color getColorByName(String colorName)
    {
	      return _colorMap.get(colorName.toUpperCase());
    }
//  -----------------------------------------------------------------

    private Color getColorBySource(String source)
    {
        Color color = null;
        
        
        if (source.equalsIgnoreCase("UNADJ_VAR"))
        {
            color = getColorByName("cyan");
        }
        
        else if (source.equalsIgnoreCase("ADJ_VAR"))
        {
            color = getColorByName("DarkOrange");
        }
        else if (source.equalsIgnoreCase("LOCAL"))
        {
            color = getColorByName("DarkPurple");
        }
        else if (source.equalsIgnoreCase("RFC"))
        {
            color =  getColorByName("HotPink");
        }
        
        else 
        {
            color = Color.pink;
        }
       
        if (color == null)
        {
            color = Color.white;
        }
        
        return color;
    }
//  -----------------------------------------------------------------

    private void setUpSigRiverLevelDrawing()
    {
//      set up painters for significant stages  
        SigRiverLevels sigStages = getPrimaryStreamModel().getSigRiverLevels();
        
        

        // action stage painter
        if ((sigStages != null) && (sigStages.hasActionStage()))
          
        {
            Color actionStageColor = Color.yellow;
            Measurement actionStageMeasurement = new Measurement(sigStages.getActionStage(),
                                                                 MeasuringUnit.feet);
            SigStageLinePainter actionStageLinePainter =
                     new SigStageLinePainter(actionStageColor,
                                             actionStageMeasurement,
                                             _stageCanvas);                                                 
        
            _stageCanvas.addTsCanvasPainter(actionStageLinePainter);
        }
        
//      flood stage painter
        if ((sigStages != null) && (sigStages.hasFloodStage()))      
        {
            //this is a darker orange, more easily distinguished
            //from Color.yellow than Color.orange
            Color betterOrange = new Color(0xFF8C00);
            Color floodStageColor = betterOrange ; 
            Measurement floodStageMeasurement = new Measurement(sigStages.getFloodStage(),
                                                                      MeasuringUnit.feet);
            
            
            _floodStageLinePainter =
                          new SigStageLinePainter(floodStageColor,
                                                  floodStageMeasurement,
                                                  _stageCanvas);                                                    
        
            _stageCanvas.addTsCanvasPainter(_floodStageLinePainter);
        }

        if ((sigStages != null) && (sigStages.hasModerateFloodStage()))      
        {
            Color moderateFloodStageColor = Color.red;
            Measurement moderateFloodStageMeasurement = 
                new Measurement(sigStages.getModerateFloodStage(),
                                MeasuringUnit.feet);
            
            SigStageLinePainter moderateFloodStageLinePainter =
                          new SigStageLinePainter(moderateFloodStageColor,
                                          moderateFloodStageMeasurement,
                                                  _stageCanvas);                                                    
        
            _stageCanvas.addTsCanvasPainter(moderateFloodStageLinePainter);
        }

        if ((sigStages != null) && (sigStages.hasMajorFloodStage()))      
        {
            Color majorFloodStageColor = Color.magenta;
            Measurement majorFloodStageMeasurement = 
                new Measurement(sigStages.getMajorFloodStage(),
                                MeasuringUnit.feet);
            
            _majorFloodStageLinePainter =
                          new SigStageLinePainter(majorFloodStageColor,
                                                  majorFloodStageMeasurement,
                                                  _stageCanvas);                                                    
        
            _stageCanvas.addTsCanvasPainter(_majorFloodStageLinePainter);
        }
        
    }
//	-----------------------------------------------------------------
    private void recenterTimeWindowsAroundModelStartTime()
    {
         setTimeWindowAroundModelStartTime(_precipCanvas);
         setTimeWindowAroundModelStartTime(_stageCanvas);       
    }

//  -----------------------------------------------------------------

    private void setTimeWindowAroundModelStartTime(TsPaintableCanvas canvas)
    {
        int hoursBack = _hoursToShowBeforeModelStartTime;
        int hoursForward = _hoursToShowAfterModelStartTime;

        long startTime = _modelStartTimeHolder.getTime() - 
                         (hoursBack * _millisPerHour);
           
        long endTime = startTime + (hoursForward * _millisPerHour);
                      canvas.setTimeWindow(startTime, endTime);
        
    }
  
//  -----------------------------------------------------------------
    		

	public void updateDataPointText(DataPoint dataPoint, ValueMapper valueMapper, TsPaintableCanvas canvas)
	{
		//System.out.println("in TsMainWindow.update(DataPoint dp)");
	
        String leftUnitString = canvas.getDisplayedMeasuringUnit().toString();
        String rightUnitString = valueMapper.getOutputMeasuringUnit().toString();
    
        String message = getDataPointString(dataPoint, valueMapper, leftUnitString, rightUnitString);
    
		//textField.setText(message);
        
        canvas.setToolTipText(message);
         
  	    return;
	}		

	//-----------------------------------------------------------------
    private void setComponentVisibility()
    {
        RainfallRunoffModelType modelType = getPrimaryStreamModel().getRainfallRunoffModelType();
        
        boolean apiMkcVisible = false;
        boolean sacSmaVisible = false;
        
        if (modelType == RainfallRunoffModelType.API_MKC)
        {
            apiMkcVisible = true;
            sacSmaVisible = false;       
        } 
        else if (modelType == RainfallRunoffModelType.SAC_SMA)
        {
            apiMkcVisible = false;
            sacSmaVisible = true;
        }
        
        _apiMkcControlPanel.setVisible(apiMkcVisible);  
        _apiMkcStartingStateComboBox.setVisible(apiMkcVisible); 
     //   _modelStateLabel.setVisible(apiMkcVisible);
        _apiMkcStateRefreshButton.setVisible(apiMkcVisible);

        _sacControlPanel.setVisible(sacSmaVisible);
        _sacStartingStateComboBox.setVisible(sacSmaVisible);
        _sacStateListRefreshButton.setVisible(sacSmaVisible);
        
        
        
        return;
    }

    //---------------------------------------------------------------------- 
    private String getDateTimeStringToMinutes(long time)
    {

        SimpleDateFormat sdf = new SimpleDateFormat("yyyy/MM/dd HH:mm");
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        String timeString = sdf.format(new Date(time)) + " Z";

        return timeString;
    }

   //---------------------------------------------------------------------- 
    private long getTimeOfLatestHour()
    {
        long time = System.currentTimeMillis();    
            
        time = TimeHelper.truncateTimeInMillisToNearestHour(time, 1);
        
        
        return time;
    }
    
    // ---------------------------------------------------------------------
    private long getLastObservedStageTime()
    {
        long time = -1;
        
        try
        {
           time = getPrimaryStreamModel().getObservedStageTimeSeriesHolder().getTimeSeries().getEndTime();
        }
        catch (Throwable t)
        {
            System.out.println("getLastObservedStageTime(): " + "caught a throwable, but don't worry");
        }
        
        return time;
    }
    
    //------------------------------------------------------------------------
    
//  //---------------------------------------------------------------------- 
     
    private String getDataPointString(DataPoint dataPoint,
                                      ValueMapper valueMapper, 
                                      String leftUnitString,
                                      String rightUnitString)
	{
		NumberFormat nf = new DecimalFormat("####.##");
		String  valueString = nf.format(dataPoint.getY());
    
        String mappedValueString = "";
        
        if (valueMapper != null)
        {
            MeasuringUnit inputUnit = valueMapper.getInputMeasuringUnit();
            Measurement inputMeasurement = new Measurement(dataPoint.getY(), inputUnit);
  
            Measurement mappedMeasurement = valueMapper.getResultMeasurement(inputMeasurement);
            
            double mappedValue = mappedMeasurement.getValue();
            
            mappedValueString = nf.format(mappedValue);
		}

		long time = (long) Math.floor( dataPoint.getX());

        String timeString = getDateTimeStringToMinutes(time);

		String dataPointString = timeString  + ", " +
									 valueString + " " + leftUnitString + ", "  +
                                     mappedValueString + " " + rightUnitString;

		return dataPointString;
	}
	
	
   // event handler --------------------------------------------------

   
    
    //---------------------------------------------------------------------- 

	private void replaceTimeSeriesValue(RegularTimeSeriesHolder timeSeriesHolder,
										TimeIntervalMeasurement newMeasurement)
	{
	    //String header = "AnalysisWindow.replaceTimeSeriesValue(): ";
			
		RegularTimeSeries timeSeries =
                       timeSeriesHolder.getTimeSeries();
		      
      //  System.out.println(header + "before conversion, newMeasurement is " + newMeasurement);
	
        newMeasurement = TimeIntervalMeasurement.getConvertedCopy(newMeasurement,
                                                                  timeSeries.getMeasuringUnit());
    
		
      //  System.out.println(header + "after conversion, newMeasurement is " + newMeasurement);

  	  long endTimeInMillis = newMeasurement.getEndTime();
		
	  timeSeries.setMeasurementByTime(newMeasurement, endTimeInMillis);
		
	} //end replaceTimeSeriesValue()

    //---------------------------------------------------------------------- 


    private void replaceTimeSeriesValue(RegularTimeSeriesHolder timeSeriesHolder,
                                        AbsTimeMeasurement newMeasurement)
    {
        String header = "AnalysisWindow.replaceTimeSeriesValue(): ";
            
        RegularTimeSeries timeSeries =
                       timeSeriesHolder.getTimeSeries();
                       
                       
      //  System.out.println(header + "timeSeries = " + timeSeries);               
        
        if (timeSeries != null)
        {
            newMeasurement = AbsTimeMeasurement.getConvertedCopy(newMeasurement,
                                                                  timeSeries.getMeasuringUnit());
         
            timeSeries.setMeasurementByTime(newMeasurement, newMeasurement.getTime());
        }
        else
        {
           // System.err.println(header + " time series is null ");     
        }
        
      
    } //end replaceTimeSeriesValue()
	
    // ----------------------------------------------------------------
    private void runModel()
    {
        
    //   String header = "AnalysisWindow.runModel(): ";       
    //   System.out.println(header + "inside the method");
        _modelManager.runStreamModels();
    
        redrawCanvases();
        
        announceModelRun();
        
    }
    // ----------------------------------------------------------------

    private void announceModelRun()
    {
        announceTimeSeriesChange("Model Run");         
    }
  
    //-----------------------------------------------------------------
  
    private void announceForecastStageEdited()
    { 
        String message = "Manually Edited Fcst Stage";
        TimeSeriesEvent event = new TimeSeriesEvent(message, AnalysisWindow.this);
       
        getPrimaryStreamModel().getForecastStageTimeSeriesHolder().forwardEvent(event);
    }
    
    //  -----------------------------------------------------------------
    private void announceTimeSeriesChange(String message)
    {
        TimeSeriesEvent event = new TimeSeriesEvent(message, AnalysisWindow.this);
        
        getPrimaryStreamModel().getPrecipTimeSeriesHolder().forwardEvent(event);
        getPrimaryStreamModel().getForecastStageTimeSeriesHolder().forwardEvent(event);
        
        getPrimaryStreamModel().getEvaporationTimeSeriesHolder().forwardEvent(event);
        getPrimaryStreamModel().getPriorRunoffTimeSeriesHolder().forwardEvent(event);
        
        getPrimaryStreamModel().getRunoffTimeSeriesHolder().forwardEvent(event);
        
    }
    
    //  -----------------------------------------------------------------
    
    private void setModelTypeToComboBoxSelection()
    {     
  
            int selectedIndex = _modelTypeComboBox.getSelectedIndex();

            RainfallRunoffModelType[] typeArray = 
                                     RainfallRunoffModelType.getModelTypeArray();
           
             
            RainfallRunoffModelType type = typeArray[selectedIndex];
           
            changeModelType(type);
    }
    
    //  -----------------------------------------------------------------
   
    private void changeModelType(RainfallRunoffModelType newType)
    {
    
        RainfallRunoffModelType oldType = getPrimaryStreamModel().getRainfallRunoffModelType();
        
        String header = "AnalysisWindow.changeModelType()";
     //   System.out.println(header +": changing type to " + newType); 
        
       // _tracer.trace("in " + header);
        
        
        
        boolean success = getPrimaryStreamModel().setRainfallRunoffModelType(newType);
        
        if (success)
        {
            //get rid of old alternate models from the ModelManager
            _modelManager.removeAllAlternateStreamModels();
            setUpHydrographTimeSeriesPainters();
            
       
            if ((oldType != null) && (oldType==newType))
            {
               //System.out.println(header + "changing to same RR model type");
            }
    
            setComponentVisibility();
             
            // the title needs to update because the
            // model may have changed
            initTitle(); 
                       
            // take any already graphically set parameters and put them back into the
            // streamModel to be used for model runs            
            updateModelSettingsFromGui();  
    
            if (newType == RainfallRunoffModelType.API_MKC)
            {    
                setApiMkcModelState();
            }
            else if (newType == RainfallRunoffModelType.SAC_SMA)
            {
                setSacModelState();
            }          
                       
            //recenter the canvases around the model run start time
            recenterTimeWindowsAroundModelStartTime();
    
            //refresh the selected Unit HydroGraph
            refreshUhgSelection();
            
            //run the stream model again            
            //runModel(); 
        }
        else //failed, so change it back to MKC_API
        {
             
            String locationId = getPrimaryStreamModel().getLocationId();
            
            String message = "Location: " + locationId +
                            " is not configured properly to run the SAC-SMA.\n" +
                            " Please add a record in the SSHPConfig Dialog to configure it.";
            
            String dialogTitle = locationId + " Configuration Message";
            DialogHelper.displayMessageDialog(this, message, dialogTitle);  
            
            int index = getIndexOfModelType(RainfallRunoffModelType.API_MKC);
            _modelTypeComboBox.setSelectedIndex(index);
       
        }
        return;  
    }
    //-----------------------------------------------------------------
    private int getIndexOfModelType(RainfallRunoffModelType type)
    {
       int index = -1;
       
       for (int i = 0; i < _modelTypeNameArray.length; i++)
       {
           if (type.getName().equalsIgnoreCase(_modelTypeNameArray[i]))
           {
              index = i;    
           }    
       } 
       
       return index;
    }
    //  -----------------------------------------------------------------
    
    private void setUnitHydrographToComboBoxSelection()
    {     
        
        Object selectedItem = _uhgComboBox.getSelectedItem();
        
        String selectedUnitHydrographName = (String) selectedItem;
        
        getPrimaryStreamModel().changeUnitHydrograph(selectedUnitHydrographName);
        
        runModel();
    }
    
    //-----------------------------------------------------------------
   
    private void updateModelSettingsFromGui()
    {
        RainfallRunoffModelType modelType = 
                   getPrimaryStreamModel().getRainfallRunoffModelType();

        if (modelType == RainfallRunoffModelType.API_MKC)
        {
            setInitialStageMeasurementFromGui();
            setFfhFromGui();
            setThresholdRunoffFromGui();     
        }
        else if (modelType == RainfallRunoffModelType.SAC_SMA)
        {
          //do nothing
        }

        
        
        return;
    }
	//-----------------------------------------------------------------
    
      
//  add all of the event handlers
     private void addListeners()
     {

         //allow the frame to close when the user presses the close-box
         addWindowListener(new FrameCloseWindowListener());
         
        // addComponentListener(new WindowResizeComponentListener());
         // this object does not get garbage collected right away, since in the
         // constructor, it adds itself as a Window  listener
         WindowResizingManager mgr = new WindowResizingManager(this, _minDimension, _maxDimension);
   
         // set up the action to perform when the close button is pressed
         _closeButton.addActionListener( new CloseListener());
         
         //setup of the action for the screen capture button
         _screenCaptureButton.addActionListener(new ScreenCaptureActionListener());
         
         //set up the window launcher button
         _windowLauncherButton.addActionListener(new ControlWindowFocusActionListener());
    

         // set up the action to perform when the forward button is pressed
         _forwardButton.addActionListener( new TimeWindowSlideListener(1));
        
         // set up the action to perform when the forward button is pressed
         _forward1DayButton.addActionListener(new TimeWindowSlideListener(24));
        
        //setup of the action to perform when the recenter button is pressed
         _recenterButton.addActionListener(new TimeWindowRecenterActionListener());

         // set up the action to perform when the back button is pressed
         _back1DayButton.addActionListener( new TimeWindowSlideListener(-24));

         // set up the action to perform when the back button is pressed
         _backButton.addActionListener(  new TimeWindowSlideListener(-1));
        
    
        
         // set up the action to perform when the time mode check box is pressed
        // _timeModeCheckBox.addActionListener( new TimeModeCheckBoxListener());
       
       
        // set up the action to perform when the show flood stage check box is pressed
         _showFloodStageCheckBox.addActionListener( new FloodStageCheckBoxListener());
     
         // set up the action to perform when the show major flood stage check box is pressed
         _showMajorFloodStageCheckBox.addActionListener( new MajorFloodStageCheckBoxListener());
            
         // set up the action to perform when the _showPrecipAmountsCheckBox is pressed
         _showPrecipAmountsCheckBox.addActionListener( new ShowPrecipAmountsCheckBoxListener());
     
         // set up the action to perform when the show obs stage time series check box is pressed
          _showObsHeightTsCheckBox.addActionListener( new ShowObservedHeightTsCheckBoxListener());
     
         
          //addListener for the delayRerunCheckBox
          _delayRerunCheckBox.addActionListener(new DelayRerunCheckBoxListener());
          
          
          // SHOW MINOR PRECIP TICKS
          _showMinorPrecipTicksCheckBox.addActionListener(new ShowMinorPrecipTicksCheckBoxListener());
          
          // SHOW MINOR STAGE TICKS
         _showMinorStageTicksCheckBox.addActionListener(new ShowMinorStageTicksCheckBoxListener());
          
          
         // SHOW Simulated Stage
         _showSimulatedStageCheckBox.addActionListener(new ShowSimulatedStageCheckBoxListener());
       
         
          //CANVAS LISTENERS
          
    
         // addlistener for the PrecipCanvas
         _precipCanvas.addMouseMotionListener(
                    new CanvasMouseMovedListener(_precipCanvas, _precipValueMapper));
         
      
         // addlistener for the StageCanvas
         _stageCanvas.addMouseMotionListener(  
                    new CanvasMouseMovedListener(_stageCanvas, _stageValueMapper));

       

         //--- KC API model settings

         //baseflow reset
         _apiMkcInitialStagePanel.getButton().addActionListener(new InitialStageResetListener());
     
         //baseflow adjustment
         _apiMkcInitialStagePanel.getTextField().addFocusListener(new InitalStageFocusListener());
                      
         //FFH reset
         _apiMkcFfhPanel.getButton().addActionListener(new FfhResetListener());
         
         //FFH adjustment
         _apiMkcFfhPanel.getTextField().addFocusListener(new FfhFocusListener());
   
         _apiMkcStartingStateComboBox.addActionListener(
                    new ModelRunStartingStateActionListener(_apiMkcStartingStateComboBox));

        
        //threshold Runoff reset
        _apiMkcThreshRPanel.getButton().addActionListener(new ThreshRunoffResetListener());
        
        
        //threshold Runoff adjustment
        _apiMkcThreshRPanel.getTextField().addFocusListener(new ThreshRunoffFocusListener());
        
        _apiMkcUseCustomModelRunTimeCheckBox.addActionListener(new UseCustomModelRunStartTimeCheckBoxListener());

        // the apply button for API-MKC
        _apiMkcApplySettingsButton.addActionListener(new ApplySettingsActionListener());
        
        // refresh button for API-MKC state/time list
        _apiMkcStateRefreshButton.addActionListener(new RefreshApiMkcStateListActionListener());

        _apiMkcCustomModelRunDateTimeTextField.getDocument().addDocumentListener(new CustomModelRunDateTimeChangedDocumentListener());

        // add listener for Model Change
        _modelTypeComboBox.addActionListener(new ModelChoiceListener());
       
        // Unithydrograph manual change
        _uhgComboBox.addActionListener(new UnitHydrographChoiceListener());
        
         //add listener for the Mouse events from TsCanvas
         AdjustPrecipMouseListener precipAdjustmentListener = new AdjustPrecipMouseListener();
         _precipCanvas.addMouseListener(precipAdjustmentListener);
         _precipCanvas.addMouseMotionListener(precipAdjustmentListener);
         _precipCanvas.addKeyListener(precipAdjustmentListener);

         //     add listener for the Mouse events from TsCanvas
        // _stageCanvas.addTsCanvasEventListener(new ModelTimeAdjustmentTsCanvasEventListener());
        AdjustStageMouseListener adjustStageListener = new AdjustStageMouseListener();
        _stageCanvas.addMouseListener(adjustStageListener);
        _stageCanvas.addMouseMotionListener(adjustStageListener);
       
       //SAC_SMA controls
       
         // add Frame launching button actions
         _sacParamsButton.addActionListener(new SacParamsLaunchActionListener());
         _sacStateButton.addActionListener(new SacStateLaunchActionListener());          
         
         _sacSetVarInitialStateButton.addActionListener(new SetInitialVarStateActionListener());
         
   //      _sacMonthlyMapeValuesEditorButton.addActionListener(new MonthlyMapeEditorLaunchActionListener());

         _sacStartingStateComboBox.addActionListener(
                    new ModelRunStartingStateActionListener(_sacStartingStateComboBox));

         
         //model controls
         // add listener for setting the length of forecast
         _forecastHoursSpinner.addChangeListener(new ForecastHoursChangeListener());

         
         _adjustmentCheckBox.addActionListener( new AdjustToggleActionListener());
         
         _adjustmentLastObsInputTimeTextField.getDocument().addDocumentListener(new LastObsTimeForAdjustmentChangedDocumentListener());
         
         _blendPeriodSpinner.addChangeListener(new AdjustmentHoursChangeListener());
       
         
         //launches the LiveSacState editor dialog (the one with sliders)
         _sacLiveStateAdjustmentButton.addActionListener(new LiveSacStateLaunchActionListener());
         
        // refresh button for SAC_SMA state/time list
        _sacStateListRefreshButton.addActionListener(new RefreshSacSmaStateListActionListener());


        // create listeners for the time series that can be editted
        getPrimaryStreamModel().getEvaporationTimeSeriesHolder().addListener(new EvaporationTimeSeriesChangeListener());
        getPrimaryStreamModel().getPrecipTimeSeriesHolder().addListener(new PrecipTimeSeriesChangeListener());
        getPrimaryStreamModel().getForecastStageTimeSeriesHolder().addListener(new ForecastHeightTimeSeriesChangeListener());
        	

     }  //end addListeners

     //-------used by inner classes --------------------------------------------------------------------
     private void usePrecipFileChooserForReading()
     {
         String defaultDirectory = null;
         
         if (_precipFileChooserHelper == null)
         {
             String[] extensions = {"work" };
             _precipFileChooserHelper = new FileChooserHelper(_sshp_precip_directoryString, 
                                                              extensions,
                                                              "Precip Work files .work");
         }
               
         String filePath = _precipFileChooserHelper.displayForOpen(AnalysisWindow.this);
         
         if (filePath != null)
         {
             getPrimaryStreamModel().reloadFilePrecipTimeSeries(filePath); 
             runModel();
         }
         
         return;
     }
  
     // -----------------------------------------------------------------------------------
     private void usePrecipFileChooserForSaving()
     {
         String defaultDirectory = null;
         
         if (_precipFileChooserHelper == null)
         {
             String[] extensions = {"work" };
             _precipFileChooserHelper = new FileChooserHelper(_sshp_precip_directoryString, 
                                                              extensions,
                                                              "Precip Work files .work");
         }
               
         String filePath = _precipFileChooserHelper.displayForSave(AnalysisWindow.this);
         
         if (filePath != null)
         {
             getPrimaryStreamModel().savePrecipTimeSeriesToFile(filePath);
         }
         
         return;
     }
  
     // -----------------------------------------------------------------------------------
     private void useCaptureScreenFileChooserForSaving()
     {
         BufferedImage image = captureScreenToImage();
         
         String defaultDirectory = null;
         
         if (_imageFileChooserHelper == null)
         {
             String[] extensions = {"jpg", "JPEG", "JPG"  };
             _imageFileChooserHelper = new FileChooserHelper(_whfs_image_directoryString, 
                                                              extensions,
                                                              "JPEG files .jpg .JPEG .JPG ");
         }
               
         String filePath = _imageFileChooserHelper.displayForSave(AnalysisWindow.this);
         
         if (filePath != null)
         {
            writeImageToFile(image, new File(filePath)); 
         }
         
         return;
     }
     
     // --------------------------------------------------------------------------------
     private void displayCaptureScreenFileChooser()
     {
         BufferedImage image = captureScreenToImage();
         
         if (_imageFileChooser == null)
         {
               _imageFileChooser = new JFileChooser(_whfs_image_directoryString);
         }
         
         
         //add a FileFilter for JPG files
         List filterList = new ArrayList();
         
         filterList.add( "JPEG" );
         filterList.add("JPG");  
         FileFilter fileFilter = new ExtensionFileFilter( filterList, "JPEG Image Files (*.jpg, *.JPG) ");
         
         
         //set the file filter    
         _imageFileChooser.setFileFilter( fileFilter );
         // _imageFileChooser.set
         
         
         //open the dialog    
         int returnVal = _imageFileChooser.showSaveDialog(this);
         
         if (returnVal == JFileChooser.APPROVE_OPTION)
         {
             File file =  _imageFileChooser.getSelectedFile();
             
             //ensure ends in ".jpg"
             if   ( (! file.getName().endsWith(".jpg")) &&
                     (! file.getName().endsWith(".JPG"))
             )
             {
                 String newFilePath = file.getPath() + ".jpg";  
                 file = new File(newFilePath); 
             }
             
             writeImageToFile(image, file);
         } 
         
         else
         {
             //System.out.println("AnalysisWindow.displayCaptureScreenFileChooser(): ");
             //JOptionPane.showMessageDialog(AnalysisWindow.this, "Image file not saved.");  
             DialogHelper.displayMessageDialog(AnalysisWindow.this, "Image file not saved.");
             
         }
         
        
         
         return;
     }
     
    // -----------------------------------------------------------------
        private void sleep(int millis)
        {
            
            try
            {
                Thread.sleep(millis);
            }
            catch (InterruptedException e)
            {
                e.printStackTrace();
            }    
            
        }
 
    //  -----------------------------------------------------------------
    private BufferedImage captureScreenToImage()
    {
        BufferedImage bufferedImage = null;
        try
        {
          
            Robot robot = new Robot();
            int x = AnalysisWindow.this.getX();
            int y = AnalysisWindow.this.getY();
            int width = AnalysisWindow.this.getWidth();
            int height = AnalysisWindow.this.getHeight();
            Rectangle rectangleBounds = new Rectangle(x, y, width, height);
                  
            //System.out.println("bounds rectangle of image capture = " + rectangleBounds);
            bufferedImage = robot.createScreenCapture(rectangleBounds);
        
        } //end try
         
        catch (AWTException e)
        {
            e.printStackTrace();
        }
  

        return bufferedImage;
    }
    
    //  -----------------------------------------------------------------
    
    private void writeImageToFile(BufferedImage image, File file)
    { 
        try
        {
            ImageOutputStream imageOutputStream = new FileImageOutputStream(file);
            ImageIO.write(image, "JPG", imageOutputStream);
            imageOutputStream.close();
        }
      
        catch(java.io.IOException e )
        {
            e.printStackTrace();  
        }        
                 
        
    }
    //  -----------------------------------------------------------------
 
    private void setInitialStageMeasurementFromGui()
    {
        String valueString = _apiMkcInitialStagePanel.getTextField().getText();
        
        setInitalStageMeasurementInModelFromString(valueString);
    }
 
    // -----------------------------------------------------------------

    
    private void setInitalStageMeasurementInModelFromString(String initialStageString)
    {
        double value = Double.parseDouble(initialStageString);
        
        Measurement measurement = new Measurement(value, _stageUnit);
        
        getPrimaryStreamModel().setInitialStageMeasurement(measurement);
        
    }
    
    // -----------------------------------------------------------------

    
    private void resetInitialStageMeasurementFromModel()
    {
        //uses a streamModel call to get the Baseflow value, but does not change the
        // value STORED by the streamModel object 
        Measurement initialStageMeasurement = getPrimaryStreamModel().findInitialStageMeasurement();
        
        //set the value STORED by the streamModel
        getPrimaryStreamModel().setInitialStageMeasurement(initialStageMeasurement);
        
        NumberFormat nf = new DecimalFormat("####.##");
        String valueString = nf.format(initialStageMeasurement.getValue());
    
        _apiMkcInitialStagePanel.getTextField().setText("" + valueString);
    }
    
    // -----------------------------------------------------------------
    
    private void setFfhFromGui()
    {
        String valueString = _apiMkcFfhPanel.getTextField().getText();
        
        setFfhFromString(valueString);
    }
 
    // -----------------------------------------------------------------

    private void setFfhFromString(String ffhString)
    {
        String header = "AnalysisWindow.setFfgFromString(): ";
        double value = Double.parseDouble(ffhString);
        
        System.out.println(header + "value = " + value);
        
        getPrimaryStreamModel().setFfhValue(value);
    }
    
    private void resetFfhFromModel()
    {
        //uses a streamModel call to get the FFH value, but does not change the
        // value STORED by the streamModel object 
        double ffh = getPrimaryStreamModel().findFfhValue();
        
        //set the value STORED by the streamModel
        getPrimaryStreamModel().setFfhValue(ffh);
        
        NumberFormat nf = new DecimalFormat("####.##");
        String valueString = nf.format(ffh);
      //  System.out.println( "AnalysisWindow:resetFfhFromModel(): FFHValue = : " + valueString );
        _apiMkcFfhPanel.getTextField().setText(valueString);
    }
 
    // -----------------------------------------------------------------
    private StreamModel getPrimaryStreamModel()
    {
        StreamModel model = _modelManager.getPrimaryStreamModel();
        
        return model;
    }
    // -----------------------------------------------------------------
    
    private void loadModelData()
    {
        getPrimaryStreamModel().reloadModelData();

        if (getPrimaryStreamModel().getRainfallRunoffModelType() == 
            RainfallRunoffModelType.SAC_SMA)
        {
           //do nothing
        }
        else if (getPrimaryStreamModel().getRainfallRunoffModelType() == 
             RainfallRunoffModelType.API_MKC)
        {
            resetFfhFromModel();
            resetInitialStageMeasurementFromModel();
            resetThresholdRunoffFromModel(); 
        }     
    }
 
    // -----------------------------------------------------------------
    private void setApiMkcModelState()
    {

        String header = "AnalysisWindow.setApiMkcModelState(): ";
        
        FFHDescriptor descriptorToUse = null;
        MeasuringUnit ffhUnit = MeasuringUnit.inches;
         
         
        if (_apiMkcUseCustomModelRunTime)
        {
            //should use some other method for this
            double value = Double.parseDouble(_apiMkcFfhPanel.getTextField().getText());
            
            long time = _apiMkcCustomModelRunDateTimeTextField.getTime();
            AbsTimeMeasurement measurement = new AbsTimeMeasurement(value, time, ffhUnit); 
            descriptorToUse = new FFHDescriptor(getPrimaryStreamModel().getLocationId(),
                                                measurement, StreamModel.DEFAULT_FFG_DURATION_CODE, false); 
            
         
            System.out.println(header + "ffh measurement =  " + measurement);
            
        }
        else //use the standard selection method
        {
            
            _selectedFFHDescriptor = (FFHDescriptor)
                         _apiMkcStartingStateComboBox.getSelectedItem();
                         
            descriptorToUse = _selectedFFHDescriptor; 
        }
        
        // if there are no products,
        if (descriptorToUse == null)
        {
            
            AbsTimeMeasurement fakeFfmMeasurement = new AbsTimeMeasurement(10,
                                                                           getTimeOfLatestHour(),
                                                                           MeasuringUnit.inches);
            
            String lid = getPrimaryStreamModel().getLocationId();
            descriptorToUse = new FFHDescriptor(lid,
                                                fakeFfmMeasurement,
                                                StreamModel.DEFAULT_FFG_DURATION_CODE, 
                                                false);    
        }
        
            
        selectFFHProduct(descriptorToUse);
        
        System.out.println(header + "descriptorToUse =  " + descriptorToUse);

       // System.out.println("AnalysisWindow.setApiMkcModelState : descriptor = " + 
       // descriptorToUse.toString());
            
        long newTime = descriptorToUse.getTime(); 
        
        System.out.println(header + "newTime =  " + DbTimeHelper.getDateTimeStringFromLongTime(newTime));
        
        setModelRunStartTime(newTime);

        recenterTimeWindowsAroundModelStartTime();
            
        //runModel();
    }

  // -----------------------------------------------------------------
    
    private void setSacModelState()
    {
        String header = "AnalysisWindow.setSacModelState():";
        
        _selectedSacStateDescriptor = (SacSmaStateDescriptor)
                         _sacStartingStateComboBox.getSelectedItem(); 
            
           
        selectSacSmaStateDescriptor(_selectedSacStateDescriptor);
        
        //make sure that we reload the prior runoff time series, which happens in here
      
       // System.out.println(" ********* AnalysisWindow.setSacModelState() : descriptor = " + 
      //                  _selectedSacStateDescriptor.toString());
 //TODO the newTime = line seems redundant              
        long newTime = _selectedSacStateDescriptor.getValidTime(); 
        setModelRunStartTime(newTime);

        recenterTimeWindowsAroundModelStartTime();
            
        //runModel();
    }

    
    // -----------------------------------------------------------------
    private void selectFFHProduct(FFHDescriptor descriptor)
    {
        StreamModel primaryStreamModel = getPrimaryStreamModel();
        
        primaryStreamModel.setFFHProductDescriptor(descriptor);    
        
        _modelManager.configureStreamModelsForMkcApi(primaryStreamModel, descriptor);
    }

    // -----------------------------------------------------------------
    private void selectSacSmaStateDescriptor(SacSmaStateDescriptor primaryDescriptor)
    {
        
       String  header = "AnalysisWindow.selectSacSmaStateDescriptor(): ";
        //store the primaryModel and set the SAC-SMA Descriptor
        StreamModel primaryModel = getPrimaryStreamModel();
        primaryModel.setSacSmaStateDescriptor(primaryDescriptor);
        long primaryTime = primaryDescriptor.getValidTime();
        
        
        //setup the parallel stream models that start at the same time, but have alternate states   
        _modelManager.configureStreamModelsForSacSma(primaryModel, primaryDescriptor, _showAlternateForecasts, _showPastForecasts);
         
         //adjust the painters to paint this new stuff
        setUpHydrographTimeSeriesPainters();
    }
    
    // ---------------------------------------------------------------------------------------
    
   
    
    private void setModelRunStartTime(long newStartTime)
    {
 //       String header = "AnalysisWindow.setModelRunStartTime(): ";
        
        newStartTime = TimeHelper.truncateTimeInMillisToNearestHour(newStartTime, 1);
        
       _modelManager.setPrimaryModelStartTime(newStartTime);
        
      //  getPrimaryStreamModel().setModelStartTime(newStartTime);

        //to make the response time to the clicking faster
        // this will happen eventually, anyway after the model run
       // _precipCanvas.refresh();

        String dateTimeString = getDateTimeStringToMinutes(newStartTime);

     //   _modelRunStartTimeValueLabel.setText("Model Run Start Time: " + dateTimeString);
        _modelRunStartTimeValueLabel.setText(dateTimeString);
        
        
        //reload the model data, so that the correct parameters and
        // states are used.
        
        loadModelData();
     
        runModel();
 
    }
    // -----------------------------------------------------------------
    
    private void setThresholdRunoffFromGui()
    {
        String valueString = _apiMkcThreshRPanel.getTextField().getText();
        
        setThresholdRunoffFromString(valueString);
    }
 
    // -----------------------------------------------------------------

    
    private void setThresholdRunoffFromString(String thresholdRunoffString)
    {
         double value = Double.parseDouble(thresholdRunoffString);
         getPrimaryStreamModel().setThresholdRunoff(value);
    }

   //  -----------------------------------------------------------------
    

    private void resetThresholdRunoffFromModel()
    {
        //uses a streamModel call to get the threshold Runoff value,
        //   but does not change the
        // value STORED by the streamModel object 
        double thresholdRunoff = getPrimaryStreamModel().findThresholdRunoffValue();
        
        //set the value STORED by the streamModel
        getPrimaryStreamModel().setThresholdRunoff(thresholdRunoff);
        
        NumberFormat nf = new DecimalFormat("####.##");
        String valueString = nf.format(thresholdRunoff);
    
        _apiMkcThreshRPanel.getTextField().setText(valueString);
    }
   
    // -----------------------------------------------------------------------
 
    private void redrawCanvases()
    {
        String header = "AnalysisWindow.redrawCanvases()";
  
        _precipCanvas.refresh();
        
        _stageCanvas.refresh();
    }

    // -----------------------------------------------------------------------
    
    private void initMenuBar()
    {
         _menuBar = new JMenuBar();
          
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
       

        // Edit/View Menu
        menu = new JMenu("View/Edit");
        menu.setMnemonic(KeyEvent.VK_V);
        menu.getAccessibleContext().setAccessibleDescription(
                         "View/Edit time series data.");
        _menuBar.add(menu);
        
          
        // edit menu items
        
        //Precip editor
        menuItem = new JMenuItem("Precip Editor...");
        menuItem.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_P, ActionEvent.ALT_MASK));
        menuItem.getAccessibleContext().setAccessibleDescription(
                "");
        menu.add(menuItem);
        
        menuItem.addActionListener(new PrecipTimeSeriesLaunchActionListener());

        
        // Forecast Stage Editor
        menuItem = new JMenuItem("Forecast Stage Editor...");
        menuItem.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_I, ActionEvent.ALT_MASK));
        menuItem.getAccessibleContext().setAccessibleDescription(
                "");
        menu.add(menuItem);
        
        menuItem.addActionListener(new ForecastHeightTimeSeriesLaunchActionListener());

        
        // ET Time Series editor
        menuItem = new JMenuItem("Evapotranspiration Editor...");
        menuItem.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_T, ActionEvent.ALT_MASK));
        menuItem.getAccessibleContext().setAccessibleDescription(
                "");
        menu.add(menuItem);
        
        menuItem.addActionListener(new EvaporationTimeSeriesLaunchActionListener());

       
        
        // Prior Computed Runoff Viewer
        menuItem = new JMenuItem("Prior Runoff Viewer...");
        menuItem.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_O, ActionEvent.ALT_MASK));
        menuItem.getAccessibleContext().setAccessibleDescription(
                "");
        menu.add(menuItem);
        
        menuItem.addActionListener(new PriorRunoffTimeSeriesLaunchActionListener());

        
        // Computed Runoff Viewer
        menuItem = new JMenuItem("Runoff Viewer...");
        menuItem.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_R, ActionEvent.ALT_MASK));
        menuItem.getAccessibleContext().setAccessibleDescription(
                "");
        menu.add(menuItem);
        
        menuItem.addActionListener(new RunoffTimeSeriesLaunchActionListener());

        
        // Toggle UNADJUSTED states
        JCheckBoxMenuItem checkBoxMenuItem = new JCheckBoxMenuItem("Show Unadjusted States");
        checkBoxMenuItem.setState(_showUnadjustedStates);
        menuItem = checkBoxMenuItem;
      
        menuItem.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_U, ActionEvent.ALT_MASK));
        menuItem.getAccessibleContext().setAccessibleDescription(
                "");
        menu.add(menuItem);
        
        menuItem.addActionListener(new ShowUnadjustedStatesActionListener());
        
        
        // Save Menu
        menu = new JMenu("Save");
        menu.setMnemonic(KeyEvent.VK_S);
        menu.getAccessibleContext().setAccessibleDescription(
                         "Save time series changes to database.");
        _menuBar.add(menu);
        

        // Save Forecast Stage
        menuItem = new JMenuItem("Save Forecast Stage...");
        menuItem.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_G, ActionEvent.ALT_MASK));
        menuItem.getAccessibleContext().setAccessibleDescription(
                "");
        menu.add(menuItem);
        
        menuItem.addActionListener(new ForecastHeightSaveLaunchActionListener());

        
        // Save Forecast Discharge
        menuItem = new JMenuItem("Save Forecast Discharge Time Series...");
        menuItem.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_D, ActionEvent.ALT_MASK));
        menuItem.getAccessibleContext().setAccessibleDescription(
                "");
        menu.add(menuItem);
        
        menuItem.addActionListener(new ForecastDischargeSaveLaunchActionListener());

        
        // Save Evaporation Time Series
        menuItem = new JMenuItem("Save Evap Time Series..");
        menuItem.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_E, ActionEvent.ALT_MASK));
        menuItem.getAccessibleContext().setAccessibleDescription(
                "");
        menu.add(menuItem);
        
        menuItem.addActionListener(new EvapSaveLaunchActionListener());
      
        
        // Save Edited MAP Time Series
        menuItem = new JMenuItem("Save Edited MAP Time Series..");
        menuItem.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_M, ActionEvent.ALT_MASK));
        menuItem.getAccessibleContext().setAccessibleDescription(
                "");
        menu.add(menuItem);
        
        menuItem.addActionListener(new MAPSaveLaunchActionListener());
        
         
        //reloadPrecipButton directly on menuBar      
        Color menuBarColor = _menuBar.getBackground();
        Border invisibleBorder = BorderFactory.createLineBorder(menuBarColor);

        JButton button = new JButton("   Reload Precip  ");
        button.addActionListener(new ReloadPrecipTimeSeriesActionListener());
        button.setBorder(invisibleBorder);
        button.setSize(60, 30);
        String text = "Reload the MAP time series from the database.\n" +
        "Changes will be lost.";
        button.setToolTipText(text);
        _menuBar.add(button);
        
        //reloadFilePrecipButton directly on menuBar      
        menuBarColor = _menuBar.getBackground();
        invisibleBorder = BorderFactory.createLineBorder(menuBarColor);

        button = new JButton("   Reload File Precip  ");
        button.addActionListener(new ReloadFilePrecipTimeSeriesActionListener());
        button.setBorder(invisibleBorder);
        button.setSize(60, 30);
        text = "Reload the MAP time series from a workfile.\n" +
        "Changes will be lost.";
        button.setToolTipText(text);
        _menuBar.add(button);
      
        
        // reloadObsStageButton directly on menuBar 
        button = new JButton("    Reload Stream Obs.    ");
        button.addActionListener(new ReloadObsStageTimeSeriesActionListener());
        button.setBorder(invisibleBorder);
        button.setSize(80, 30);
        text = "Reload the observed stage/discharge time series from the database.";
        button.setToolTipText(text);
        _menuBar.add(button);
   
        // separator
        
   //     JSeparator menuSeparator = new JSeparator();  
    //    menuSeparator.setOrientation(SwingConstants.VERTICAL);
  //      _menuBar.add(menuSeparator);
     
        //acts as a separator to move the Help menu all the way to the right
        menuItem = new JMenuItem("");
        menuItem.setEnabled(false);
        _menuBar.add(menuItem);
       
        // Help Menu
         
        menu = new JMenu("Help");
        menu.setMnemonic(KeyEvent.VK_HELP);
        menu.getAccessibleContext().setAccessibleDescription(
                "Access help menu items.");        
        _menuBar.add(menu);
        
        menuItem = new JMenuItem("About...");
        menuItem.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_A, ActionEvent.ALT_MASK));
        menuItem.getAccessibleContext().setAccessibleDescription(
                "Display the About Dialog.");
        menu.add(menuItem);
        
        menuItem.addActionListener(new AboutLaunchListener());

       
    }
   
    // -----------------------------------------------------------------------  
    
    // -----inner classes ------------------------------------------------------
    private class AboutLaunchListener implements ActionListener
    { 
         public void actionPerformed(ActionEvent evt)
         {
             String aboutText = AboutInfo.getText();
             DialogHelper.displayMessageDialog(AnalysisWindow.this, aboutText, "About SSHP");
         }
    }

    //-----------------------------------------------------------------
       
    private class InitialStageResetListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
            resetInitialStageMeasurementFromModel();
            runModel();

        }
    }
    
    //-----------------------------------------------------------------
  
    private class InitalStageFocusListener implements FocusListener
    {
         
        public InitalStageFocusListener()
        {
            
        }
        
        public void focusGained(FocusEvent event)
        {

        }

        public void focusLost(FocusEvent event)
        {
            //System.out.println("InitalStageFocusListener triggered");
     
            JTextField textField = (JTextField) event.getSource();
            
            String initialStageString = textField.getText().trim();
            if (initialStageString.length() == 0)
            {
                //consider putting an error dialog here
            }
            else
            {
                try
                {
                    setInitalStageMeasurementInModelFromString(initialStageString);
                }
                catch (Throwable t)
                {
                    //consider putting an error dialog here
                }
            }

            // the apply button does this now
            //runModel();

        } //end focusLost       
        
    }
    
    //-----------------------------------------------------------------
   
    private class FfhResetListener implements ActionListener
    {
          public void actionPerformed(ActionEvent evt)
          {
              resetFfhFromModel();
              runModel();
          }
    }
    //-----------------------------------------------------------------
      
    private class ApplySettingsActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
            // System.out.println("ApplySettingsActionListener triggered");
            updateModelSettingsFromGui();
           // setApiMkcModelState();
            runModel();
        }
    }
    //-----------------------------------------------------------------
    
    private class RefreshSacSmaStateListActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
             //System.out.println("RefreshSacSmaStateListActionListener triggered");
            
             
             initSacStateComboBox(_sacStartingStateComboBox);
        }
    }
    //-----------------------------------------------------------------
      
    private class RefreshApiMkcStateListActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
             //System.out.println("RefreshApiMkcStateListActionListener triggered");
            
             initApiMkcStateComboBox(_apiMkcStartingStateComboBox);
        }
    }

    private class ReloadPrecipTimeSeriesActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
            DialogHelper.setWaitCursor(_menuBar);
             getPrimaryStreamModel().reloadPrecipTimeSeries();
             runModel();
            DialogHelper.setDefaultCursor(_menuBar);
        }
    }
    //-----------------------------------------------------------------
    
    private class ReloadFilePrecipTimeSeriesActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
            DialogHelper.setWaitCursor(_menuBar);
            
            usePrecipFileChooserForReading();
            
            DialogHelper.setDefaultCursor(_menuBar);
        }
    }
    //-----------------------------------------------------------------
      
    private class ReloadObsStageTimeSeriesActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
            DialogHelper.setWaitCursor(_menuBar);
             
            getPrimaryStreamModel().reloadObservedStageTimeSeries();
             runModel();
             
            DialogHelper.setDefaultCursor(_menuBar);
   
        }
    }
    //-----------------------------------------------------------------
      
    private class FfhFocusListener implements FocusListener
    {
           
           public FfhFocusListener ()
           {
             
           }
        
           public void focusGained(FocusEvent event)
           {

           }

           public void focusLost(FocusEvent event)
           {
               //System.out.println("FfhFocusListener triggered");
     
               JTextField textField = (JTextField) event.getSource();
               
              
               String ffhString = textField.getText().trim();
               if (ffhString.length() == 0)
               {
                    resetFfhFromModel(); //just set it back to the value in the streamModel.
               }
               else
               {
                   try
                   {
                       setFfhFromString(ffhString);
                    
                   }
                   catch (Throwable t)
                   {
                       resetFfhFromModel(); //just set it back to the value in the streamModel.
                   }
               }

               // the apply button does this now
               //runModel();

           } //end focusLost       
        
    }
    //-----------------------------------------------------------------
    
    private class ThreshRunoffResetListener implements ActionListener
    {
         public void actionPerformed(ActionEvent evt)
         {
             resetThresholdRunoffFromModel();
             runModel();

         }
    }
    //-----------------------------------------------------------------
      
    private class ThreshRunoffFocusListener implements FocusListener
    {
        
         public ThreshRunoffFocusListener()
         {
                
         }
    
         public void focusGained(FocusEvent event)
         {

         }

         public void focusLost(FocusEvent event)
         {
             //System.out.println("ThreshRunoffFocusListener triggered");
     
             JTextField textField = (JTextField) event.getSource();
             
             //System.out.println("AnalysisWindow.ThreshRFocusLost listener activated");
        
             String thresholdRunoffString = textField.getText().trim();
             if (thresholdRunoffString.length() == 0)
             {
                  resetThresholdRunoffFromModel(); //just set it back to the value in the streamModel.
             }
             else
             {
                 try
                 {
                     setThresholdRunoffFromString(thresholdRunoffString);
                 }
                 catch (Throwable t)
                 {
                     resetThresholdRunoffFromModel(); //just set it back to the value in the streamModel.
                 }
             }

             // the apply button does this now
              //runModel();

         } //end focusLost       
    
     }
      
    //-----------------------------------------------------------------
 
      
    private class CanvasMouseMovedListener extends MouseMotionAdapter
    {
        private TsPaintableCanvas _canvas = null;
        private ValueMapper _valueMapper = null;
        
        public CanvasMouseMovedListener(TsPaintableCanvas canvas, ValueMapper valueMapper)
        {
            _canvas = canvas;  
            _valueMapper = valueMapper; 
        }
        
        public void mouseMoved(MouseEvent event)
        {
             super.mouseMoved(event);
             Point screenPoint = event.getPoint();

             DataPoint dataPoint = 
                  _canvas.getViewport().getDataPoint(screenPoint);
                       
            
             if (_canvas.getViewport().isViewable(dataPoint))
             {
                 updateDataPointText(dataPoint, _valueMapper, _canvas);
             }
         }  
        
    } 
    //-----------------------------------------------------------------
   
    private class FrameCloseWindowListener extends WindowAdapter
    {
        public void windowClosing(WindowEvent evt)
        {
            dispose();
        }
        
    }
    
    //-----------------------------------------------------------------
    
    private class CloseListener implements ActionListener
    { 
        public void actionPerformed(ActionEvent evt)
        {
            dispose();
        }
    }
      
    //-----------------------------------------------------------------
    private class ControlWindowFocusActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
           _controller.bringLaunchWindowToFront();
        }
    }

   //----------------------------------------------------------------- 
    private class FloodStageCheckBoxListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
            _forceShowFloodStage = _showFloodStageCheckBox.isSelected();
            _floodStageLinePainter.setShouldPaint(_forceShowFloodStage);
            redrawCanvases();
        }
  
    }
    //----------------------------------------------------------------- 
    
    private class MajorFloodStageCheckBoxListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        { 
            if (_majorFloodStageLinePainter != null)
            {
                _forceShowMajorFloodStage = _showMajorFloodStageCheckBox.isSelected();
                _majorFloodStageLinePainter.setShouldPaint(_forceShowMajorFloodStage);
                redrawCanvases();
            }       
        }
    }
    
    //----------------------------------------------------------------- 
    private class ShowPrecipAmountsCheckBoxListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
            _showPrecipAmounts = _showPrecipAmountsCheckBox.isSelected();
            _precipPainter.setShowPrecipAmounts(_showPrecipAmounts);
            _precipTextTotalPainter.setShouldPaint(_showPrecipAmounts);
            redrawCanvases();
        }
  
    }
    //-----------------------------------------------------------------
    private class ShowObservedHeightTsCheckBoxListener implements ActionListener
    {
            public void actionPerformed(ActionEvent evt)
            {
                
                String header = "ShowObservedHeightTsCheckBoxListener.actionPerformed(): ";
                System.out.println(header + "I have been clicked. My _observedStageTsPainter is  " +
                                _observedStageTsPainter);
                
                _showObservedHeightData = _showObsHeightTsCheckBox.isSelected();
                _observedStageTsPainter.setShouldPaint(_showObservedHeightData);     
                
             

                redrawCanvases();
            }
    }
    
    //-----------------------------------------------------------------
    private class DelayRerunCheckBoxListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
           _delayRerunModelWhileDrawing = _delayRerunCheckBox.isSelected();
        }
  
    }
    //-----------------------------------------------------------------
    private class ShowMinorPrecipTicksCheckBoxListener implements
            ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
            _showMinorPrecipTicks = _showMinorPrecipTicksCheckBox.isSelected();
            _precipBackgroundPainter.setShowMinorTicks(_showMinorPrecipTicks);
            redrawCanvases();
        }

    }

    //-----------------------------------------------------------------
    private class ShowMinorStageTicksCheckBoxListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
            _showMinorStageTicks = _showMinorStageTicksCheckBox.isSelected();
            _stageBackgroundPainter.setShowMinorTicks(_showMinorStageTicks);
            redrawCanvases();
        }

    }

    //-----------------------------------------------------------------
      
    private class ShowSimulatedStageCheckBoxListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
            _showSimulatedStage = _showSimulatedStageCheckBox.isSelected();
            _simulatedStageTsPainter.setShouldPaint(_showSimulatedStage);
            redrawCanvases();
        }

    }
    
    //-----------------------------------------------------------------
    
    private class ShowAlternateForecastsCheckBoxListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
            DialogHelper.setWaitCursor(AnalysisWindow.this);
            
            
            String header = "AnalysisWindow.ShowAlternateForecastsCheckBoxListener.actionPerformed(): ";
            CodeTimer timer = new CodeTimer();
            timer.start();

            _showAlternateForecasts = _sacShowAlternateForecastsCheckBox.isSelected();

            //     System.out.println(header + " adjust the model Manager and rerun the model");

            CodeTimer setShowAlternateForecastsTimer = new CodeTimer();
            setShowAlternateForecastsTimer.start();
            _modelManager.setShowAlternateForecasts(_showAlternateForecasts);
            setShowAlternateForecastsTimer.stop(header + "setShowAlternateForecasts() took: ");

            //adjust the painters to paint this new stuff
            CodeTimer setUpHydrographTimeSeriesPaintersTimer = new CodeTimer();
            setUpHydrographTimeSeriesPaintersTimer.start();
            setUpHydrographTimeSeriesPainters();
            setUpHydrographTimeSeriesPaintersTimer.stop(header + "setUpHydrographTimeSeriesPainters() took: ");

            
            CodeTimer runModelTimer = new CodeTimer();
            runModelTimer.start();
            runModel();
            runModelTimer.stop(header + "runModel()  took ");
            
            
            DialogHelper.setDefaultCursor(AnalysisWindow.this);
            
            timer.stop(header + ", the whole routine took ");
        }

    }

  //-----------------------------------------------------------------
    
    private class ShowPastForecastsCheckBoxListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
            
            DialogHelper.setWaitCursor(AnalysisWindow.this);
            
            String header = "AnalysisWindow.ShowPastForecastsCheckBoxListener(): ";
            CodeTimer timer = new CodeTimer();
            timer.start();
            
             _showPastForecasts = _sacShowPastForecastsCheckBox.isSelected();
            
            System.out.println("ShowPastForecastsCheckBoxListener. adjust the model Manager and rerun the model");
            
            CodeTimer setShowPastForecastsTimer = new CodeTimer();
            setShowPastForecastsTimer.restart();
            _modelManager.setShowPastForecasts(_showPastForecasts);
            setShowPastForecastsTimer.stop(header + "setShowPastForecasts() took");
            
            
            //adjust the painters to paint this new stuff
           
            setUpHydrographTimeSeriesPainters();
           
            runModel();
            
            DialogHelper.setDefaultCursor(AnalysisWindow.this);
            
            timer.stop(header + " took ");
        }

    }
    
    // --------------------------------------------------------------------------------------------------------------
    private class SetHoursBackAndIntervalHoursForPreviousForecastsActionListener implements ActionListener
    {
        
        private JTextField _hoursBackJTextField = null;
        private JTextField _intervalInHoursJTextField = null;
        
        
        public SetHoursBackAndIntervalHoursForPreviousForecastsActionListener(JTextField hoursBackJTextField, 
                                                                              JTextField intervalInHoursJTextField )
        {
            _hoursBackJTextField = hoursBackJTextField;
            _intervalInHoursJTextField = intervalInHoursJTextField;
        }
        
        public int getValueAndValidateTextField(JTextField textField, int defaultValue, int minValue, int maxValue)
        {

            String text = textField.getText();
            
            int value = defaultValue;
            
            try
            {
                value = Integer.parseInt(text);
                if  (  (value < minValue)  ||
                        (value > maxValue)
                )
                {
                    value = defaultValue;
                }
            }
            catch (NumberFormatException e)
            {
                value =  defaultValue;
            }
            
            textField.setText(String.valueOf(value));
            
            return value;
        }
        
        public void actionPerformed(ActionEvent evt)
        {
            String header = "AnalysisWindow.SetHoursBackAndIntervalHoursForPreviousForecastsActionListener(): ";
  
            DialogHelper.setWaitCursor(AnalysisWindow.this);
             
            int hoursInPast = getValueAndValidateTextField(_hoursBackJTextField, 0, 0, 120);
            int intervalInHours = getValueAndValidateTextField(_intervalInHoursJTextField, 1, 1, 120);
             
            _modelManager.setHoursBackAndIntervalForPreviousForecastStates(hoursInPast, intervalInHours);
     
            //adjust the painters to paint this new stuff
           
            setUpHydrographTimeSeriesPainters();
                     
            runModel();
            
            DialogHelper.setDefaultCursor(AnalysisWindow.this);
        }

    }
    // --------------------------------------------------------------------------------------------------------------
    //-----------------------------------------------------------------
    private class UseCustomModelRunStartTimeCheckBoxListener implements ActionListener
    {
         public void actionPerformed(ActionEvent evt)
         {
              
              _apiMkcUseCustomModelRunTime = _apiMkcUseCustomModelRunTimeCheckBox.isSelected();
              setApiMkcModelState();
              runModel();
         }
  
    }
 
    //-----------------------------------------------------------------
    private class TimeWindowRecenterActionListener implements ActionListener
    {
    
        public void actionPerformed(ActionEvent evt)
        {
            recenterTimeWindowsAroundModelStartTime();
         }
  
    } //end TimeWindowRecenterActionListener 
    
    //-----------------------------------------------------------------  
    private class TimeWindowSlideListener implements ActionListener
    {
        private int _slideHours = 0;
        
        public TimeWindowSlideListener(int slideHours)
        {
            _slideHours = slideHours;
        }
    
        public void actionPerformed(ActionEvent evt)
        {
          
            _precipCanvas.slideTimeWindow(_slideHours);
            _stageCanvas.slideTimeWindow(_slideHours);
                
        }
  
    } //end TimeWindowSlideListener;
    
    //-----------------------------------------------------------------
    private class AdjustStageMouseListener extends MouseAdapter implements MouseMotionListener
    {
        private boolean _button3Down = false;
        private boolean _tipShownOnce = false;
 
        public void mouseDragged(MouseEvent event)
        {
   
           if (_button3Down)
           {
               Point point = event.getPoint();
               adjustStage(point);
           }
   
        }

        public void mouseMoved(MouseEvent event)
        {
            //do nothing
            return;
        }

        public void mouseReleased(MouseEvent event)
        {
            int button = event.getButton();
   
            if (button == MouseEvent.BUTTON3)
            {
                _button3Down = false; 
            }
     
            return;
        }

        public void mousePressed(MouseEvent event)
        {
            int button = event.getButton();
      
            if (button == MouseEvent.BUTTON3)
            {
                _button3Down = true;    
            }
        
            return;
    
        }
        
        
        public void mouseClicked(MouseEvent event)
        {
            if (event.getButton() == MouseEvent.BUTTON3)
            {
                adjustStage(event.getPoint());
            }
            else if ( (event.getButton() == MouseEvent.BUTTON1)  ||
            		 ( (event.getButton() == MouseEvent.BUTTON2)))
            {
                if (! _tipShownOnce)
                {
                    _tipShownOnce = true;
                    DialogHelper.displayMessageDialog(AnalysisWindow.this, 
                            		"TIP: To avoid accidental editing of the forecast hydrograph,\n the" +
                            		" application requires the user to click the right mouse\n button to edit the hydrograph."); 
                }
            }
           _button3Down = false; 
        
        } //end mouseClicked
       
        private void adjustStage(Point point)
        {
           AbsTimeMeasurement measurement =  _stageCanvas.getAbsTimeMeasurementByRounding(point);
           
           if (measurement.getValue() < 0.0)
           {
               measurement.setValue(0.0);    
           }
           
           if (measurement.getTime() >= _modelStartTimeHolder.getTime())//forecast precip
           {
               if (measurement.getValue(MeasuringUnit.feet) <= MAX_STAGE_IN_FEET)
               {
                   replaceTimeSeriesValue(getPrimaryStreamModel().getForecastStageTimeSeriesHolder(),
                                     measurement);
                                     
                   //redraw the StageCanvas
                   _stageCanvas.refresh();   
                            
                            
                   //let any observer know of the change                     
                   announceForecastStageEdited();                      
               }
           }

         
           
          
       
       }
        
    } //end StageAdjustmentTsCanvasEventListener 

    //-----------------------------------------------------------------
    private class AdjustPrecipMouseListener extends MouseAdapter implements MouseMotionListener, KeyListener
    
    {
        private boolean _button1Down = false;
        private boolean _tipShownOnce = false;
        
        //variables related to keyboard commands
        private boolean _hasFocus = false;
        private Point _mousePosition = null;
        
        //List _pointList = new ArrayList();
        
        public void mouseDragged(MouseEvent event)
        {
           // Point point = event.getPoint();
           // _pointList.add(point);
            
           
           if (_button1Down)
           {
               Point point = event.getPoint();
            //   System.out.println("activated mouseDragged");
            //   System.out.println("activated mouseDragged, button = " + button ); 
               adjustPrecip(point);
           }
           
        }
        
        public void mouseMoved(MouseEvent event)
        {
            _mousePosition = event.getPoint();
            //do nothing
        }
        
        public void mousePressed(MouseEvent event)
        {
            //System.out.println("Precip adjust listener: mousePressed()");
            int button = event.getButton();
            if (button == MouseEvent.BUTTON1)
            {
                _button1Down = true;    
            }
            else
            {
                if (! _tipShownOnce)
                {
                    _tipShownOnce = true;
                    DialogHelper.displayMessageDialog(AnalysisWindow.this, 
                            		"TIP: Use the left mouse button to edit precip.");
                            		 
                }
            }
         
            return;
    
        }
        
        
        public void mouseReleased(MouseEvent event)
        {
            //System.out.println("Precip adjust listener: mouseReleased()");
            int button = event.getButton();
           
            if (button == MouseEvent.BUTTON1)
            {
                _button1Down = false; 
                
                adjustPrecip(event.getPoint());
                
                //if I am delaying, I need to redraw when button released,
                //otherwise, adjustPrecip
                if (_delayRerunModelWhileDrawing)
                {
                     runModel();
                }   
            }
             
        }
        
        
        public void mouseEntered(MouseEvent event)
        {
            //get the focus on this canvas, so that the keyboard commands will work on it
            AnalysisWindow.this._precipCanvas.requestFocusInWindow();
          //  System.out.println("mouseEntered():" +  " requested focus ");
            _hasFocus = true;
        }

        public void mouseExited(MouseEvent event)
        {
           // System.out.println("mouseExited():" +  " requested focus elsewhere ");
            //System.out.println("Precip adjust listener: mouseExited()");
            
            //when exit the window, if was delaying drawing, stop delaying
            if ((_delayRerunModelWhileDrawing) && (_button1Down))
            {
                runModel();
            }   
            
            //make sure that some other widget has focus,
            //so that keyboard clicks are not registered by the
            //precip canvas listeners
            AnalysisWindow.this._stageCanvas.requestFocusInWindow();
          
            _hasFocus = false;
        }
        
        
       public void keyPressed(KeyEvent e)
       {
           int keyCode = e.getKeyCode();
           char keyChar = e.getKeyChar();
           
           System.out.println("keyPressed: " + keyChar);
           
           if ( ( keyCode == KeyEvent.VK_UP) || (keyCode == KeyEvent.VK_RIGHT) )
           {
                 System.out.println("Up typed");  
                 adjustPrecipByDeltaValue(0.01, _mousePosition);
           }
           
           if ( ( keyCode == KeyEvent.VK_DOWN)|| (keyCode == KeyEvent.VK_LEFT) )
           {
                 System.out.println("Down typed");
                 adjustPrecipByDeltaValue(-0.01, _mousePosition);
           }
       }
       
       public void keyReleased(KeyEvent e)
       {
         
       }
       
       public void keyTyped(KeyEvent e)
       {
        
       }
       
       private void adjustPrecipByDeltaValue(double changeInValue, Point point)
       {
           System.out.println("Precip adjust listener: adjustPrecipByDeltaValue()");
           
           AbsTimeMeasurement measurement =  _precipCanvas.getAbsTimeMeasurementByTruncation(point);
        
           // for precip, we are focused on the end time, so add an hour
           long endTime = measurement.getTime() + ( _intervalInHours * MILLIS_PER_HOUR);
        
           RegularTimeSeriesHolder tsHolder = getPrimaryStreamModel().getPrecipTimeSeriesHolder();
           
           AbsTimeMeasurement oldMeasurement = tsHolder.getTimeSeries().getAbsTimeMeasurementByTime(endTime);
           
           measurement.setValue(oldMeasurement.getValue() + changeInValue);
           measurement.setTime(endTime); 
           
           if (measurement.getValue() < 0.0)
           {
               measurement.setValue(0.0);    
           }
           
           TimeIntervalMeasurement newMeasurement = 
                             new TimeIntervalMeasurement(measurement.getValue(),
                                                         measurement.getTime(), 
                                                         _intervalInHours,
                                                         measurement.getUnit());  
                                               
                                      
           double value = measurement.getValue(MeasuringUnit.inches); 
           boolean valueIsInRange = false;
           boolean timeIsInRange = false;
           
           if (value <= MAX_PRECIP_IN_INCHES_PER_HOUR)  
           {
               valueIsInRange = true;    
           }
           
           
           //ending time at or before model start time is not allowed, since the period is before
           if (measurement.getTime() > getPrimaryStreamModel().getModelStartTime())
           {
               timeIsInRange = true;
           }
           
           if (valueIsInRange && timeIsInRange)  
           {
               RegularTimeSeriesHolder timeSeriesHolder = null;
               
               timeSeriesHolder = getPrimaryStreamModel().getPrecipTimeSeriesHolder();                                                  
               replaceTimeSeriesValue(timeSeriesHolder, newMeasurement);
               
              
               //redraw the PrecipCanvas
               _precipCanvas.refresh(); 
               
               // if not delaying, then rerun the model right now
               if (! _delayRerunModelWhileDrawing)
               {  
                   runModel();   
               }
           }
                 
       } //end adjustPrecipByValue
        
   
       private void adjustPrecip(Point point)
       {
           //System.out.println("Precip adjust listener: adjustPrecip()");
          
           AbsTimeMeasurement measurement =  _precipCanvas.getAbsTimeMeasurementByTruncation(point);
           
           // System.out.println(header + measurement);    
        
           if (measurement.getValue() < 0.0)
           {
               measurement.setValue(0.0);    
           }
           
           // for precip, we are focused on the end time, so add an hour
           long endTime = measurement.getTime() + ( _intervalInHours * MILLIS_PER_HOUR);
           measurement.setTime(endTime); 
         
           TimeIntervalMeasurement newMeasurement = 
                             new TimeIntervalMeasurement(measurement.getValue(),
                                                         measurement.getTime(), 
                                                         _intervalInHours,
                                                         measurement.getUnit());  
                                               
                                      
           double value = measurement.getValue(MeasuringUnit.inches); 
           boolean valueIsInRange = false;
           boolean timeIsInRange = false;
           
           if (value <= MAX_PRECIP_IN_INCHES_PER_HOUR)  
           {
               valueIsInRange = true;    
           }
           
           
           //ending time at or before model start time is not allowed, since the period is before
           if (measurement.getTime() > getPrimaryStreamModel().getModelStartTime())
           {
               timeIsInRange = true;
           }
           
           if (valueIsInRange && timeIsInRange)  
           {
               RegularTimeSeriesHolder timeSeriesHolder = null;
               
               timeSeriesHolder = getPrimaryStreamModel().getPrecipTimeSeriesHolder();                                                  
               replaceTimeSeriesValue(timeSeriesHolder, newMeasurement);
               
              
               //redraw the PrecipCanvas
               _precipCanvas.refresh(); 
               
               // if not delaying, then rerun the model right now
               if (! _delayRerunModelWhileDrawing)
               {  
                   runModel();   
               }
           }
                 
       } //end adjustPrecip
        
    } //end PrecipAdjustmentTsCanvasEventListener
  
  	//--------------------------------------------------------------------
    private class ModelRunStartingStateActionListener implements ActionListener
    {

        private JComboBox _comboBox = null;

        public ModelRunStartingStateActionListener(JComboBox comboBox)
        {
            _comboBox = comboBox;
        }
        
       
        public void actionPerformed(ActionEvent evt)
        {
            
            DialogHelper.setWaitCursor(AnalysisWindow.this);
            
            
            if (_comboBox == AnalysisWindow.this._apiMkcStartingStateComboBox)
            {
                if ( ! _reloadingApiMkcStartingStateComboBox )
                {
                    setApiMkcModelState();
                }    
            }
            else if (_comboBox == AnalysisWindow.this._sacStartingStateComboBox)
            {
                if ( ! _reloadingSacStartingStateComboBox)
                {
                    setSacModelState();
                }
            }    
           
            DialogHelper.setDefaultCursor(AnalysisWindow.this);
            
        }
  
    } //end ModelRunStartTimeActionListener;
  
    //--------------------------------------------------------------------
    private class CustomModelRunDateTimeChangedDocumentListener implements DocumentListener   
    {
	
		public void insertUpdate(DocumentEvent arg0)
		{
            //don't bother to rerun the model if you aren't supposed to use this time anyway
            if (_apiMkcUseCustomModelRunTime)
            {
                setApiMkcModelState();
                runModel();
            }
		}

		
		public void removeUpdate(DocumentEvent arg0)
		{
            //System.out.println("CustomModelRunDateTimeChangedActionListener.removeUpdate()");	
			
		}

		
		public void changedUpdate(DocumentEvent arg0)
		{
           // System.out.println("CustomModelRunDateTimeChangedActionListener.changedUpdate()");
           // setApiMkcModelState();
           // runModel();
			
		}
        
    }
 
    private class  LastObsTimeForAdjustmentChangedDocumentListener implements DocumentListener   
    {
	
		public void insertUpdate(DocumentEvent arg0)
		{
		    
		//    String header = "LastObsTimeForAdjustmentChangedDocumentListener.insertUpdate(): ";	
		    	
		    
		    long lastTime = _adjustmentLastObsInputTimeTextField.getTime();
		    
		  
		//	System.out.println(header + " lastTime = " +
		//	        		DbTimeHelper.getDateTimeStringFromLongTime(lastTime));
		    
		    getPrimaryStreamModel().setLastObservedTimeForAdjustment(lastTime);
		    
            //don't bother to rerun the model if you aren't supposed to use this time anyway
            if (getPrimaryStreamModel().isAdjustmentOn())
            {
               
                runModel();
             }
		}

		
		public void removeUpdate(DocumentEvent arg0)
		{
			
		}

		
		public void changedUpdate(DocumentEvent arg0)
		{
     		
		}
        
    }
  
    //--------------------------------------------------------------------   
    private class ModelChoiceListener implements ActionListener
    {
        //private int _invocationCount = 0;

        public void actionPerformed(ActionEvent event)
        {
          // _invocationCount ++;
      //    System.out.println("---------------ModelChoiceListener.actionPerformed() invoked " + _invocationCount + " times.");     
            
           setModelTypeToComboBoxSelection();
        }
    } 
    //--------------------------------------------------------------------   
    private class UnitHydrographChoiceListener implements ActionListener
    {
        //private int _invocationCount = 0;

        public void actionPerformed(ActionEvent event)
        {       
           setUnitHydrographToComboBoxSelection();
        }
    } 
  
    //--------------------------------------------------------------------   
    private  class ForecastHeightTimeSeriesLaunchActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
           RegularTimeSeriesEditor editor = null;
           
           RegularTimeSeriesHolder tsHolder = getPrimaryStreamModel().getForecastStageTimeSeriesHolder();
           String titleString = "Forecast Stage Editor";
           String unitString = "Unknown Units";
           if (tsHolder.getTimeSeries() != null)
           {
               unitString = tsHolder.getTimeSeries().getMeasuringUnit().getName();   
           }
           String valueLabelString = "Stage (" + unitString + ")";
           ValueMapper valueMapper = new StageToFlowValueMapper(getPrimaryStreamModel().getRatingCurve());
           
           String mappedUnitString =  valueMapper.getOutputMeasuringUnit().getName();
           String mappedValueLabelString = "Discharge(" + mappedUnitString + ")";
          
           boolean isModal = false;
           boolean isEditable = true;
           
           long initialSearchTime = 0;
           double missingValue = _defaultMissingValue;
           


           RegularTimeSeriesEditorDescriptor descriptor = new RegularTimeSeriesEditorDescriptor(AnalysisWindow.this,
                                                tsHolder,
                                                titleString, valueLabelString, 
                                                _defaultFormatString,
                                                 isEditable,
                                                 isModal,
                                                 valueMapper,
                                                 mappedValueLabelString,
                                                 initialSearchTime,
                                                 missingValue,
                                                 MIN_STAGE_IN_FEET,
                                                 MAX_STAGE_IN_FEET);

            editor = new RegularTimeSeriesEditor(descriptor);
            
          // editor.addApplyActionListener(new ForecastHeightTimeSeriesApplyActionListener());
           editor.setVisible(true); 
        }
    }

    //--------------------------------------------------------------------   
    private  class PrecipTimeSeriesLaunchActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
             RegularTimeSeriesEditor editor = null;
           
             RegularTimeSeriesHolder tsHolder = getPrimaryStreamModel().getPrecipTimeSeriesHolder();
             String titleString = "Mean Areal Precip Editor";
             
             String unitString = "Unknown Units";
             if (tsHolder.getTimeSeries() != null)
             {
                 unitString = tsHolder.getTimeSeries().getMeasuringUnit().getName();   
             }
             String valueLabelString = "Precip (" + unitString + ")";
           
             boolean isEditable = true;
             boolean isModal = false;
             
             ValueMapper valueMapper = null;
             String mappedValueLabelString = "BOGUS";

             long initialSearchTime = getPrimaryStreamModel().getModelStartTime() + _millisPerHour;
             double missingValue = _defaultMissingValue;
           


             RegularTimeSeriesEditorDescriptor descriptor = new RegularTimeSeriesEditorDescriptor(AnalysisWindow.this,
                                                tsHolder,
                                                titleString, valueLabelString, 
                                                _defaultFormatString,
                                                 isEditable,
                                                 isModal,
                                                 valueMapper,
                                                 mappedValueLabelString,
                                                 initialSearchTime,
                                                 missingValue,
                                                 MIN_PRECIP_IN_INCHES_PER_HOUR,
                                                 MAX_PRECIP_IN_INCHES_PER_HOUR);

             editor = new RegularTimeSeriesEditor(descriptor);
             editor.setVisible(true);
             
             return;
        }
    }

   //--------------------------------------------------------------------  
    private class EvaporationTimeSeriesLaunchActionListener implements
            ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
            RegularTimeSeriesEditor editor = null;

            RegularTimeSeriesHolder tsHolder = getPrimaryStreamModel()
                    .getEvaporationTimeSeriesHolder();
            String titleString = "Mean Areal Potential Evaporation";

            String unitString = "Unknown Units";
            if (tsHolder.getTimeSeries() != null)
            {
                unitString = tsHolder.getTimeSeries().getMeasuringUnit()
                        .getName();
            }
            String valueLabelString = "Evap. (" + unitString + ")";

            //editor = new RegularTimeSeriesEditor(AnalysisWindow.this, tsHolder, title, valueLabelString);

            boolean isEditable = true;
            boolean isModal = false;

            ValueMapper valueMapper = null;
            String mappedValueLabelString = "BOGUS";


            long initialSearchTime = getPrimaryStreamModel().getModelStartTime();
            double missingValue = _defaultMissingValue;



            RegularTimeSeriesEditorDescriptor descriptor = new RegularTimeSeriesEditorDescriptor(
                    AnalysisWindow.this, tsHolder, titleString,
                    valueLabelString, _defaultFormatString, isEditable,
                    isModal, valueMapper, mappedValueLabelString,
                    initialSearchTime, missingValue, MIN_EVAP_PER_HOUR,
                    MAX_EVAP_PER_HOUR);


            editor = new RegularTimeSeriesEditor(descriptor);

            editor.setVisible(true);

        }
    }

    //--------------------------------------------------------------------
    private class PriorRunoffTimeSeriesLaunchActionListener implements
            ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
            RegularTimeSeriesEditor editor = null;

            RegularTimeSeriesHolder tsHolder = getPrimaryStreamModel()
                    .getPriorRunoffTimeSeriesHolder();
            String titleString = "Prior Computed Runoff ";

            RegularTimeSeries ts = tsHolder.getTimeSeries();
            MeasuringUnit unit = null;

            String unitString = "Unknown Units";
            if (tsHolder.getTimeSeries() != null)
            {
                unitString = tsHolder.getTimeSeries().getMeasuringUnit()
                        .getName();
                String valueLabelString = "Runoff (" + unitString + ")";

                boolean isEditable = false;
                boolean isModal = false;

                ValueMapper valueMapper = null;
                String mappedValueLabelString = "BOGUS";


                long initialSearchTime = getPrimaryStreamModel().getModelStartTime();
                double missingValue = _defaultMissingValue;


                RegularTimeSeriesEditorDescriptor descriptor = new RegularTimeSeriesEditorDescriptor(
                        AnalysisWindow.this, tsHolder, titleString,
                        valueLabelString, _defaultFormatString, isEditable,
                        isModal, valueMapper, mappedValueLabelString,
                        initialSearchTime, missingValue,
                        MIN_RUNOFF_IN_INCHES_PER_HOUR,
                        MAX_RUNOFF_IN_INCHES_PER_HOUR);


                editor = new RegularTimeSeriesEditor(descriptor);


                //this is not editable, so there is no apply action listener
                //editor.addApplyActionListener(new PriorRunoffTimeSeriesApplyActionListener());

                editor.setVisible(true);

            }

            else
            {
                //JOptionPane.showMessageDialog(AnalysisWindow.this, "Sorry, there is no prior computed runoff.");  
                DialogHelper.displayMessageDialog(AnalysisWindow.this,
                        "Sorry, there is no prior computed runoff.");
            }

        }
    }

    //--------------------------------------------------------------------
    
    private class ShowUnadjustedStatesActionListener implements ActionListener
    {
           
        public void actionPerformed(ActionEvent event)
        {
            JCheckBoxMenuItem checkBox = (JCheckBoxMenuItem) event.getSource();
            _showUnadjustedStates = checkBox.getState();
            
            initSacStateComboBox(_sacStartingStateComboBox);
        }
    }
    
    //--------------------------------------------------------------------
    
    
    private class RunoffTimeSeriesLaunchActionListener implements
            ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
            RegularTimeSeriesEditor editor = null;

            RegularTimeSeriesHolder tsHolder = getPrimaryStreamModel()
                    .getRunoffTimeSeriesHolder();
            String titleString = "Computed Runoff ";

            String unitString = "Unknown Units";
            if (tsHolder.getTimeSeries() != null)
            {
                unitString = tsHolder.getTimeSeries().getMeasuringUnit()
                        .getName();
            }
            String valueLabelString = "Runoff (" + unitString + ")";



            boolean isEditable = false;
            boolean isModal = false;

            ValueMapper valueMapper = null;
            String mappedValueLabelString = "BOGUS";

            long initialSearchTime = getPrimaryStreamModel().getModelStartTime()
                    + _millisPerHour;
            double missingValue = _defaultMissingValue;


            RegularTimeSeriesEditorDescriptor descriptor = new RegularTimeSeriesEditorDescriptor(
                    AnalysisWindow.this, tsHolder, titleString,
                    valueLabelString, _defaultFormatString, isEditable,
                    isModal, valueMapper, mappedValueLabelString,
                    initialSearchTime, missingValue,
                    MIN_RUNOFF_IN_INCHES_PER_HOUR,
                    MAX_RUNOFF_IN_INCHES_PER_HOUR);

            editor = new RegularTimeSeriesEditor(descriptor);
            editor.setVisible(true);
        }
    }

    //--------------------------------------------------------------------
    private class ForecastHeightSaveLaunchActionListener implements
            ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
            ForecastTsSaveDialog dialog = null;

            RegularTimeSeriesHolder tsHolder =
                   getPrimaryStreamModel().getForecastStageTimeSeriesHolder();
            String title = "Forecast Height Time Series Save Dialog";
            String locId = getPrimaryStreamModel().getLocationId();

            String tableName = "FcstHeight";
      //      String[] physicalElementStringArray = {"HG", "HP", "HT"};
            
            String[] physicalElementStringArray = getPrimaryStreamModel().getPeArray('H');
            
            String primaryPe = getPrimaryStreamModel().getPrimaryPe();  
            String preferredPhysicalElement = primaryPe;
            
            if (primaryPe.charAt(0) != 'H')
            {
                preferredPhysicalElement = "HG";
            }

            dialog = new ForecastTsSaveDialog(AnalysisWindow.this, locId,
                    title, tsHolder, getPrimaryStreamModel().getDataMgr(), tableName,
                    physicalElementStringArray,
                    preferredPhysicalElement);

            dialog.setVisible(true);

        }
    }

    //--------------------------------------------------------------------
    private class ForecastDischargeSaveLaunchActionListener implements
            ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
            ForecastTsSaveDialog dialog = null;

            RegularTimeSeries dischargeTimeSeries = getPrimaryStreamModel()
                    .getDischargeTimeSeries();
            RegularTimeSeriesHolder dischargeTimeSeriesHolder = new RegularTimeSeriesHolder();
            dischargeTimeSeriesHolder.setTimeSeries(dischargeTimeSeries);

            String title = "Forecast Discharge Time Series Save Dialog";
            String locId = getPrimaryStreamModel().getLocationId();

            String tableName = "FcstDischarge";
           //String[] physicalElementStringArray = {"QR"};
            
            String[] physicalElementStringArray = getPrimaryStreamModel().getPeArray('Q');
            
            String primaryPe = getPrimaryStreamModel().getPrimaryPe();
            String preferredPhysicalElement = primaryPe;
            if (primaryPe.charAt(0) != 'Q')
            {
                preferredPhysicalElement = "QR";
            }
  
            dialog = new ForecastTsSaveDialog(AnalysisWindow.this, locId,
                    title, dischargeTimeSeriesHolder,
                    getPrimaryStreamModel().getDataMgr(), tableName,
                    physicalElementStringArray,
                    preferredPhysicalElement);

            dialog.setVisible(true);

        }
    }

    //--------------------------------------------------------------------
    private class EvapSaveLaunchActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
            ForecastTsSaveDialog dialog = null;

            RegularTimeSeriesHolder tsHolder = getPrimaryStreamModel()
                    .getEvaporationTimeSeriesHolder();
            String title = "Evapotranspiration Time Series Save Dialog";
            String locId = getPrimaryStreamModel().getLocationId();

            String tableName = "FcstOther";
            String[] physicalElementStringArray = {"EA"};
            String preferredPhysicalElement = "EA";
       
            dialog = new ForecastTsSaveDialog(AnalysisWindow.this, locId,
                            title, tsHolder, getPrimaryStreamModel().getDataMgr(), tableName,
                            physicalElementStringArray,
                            preferredPhysicalElement);
            dialog.addApplyActionListener(new ForecastOtherSaveCompleteListener());

            dialog.setVisible(true);

        }
    }
    
    // ----------------------------------------------------------------------------------------------
    
    private class MAPSaveLaunchActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        { 
            usePrecipFileChooserForSaving();
        }
    }

    //--------------------------------------------------------------------
    private class SacParamsLaunchActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {

            String basinId = getPrimaryStreamModel().getBasinId();

            SacParamsEditor editor = null;
            editor = new SacParamsEditor(AnalysisWindow.this,  getPrimaryStreamModel().getDataMgr(), basinId);

            editor.addApplyActionListener(new SacParamsSaveCompleteActionListener());

            editor.setVisible(true);
        }
    }
    
  
    //--------------------------------------------------------------------
    
    private class SetInitialVarStateActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
            
            boolean confirmed = false;

            confirmed =  DialogHelper.displayConfirmDialog(AnalysisWindow.this,        
                    "Do you want to turn the currently selected model conditions into\n" +
                    "the initial VAR conditions (moving forward) ?",
                    "Confirm VAR state Reinitialization"); 

           if (confirmed)
           {
               
               DialogHelper.displayMessageDialog(AnalysisWindow.this, "Reinitializing initial VAR conditions");
                
               SSHPSource source = SSHPSource.UNADJUSTED_VAR;
               
               StreamModel primaryStreamModel = getPrimaryStreamModel();
               primaryStreamModel.saveInitialSacSmaConditions(source);
               
               initSacStateComboBox(_sacStartingStateComboBox);
               
           }
           else  //rejected
           {
               DialogHelper.displayMessageDialog(AnalysisWindow.this, "Cancelling reinitialization of initial VAR conditions");
           }
                 
        }
    }

    //--------------------------------------------------------------------
    private class AdjustToggleActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {           
            boolean isAdjustmentOn = _adjustmentCheckBox.isSelected();
            getPrimaryStreamModel().setAdjustmentOn(isAdjustmentOn);
           runModel();
        }
    }
    //--------------------------------------------------------------------
    private class AdjustmentHoursChangeListener implements ChangeListener
    {
        public void stateChanged(ChangeEvent e)
        {
               
            Integer integer =(Integer) _blendPeriodSpinner.getValue();
            
            int newValue = integer.intValue();
                
          //  _blendPeriodValueLabel.setText(newValue + "" );
            
            getPrimaryStreamModel().setAdjustmentHours(newValue);
            
            if (getPrimaryStreamModel().isAdjustmentOn())
            {  
                runModel();
            }
        }


    }
    
    //--------------------------------------------------------------------
    
    private class LiveSacStatesAdjustmentActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent event)
        {
           // final String header = "LiveSacStatesAdjustmentActionListener.actionPeformed(): ";
           // System.out.println(header + "message = " + event.getActionCommand());
          
            runModel();
        }
    }
    //--------------------------------------------------------------------

    private class ForecastHoursChangeListener implements ChangeListener
    {
        public void stateChanged(ChangeEvent e)
        {          
            Integer integer =(Integer) _forecastHoursSpinner.getValue();   
            int newValue = integer.intValue();            
           
            _modelManager.setForecastLengthInHours(newValue);
           // getPrimaryStreamModel().setForecastLengthInHours(newValue);
            runModel();
           
        }
    }
    //--------------------------------------------------------------------

    private class SacStateLaunchActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
            String basinId = getPrimaryStreamModel().getBasinId();

            SacStateEditor editor = null;
            editor = new SacStateEditor(AnalysisWindow.this, 
                    getPrimaryStreamModel().getDataMgr(), basinId);

            editor.addApplyActionListener(new SacStateSaveCompleteActionListener());
            editor.setVisible(true);

        }
    }

    
    private class LiveSacStateLaunchActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
            boolean isModal = false;
            
            //used by AnalysisWindow to call runModel();
            ActionListener liveSacStatesAdjustmentActionListener = 
                			new LiveSacStatesAdjustmentActionListener();
                  
            LiveSacStateEditor editor = new LiveSacStateEditor(AnalysisWindow.this, 
                                            getPrimaryStreamModel().getSacState(),
                                            getPrimaryStreamModel().getSacParams(),
                                            isModal, 
                                            liveSacStatesAdjustmentActionListener);
                    
            editor.setVisible(true);

        }
    }

    //--------------------------------------------------------------------

    private class SacStateSaveCompleteActionListener implements ActionListener
    {
        // make sure that the model settings are reloaded and
        // then rerun the model
        public void actionPerformed(ActionEvent evt)
        {


            getPrimaryStreamModel().reloadModelData();


            getPrimaryStreamModel().runModel();

        }
    }

    //--------------------------------------------------------------------


    private class SacParamsSaveCompleteActionListener implements ActionListener
    {
        // make sure that the model settings are reloaded and
        // then rerun the model
        public void actionPerformed(ActionEvent evt)
        {
            getPrimaryStreamModel().reloadModelData();

            getPrimaryStreamModel().runModel();

        }
    }

    //--------------------------------------------------------------------

    private class MonthlyMapeEditorLaunchActionListener implements
            ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
            MonthlyValueEditor editor = null;

            JFrame ownerFrame = AnalysisWindow.this;
            DataMgr dataMgr = getPrimaryStreamModel().getDataMgr();
            String basinId = getPrimaryStreamModel().getBasinId();
            boolean isModal = false;
            String pe = "EA";

            editor = new MonthlyValueEditor(ownerFrame, dataMgr, basinId, pe,
                    isModal);
            editor.setVisible(true);

        }
    }

    //--------------------------------------------------------------------

    private class ForecastOtherSaveCompleteListener implements ActionListener
    {
        // a listener for the forecast save dialog
        public void actionPerformed(ActionEvent event)
        {
            //System.out.println("ForecastOtherSaveCompleteListener invoked");
            //do nothing, needed to fulfill editor class obligations
        }

    }

    //--------------------------------------------------------------------

    private class ForecastHeightTimeSeriesChangeListener implements
            TimeSeriesListener
    {
        public Object getReceiver()
        {
            return AnalysisWindow.this;
        }

        public void handleTimeSeriesEvent(TimeSeriesEvent event)
        {
           //  String header = "ForecastHeightTimeSeriesChangeListener.handleTimeSeriesEvent()";             
           //  System.out.println(header +  "event = " + event.toString());
            redrawCanvases();

        }
    }

    //--------------------------------------------------------------------

    private class PrecipTimeSeriesChangeListener implements TimeSeriesListener
    {

        public Object getReceiver()
        {
            return AnalysisWindow.this;
        }

        // used for receiving events from precip tabular editing
        public void handleTimeSeriesEvent(TimeSeriesEvent event)
        {
            //String header = "PrecipTimeSeriesChangeListener.handleTimeSeriesEvent()";
           // System.out.println(header + "event = " + event.toString());
            runModel();

        }

    }

    //  --------------------------------------------------------------------

    private class EvaporationTimeSeriesChangeListener implements
            TimeSeriesListener
    {

        public Object getReceiver()
        {
            return AnalysisWindow.this;
        }

        // used for tabular editing of the evaporation time series
        public void handleTimeSeriesEvent(TimeSeriesEvent event)
        {
            //String header = "EvaporationTimeSeriesChangeListener.handleTimeSeriesEvent()";
            //System.out.println(header + "event = " + event.toString());
            runModel();

        }

    }

    //  --------------------------------------------------------------------

    private class ScreenCaptureActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent event)
        {
            
           useCaptureScreenFileChooserForSaving(); 
         //  AnalysisWindow.this.displayCaptureScreenFileChooser();

        } //end actionPerformed

    } //end ScreenCaptureActionListener

    //--------------------------------------------------------------------
    
	//	-----------------------------------------------------------------
} //end class AnalysisWindow
