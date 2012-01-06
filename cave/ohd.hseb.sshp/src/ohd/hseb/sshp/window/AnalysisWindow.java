/*
 * Created on Jul 18, 2003
 *
 * 
 */
package ohd.hseb.sshp.window;

import java.awt.AWTException;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.image.BufferedImage;
import java.io.File;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import javax.imageio.ImageIO;
import javax.imageio.stream.FileImageOutputStream;
import javax.imageio.stream.ImageOutputStream;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JSpinner;
import javax.swing.JSplitPane;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;
import javax.swing.border.Border;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.filechooser.FileFilter;

import ohd.hseb.measurement.AbsTimeMeasurement;
import ohd.hseb.measurement.Measurement;
import ohd.hseb.measurement.MeasuringUnit;
import ohd.hseb.measurement.RegularTimeSeries;
import ohd.hseb.measurement.RegularTimeSeriesHolder;
import ohd.hseb.measurement.TimeIntervalMeasurement;
import ohd.hseb.measurement.TimeSeriesEvent;
import ohd.hseb.measurement.TimeSeriesListener;
import ohd.hseb.model.RainfallRunoffModelType;
import ohd.hseb.model.SigRiverLevels;
import ohd.hseb.model.StageToFlowValueMapper;
import ohd.hseb.sshp.AboutInfo;
import ohd.hseb.sshp.AppController;
import ohd.hseb.sshp.DataMgr;
import ohd.hseb.sshp.FFHDescriptor;
import ohd.hseb.sshp.SacSmaStateDescriptor;
import ohd.hseb.sshp.StreamModel;
import ohd.hseb.sshp.gui.GuiHelper;
import ohd.hseb.sshp.gui.PrecipPainter;
import ohd.hseb.sshp.gui.PrecipTotalTextPainter;
import ohd.hseb.sshp.gui.SettingPanel;
import ohd.hseb.sshp.gui.SigStageLinePainter;
import ohd.hseb.sshp.gui.StagePainter;
import ohd.hseb.sshp.gui.TimeLinePainter;
import ohd.hseb.sshp.gui.TsBackgroundPainter;
import ohd.hseb.util.CodeTracer;
import ohd.hseb.util.DataPoint;
import ohd.hseb.util.TimeHelper;
import ohd.hseb.util.TimeHolder;
import ohd.hseb.util.UnitValueMapper;
import ohd.hseb.util.ValueMapper;
import ohd.hseb.util.gui.DateTimeTextField;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.ExtensionFileFilter;
import ohd.hseb.util.gui.StandardImageIcons;
import ohd.hseb.util.gui.WindowResizingManager;
import ohd.hseb.util.gui.drawing.TsPaintableCanvas;

import com.raytheon.uf.common.ohd.AppsDefaults;

/**
 * @author Chip Gobs
 * 
 *         Main Frame for the Precip and Stage Canvases and the application
 *         itself.
 */
public class AnalysisWindow extends JFrame {
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

    // forecast length and spinner constants
    // see StreamModel.java for the default length in hours
    private static final int FORECAST_LENGTH_SPINNER_STEPS = 6;

    private static final MeasuringUnit _stageUnit = MeasuringUnit.feet;

    private static final String ALLOW_LIVE_SAC_STATE_ADJUSTMENT_TOKEN_NAME = "sshp_allow_live_state_adjustment";

    // determines whether to default to showing the simulated time series or not
    private static final String SHOW_SIMULATED_TS_TOKEN_NAME = "sshp_show_simulated_timeseries";

    private static final boolean _default_show_simulated_timeseries = true;

    // image storage directory
    private String _whfs_image_directoryString = null;

    private String _sshp_precip_directoryString = null;

    // window sizing data
    private Dimension _maxDimension = new Dimension(1280, 1024);

    private Dimension _initialDimension = new Dimension(1200, 944);

    private Dimension _minDimension = new Dimension(800, 700);

    private StreamModel _streamModel = null;

    private AppController _controller = null;

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

    // these Painters are in this class because of the ability to modify the way
    // the canvases are
    // painted. The modifications are made in this class.
    // The painters have special options which are invoked. Then, upon
    // repainting, the
    // painter paints in the way dictated by those options.
    private SigStageLinePainter _floodStageLinePainter = null;

    private SigStageLinePainter _majorFloodStageLinePainter = null;

    private StagePainter _observedStageTsPainter = null;

    private StagePainter _simulatedStageTsPainter = null;

    private PrecipPainter _precipPainter = null;

    private PrecipTotalTextPainter _precipTextTotalPainter = null;

    private TsBackgroundPainter _stageBackgroundPainter = null;

    private TsBackgroundPainter _precipBackgroundPainter = null;

    private Font _buttonFont = null;

    private Border _panelBorder = BorderFactory.createLineBorder(Color.black);

    // vcr buttons
    private JPanel _timeButtonPanel = null;

    private JButton _forwardButton = null;

    private JButton _forward1DayButton = null;

    private JButton _backButton = null;

    private JButton _back1DayButton = null;

    private JButton _recenterButton = null;

    // action buttons
    private JButton _closeButton = null;

    private JButton _windowLauncherButton = null;

    private JPanel _windowButtonPanel = null;

    private JButton _screenCaptureButton = null;

    // model control panel
    private JPanel _modelControlPanel = null;

    private JLabel _modelControlPanelLabel = null;

    private JLabel _modelTypeComboBoxLabel = null;

    private JComboBox _modelTypeComboBox = null;

    private String[] _modelTypeNameArray = null;

    private JLabel _uhgComboBoxLabel = null;

    private JComboBox _uhgComboBox = null;

    private JLabel _adjustmentCheckBoxLabel = null;

    private JCheckBox _adjustmentCheckBox = null;

    private JLabel _adjustmentLastObsInputTimeLabel = null;

    private DateTimeTextField _adjustmentLastObsInputTimeTextField = null;

    private JLabel _blendPeriodSpinnerLabel = null;

    private JSpinner _blendPeriodSpinner = null;

    // These share the same spot on the model control panel, depending on
    // the model being used
    private JComboBox _sacStartingStateComboBox = null;

    private JButton _sacStateListRefreshButton = null;

    private JComboBox _apiMkcStartingStateComboBox = null;

    private JButton _apiMkcStateRefreshButton = null;

    private JLabel _forecastHoursSpinnerLabel = null;

    private JSpinner _forecastHoursSpinner = null;

    // loose on the button panel
    private JLabel _modelRunStartTimeLabel = null;

    private JLabel _modelRunStartTimeValueLabel = null;

    // SAC_SMA control panel
    private JPanel _sacControlPanel = null;

    private JLabel _sacControlPanelLabel = null;

    private JButton _sacParamsButton = null;

    private JButton _sacStateButton = null;

    private JButton _sacMonthlyMapeValuesEditorButton = null;

    private JButton _sacLiveStateAdjustmentButton = null;

    private boolean _allowLiveSacAdjustment = false;

    // time series editing and saving panel
    private JMenuBar _menuBar = null;

    // API-MKC variable setting/resetting buttons for model settings
    private JPanel _apiMkcControlPanel = null;

    private JLabel _apiMkcSettingsLabel = null;

    private JLabel _modelStateLabel = null;

    private SettingPanel _apiMkcInitialStagePanel = null;

    private SettingPanel _apiMkcFfhPanel = null;

    private SettingPanel _apiMkcThreshRPanel = null;

    private JCheckBox _apiMkcUseCustomModelRunTimeCheckBox = null;

    private DateTimeTextField _apiMkcCustomModelRunDateTimeTextField = null;

    private JButton _apiMkcApplySettingsButton = null;

    // associated API-MKC variables
    private boolean _apiMkcUseCustomModelRunTime = false;

    // used to help control how events are processed
    private boolean _reloadingApiMkcStartingStateComboBox = false;

    private boolean _reloadingSacStartingStateComboBox = false;

    // graphing controls
    private JPanel _graphControlPanel = null;

    private JLabel _graphControlPanelLabel = null;

    private JCheckBox _showFloodStageCheckBox = null;

    private JCheckBox _showMajorFloodStageCheckBox = null;

    private JCheckBox _showObsHeightTsCheckBox = null;

    private JCheckBox _showPrecipAmountsCheckBox = null;

    private JCheckBox _delayRerunCheckBox = null;

    private JCheckBox _showMinorPrecipTicksCheckBox = null;

    private JCheckBox _showMinorStageTicksCheckBox = null;

    private JCheckBox _showSimulatedStageCheckBox = null;

    // booleans related to visual controls
    private boolean _forceShowFloodStage = true;

    private boolean _forceShowMajorFloodStage = false;

    private boolean _showPrecipAmounts = true;

    private boolean _showObservedHeightData = true;

    private boolean _delayRerunModelWhileDrawing = false;

    private boolean _showMinorPrecipTicks = false;

    private boolean _showMinorStageTicks = true;

    private boolean _showSimulatedStage = true;

    // main window layer
    private JPanel _mainPanel = null;

    private JPanel _buttonPanel = null;

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

    // debugging
    private CodeTracer _tracer = null;

    // ----constructors-------------------------------------

    public AnalysisWindow(StreamModel streamModel, AppController controller) {
        // init the other application objects that this object talks to
        _streamModel = streamModel;

        _controller = controller;

        _tracer = new CodeTracer();

        _modelStartTimeHolder = _streamModel.getModelStartTimeHolder();

        getAppsDefaults();

        initGui();

    } // AnalysisWindow

    // ------------------------------------------------------------
    private void getAppsDefaults() {
        AppsDefaults ad = AppsDefaults.getInstance();

        // determine if the secret adjustment window should be available
        if (ad.getToken(ALLOW_LIVE_SAC_STATE_ADJUSTMENT_TOKEN_NAME) != null) {
            _allowLiveSacAdjustment = true;
        }

        // default value for the show simulated stage time series toggle button
        _showSimulatedStage = ad.getBoolean(SHOW_SIMULATED_TS_TOKEN_NAME,
                _default_show_simulated_timeseries);

        _sshp_precip_directoryString = ad.getToken("sshp_precip_dir");

        _whfs_image_directoryString = ad.getToken("whfs_image_dir");

    }

    // ------------------------------------------------------------

    private void initGui() {

        String header = "AnalysisWindow.initGui(): ";
        // System.out.println(header + "starting.");
        int x = 10;
        int y = 10;
        int width = 700;
        int height = 200;

        int horizontalMargin = 50;
        int verticalMargin = 40;

        Dimension prefSplitPaneSize = new Dimension(width + horizontalMargin,
                (2 * height) + verticalMargin);

        initTitle();

        initMenuBar();

        // set up the 2 canvases and all of their containers
        initPrecipCanvas(x, y, width, height, _modelStartTimeHolder);
        initStageCanvas(x, y, width, height, _modelStartTimeHolder);

        _splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, _precipCanvas,
                _stageCanvas);

        _splitPane.setOneTouchExpandable(true);
        _splitPane.setPreferredSize(prefSplitPaneSize);
        // _splitPane.setDividerLocation(.50);

        // init "loose" GUI items
        initStatusAndReloadControls();

        // init model control panel
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

        // panel for the model run start time Labels
        JPanel modelRunStartTimePanel = new JPanel();
        modelRunStartTimePanel.add(_modelRunStartTimeLabel);
        modelRunStartTimePanel.add(_modelRunStartTimeValueLabel);

        // col, row numCols numRows Wcol wrow
        // row 0
        buttonGbc.insets = new Insets(5, 2, 5, 2);

        addComponent(_buttonPanel, _modelControlPanel, buttonGbc, 0, 0, 1, 3,
                0, 0);
        addComponent(_buttonPanel, _graphControlPanel, buttonGbc, 1, 0, 1, 3,
                0, 0);
        // only 1 of the following panels shows at a time

        addComponent(_buttonPanel, _apiMkcControlPanel, buttonGbc, 2, 0, 1, 3,
                0, 0);
        addComponent(_buttonPanel, _sacControlPanel, buttonGbc, 2, 0, 1, 3, 0,
                0);

        addComponent(_buttonPanel, modelRunStartTimePanel, buttonGbc, 0, 4, 1,
                1, 0, 0);

        // col, row numCols numRows Wcol wrow

        // row 6

        buttonGbc.insets = new Insets(5, 5, 5, 5);
        buttonGbc.anchor = GridBagConstraints.WEST;
        addComponent(_buttonPanel, _windowButtonPanel, buttonGbc, 1, 4, 1, 1,
                1, 0);

        // create the main panel
        _mainPanel = new JPanel();

        // add the components to the panel
        GridBagLayout layoutMgr = new GridBagLayout();

        GridBagConstraints mainGbc = new GridBagConstraints();
        mainGbc.fill = GridBagConstraints.BOTH;
        mainGbc.anchor = GridBagConstraints.NORTHWEST;
        mainGbc.weightx = 1;
        mainGbc.weighty = 1;

        _mainPanel.setLayout(layoutMgr);

        // add components to the main panel

        // col, row numCols numRows Wcol wrow

        setJMenuBar(_menuBar);
        addComponent(_mainPanel, _splitPane, mainGbc, 0, 0, 3, 10, 1, 1);
        addComponent(_mainPanel, _buttonPanel, mainGbc, 0, 10, 1, 2, 0, 0);

        // set up initial bounds limitations
        // see WindowResize listener for the minimum setting

        Rectangle maxBoundsRectangle = new Rectangle(_maxDimension);
        this.setMaximizedBounds(maxBoundsRectangle);

        // add the panel to the Frame and initialize the frame
        this.getContentPane().add(_mainPanel);
        this.pack();

        Rectangle initialBoundsRectangle = new Rectangle(_initialDimension);
        this.setBounds(initialBoundsRectangle);

        // set up the KC-API settings
        resetInitialStageMeasurementFromModel();
        resetFfhFromModel();
        resetThresholdRunoffFromModel();

        // make sure that the appropriate controls are visible
        setComponentVisibility();

        // add all of the event handlers
        addListeners();

        // select the RainfallRunoffModel
        initModelChoice();
    }

    // ------------------------------------------------------------
    private String getCenteredText(String origText) {
        String centeredText = "<HTML><CENTER>" + origText + "</CENTER></HTML>";

        return centeredText;
    }

    // ------------------------------------------------------------
    private String getVerticalText(String origText) {
        char[] charArray = origText.toCharArray();
        StringBuffer buffer = new StringBuffer();

        buffer.append("<HTML>");
        for (int i = 0; i < charArray.length; i++) {
            buffer.append(charArray[i]);

            // don't do a break for the last one
            if (i < charArray.length - 1) {
                buffer.append("<BR>");
            }
        }
        buffer.append("</HTML>");

        String verticalText = buffer.toString();

        return verticalText;
    }

    // ------------------------------------------------------------

    private void initTimeButtonPanel() {
        Dimension dimension = new Dimension(24, 28);

        Color buttonColor = this.getBackground();
        Color imageColor = Color.BLUE;

        ImageIcon rightArrowIcon = StandardImageIcons.getImageIcon(
                StandardImageIcons.RIGHT_ARROW, imageColor, buttonColor,
                dimension);
        ImageIcon leftArrowIcon = StandardImageIcons.getImageIcon(
                StandardImageIcons.LEFT_ARROW, imageColor, buttonColor,
                dimension);

        ImageIcon doubleRightArrowIcon = StandardImageIcons.getImageIcon(
                StandardImageIcons.DOUBLE_RIGHT_ARROW, imageColor, buttonColor,
                dimension);

        ImageIcon doubleLeftArrowIcon = StandardImageIcons.getImageIcon(
                StandardImageIcons.DOUBLE_LEFT_ARROW, imageColor, buttonColor,
                dimension);

        ImageIcon circleIcon = StandardImageIcons.getImageIcon(
                StandardImageIcons.CIRCLE, imageColor, buttonColor, dimension);

        _forwardButton = GuiHelper.getJButton(rightArrowIcon);
        _forwardButton.setToolTipText("Slide display forward 1 hour.");

        _forward1DayButton = GuiHelper.getJButton(doubleRightArrowIcon);
        _forward1DayButton.setToolTipText("Slide display forward 24 hours.");

        _recenterButton = GuiHelper.getJButton(circleIcon);
        _recenterButton
                .setToolTipText("Recenter display around the model run start time.");

        _backButton = GuiHelper.getJButton(leftArrowIcon);
        _backButton.setToolTipText("Slide display back 1 hour.");

        _back1DayButton = GuiHelper.getJButton(doubleLeftArrowIcon);
        _back1DayButton.setToolTipText("Slide display back 24 hours.");

        _timeButtonPanel = new JPanel();
        // _timeButtonPanel.setBorder(_panelBorder);
        _timeButtonPanel.add(_back1DayButton);
        _timeButtonPanel.add(_backButton);
        _timeButtonPanel.add(_recenterButton);
        _timeButtonPanel.add(_forwardButton);
        _timeButtonPanel.add(_forward1DayButton);

    } // end initTimeButtonPanel

    // ----------------------------------------------------------------------------------------------------
    private void initGraphControlPanel() {
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weightx = 1;
        gbc.weighty = 1;

        gbc.insets = new Insets(2, 2, 2, 2);

        _graphControlPanel = new JPanel();

        JPanel panel = _graphControlPanel;
        panel.setBorder(_panelBorder);

        panel.setLayout(new GridBagLayout());

        _graphControlPanelLabel = new JLabel(getCenteredText("Graph Controls:"));

        initTimeButtonPanel();

        // init the option buttons and the painters that are associated with
        // them
        _showFloodStageCheckBox = new JCheckBox("Scale To Flood Stage",
                _forceShowFloodStage);
        String text = "Display Horizontal Line to represent the flood stage";
        _showFloodStageCheckBox.setToolTipText(text);
        _floodStageLinePainter.setShouldPaint(_forceShowFloodStage);

        // init the option buttons and the painters that are associated with
        // them
        _showMajorFloodStageCheckBox = new JCheckBox(
                "Scale To Major Flood Stage", _forceShowMajorFloodStage);
        text = "Display Horizontal Line to represent the major flood stage";
        _showMajorFloodStageCheckBox.setToolTipText(text);
        if (_majorFloodStageLinePainter != null) {
            _majorFloodStageLinePainter
                    .setShouldPaint(_forceShowMajorFloodStage);
        }

        // init the option buttons and the painters that are associated with
        // them

        // precip totals
        _showPrecipAmountsCheckBox = new JCheckBox("Show Precip Amounts",
                _showPrecipAmounts);
        text = "Display precip totals (in inches) on the precip bars";
        _showPrecipAmountsCheckBox.setToolTipText(text);
        _precipPainter.setShowPrecipAmounts(_showPrecipAmounts);
        _precipTextTotalPainter.setShouldPaint(_showPrecipAmounts);

        // observed stages
        _showObsHeightTsCheckBox = new JCheckBox("Show Obs Stages",
                _showObservedHeightData);
        text = "Toggle Observed Stage Time Series";
        _showObsHeightTsCheckBox.setToolTipText(text);
        _observedStageTsPainter.setShouldPaint(_showObservedHeightData);

        // add simulated stage checkbox
        _showSimulatedStageCheckBox = new JCheckBox("Show Simulated Stages",
                _showSimulatedStage);
        text = "Toggle Simulated Stage Time Series";
        _showSimulatedStageCheckBox.setToolTipText(text);
        _simulatedStageTsPainter.setShouldPaint(_showSimulatedStage);

        // add delay button
        _delayRerunCheckBox = new JCheckBox("Delay Rerun While Drawing",
                _delayRerunModelWhileDrawing);
        text = "For graphical precip editing, postpones rerunning the model until after the button is released.";
        _delayRerunCheckBox.setToolTipText(text);

        // add minor precip tick checkbox
        _showMinorPrecipTicksCheckBox = new JCheckBox(
                "Show Minor Precip Lines", _showMinorPrecipTicks);
        text = "Toggle minor precip lines";
        _showMinorPrecipTicksCheckBox.setToolTipText(text);
        _precipBackgroundPainter.setShowMinorTicks(_showMinorPrecipTicks);

        // add minor stage tick checkbox
        _showMinorStageTicksCheckBox = new JCheckBox("Show Minor Stage Lines",
                _showMinorStageTicks);
        text = "Toggle minor stage lines";
        _showMinorStageTicksCheckBox.setToolTipText(text);
        _stageBackgroundPainter.setShowMinorTicks(_showMinorStageTicks);

        JPanel verticalSpacer = new JPanel();
        verticalSpacer.setPreferredSize(new Dimension(30, 34));

        // JPanel verticalSpacer2 = new JPanel();
        // verticalSpacer2.setPreferredSize(new Dimension(30, 8));

        // col, row numCols numRows Wcol wrow
        addComponent(panel, _graphControlPanelLabel, gbc, 0, 0, 2, 1, 0, 1);

        addComponent(panel, _timeButtonPanel, gbc, 0, 1, 2, 1, 0, 1);

        // addComponent(panel, verticalSpacer, gbc, 0, 2, 2, 1, 0, 1);

        addComponent(panel, _showObsHeightTsCheckBox, gbc, 0, 2, 1, 1, 0, 1);
        addComponent(panel, _showSimulatedStageCheckBox, gbc, 1, 2, 1, 1, 0, 1);

        addComponent(panel, _delayRerunCheckBox, gbc, 0, 3, 1, 1, 0, 1);
        addComponent(panel, _showFloodStageCheckBox, gbc, 1, 3, 1, 1, 0, 1);

        addComponent(panel, _showPrecipAmountsCheckBox, gbc, 0, 4, 1, 1, 0, 1);
        addComponent(panel, _showMajorFloodStageCheckBox, gbc, 1, 4, 1, 1, 0, 1);

        addComponent(panel, _showMinorPrecipTicksCheckBox, gbc, 0, 5, 1, 1, 0,
                1);
        addComponent(panel, _showMinorStageTicksCheckBox, gbc, 1, 5, 1, 1, 0, 1);

        // addComponent(panel, verticalSpacer2, gbc, 0, 6, 2, 1, 0, 1);

        return;
    } // end initVisualControlPanel

    // ----------------------------------------------------------------------------------------------------

    private void initWindowButtonPanel() {
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weightx = 1;
        gbc.weighty = 1;

        _windowLauncherButton = GuiHelper.getJButton("Control Window...");

        _screenCaptureButton = GuiHelper.getJButton("Capture Screen");

        _closeButton = GuiHelper.getJButton("Close");

        _windowButtonPanel = new JPanel();
        // _windowButtonPanel.setPreferredSize(new Dimension(150, 40));

        JPanel panel = _windowButtonPanel;

        // JPanel spacingPanel = new JPanel();
        // spacingPanel.setPreferredSize(new Dimension(160, 40));

        // spacingPanel.setBackground(Color.blue);
        // panel.setBackground(Color.yellow);

        // col, row numCols numRows Wcol wrow

        addComponent(panel, _windowLauncherButton, gbc, 0, 0, 1, 1, 1, 0);
        addComponent(panel, _screenCaptureButton, gbc, 1, 0, 1, 1, 1, 0);
        addComponent(panel, _closeButton, gbc, 2, 0, 1, 1, 1, 0);
        // addComponent(panel, spacingPanel, gbc, 1, 0, 1, 1, 1, 1);

        return;
    }

    // ----------------------------------------------------------------------------------------------------

    private void initApiMkcControlPanel() {
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

        // settings panels for KC-API
        _apiMkcSettingsLabel = new JLabel(getCenteredText("API-MKC Settings:"));

        // _apiMkcBaseflowPanel = new SettingPanel("Baseflow (cfs):", 6,
        // "Reload Baseflow");
        _apiMkcInitialStagePanel = new SettingPanel("Initial Stage(feet):", 6,
                "Reload", "Reload the initial stage from the time series.");
        toolTipText = "Initial stage in feet is converted to baseflow in cfs during model run.";
        _apiMkcInitialStagePanel.getLabel().setToolTipText(toolTipText);

        // _apiMkcFfhPanel = new SettingPanel("FFH (inches):", 6, "Reload FFH");
        _apiMkcFfhPanel = new SettingPanel("FFH(inches):", 6, "Reload",
                "Reload the headwater guidance from the database.");
        toolTipText = "Headwater guidance value in inches.";
        _apiMkcFfhPanel.getLabel().setToolTipText(toolTipText);

        // Threshold Runoff SettingPanel
        // _apiMkcThreshRPanel = new SettingPanel("Threshold Runoff (inches):",
        // 6, "Reload T. Runoff");
        _apiMkcThreshRPanel = new SettingPanel("T.Runoff(inches):", 6,
                "Reload", "Reload the threshold runoff.");
        toolTipText = "Threshold Runoff value in inches.";
        _apiMkcThreshRPanel.getLabel().setToolTipText(toolTipText);

        _apiMkcUseCustomModelRunTimeCheckBox = new JCheckBox("Use Custom Time",
                _apiMkcUseCustomModelRunTime);
        _apiMkcUseCustomModelRunTimeCheckBox
                .setToolTipText("Sets the Model Run Start Time by overriding the selected FFH time.");

        long latestHourTime = TimeHelper.truncateTimeInMillisToNearestHour(
                System.currentTimeMillis(), 1);
        _apiMkcCustomModelRunDateTimeTextField = new DateTimeTextField(
                latestHourTime, AnalysisWindow.this, "Model Run Start Time", 20);
        _apiMkcCustomModelRunDateTimeTextField.setAllowOnlyEvenHours(true);
        _apiMkcCustomModelRunDateTimeTextField.setPreferredSize(new Dimension(
                150, 20));
        _apiMkcCustomModelRunDateTimeTextField.setMaximumSize(new Dimension(
                150, 20));
        _apiMkcCustomModelRunDateTimeTextField.setMinimumSize(new Dimension(
                150, 20));

        // apply button
        _apiMkcApplySettingsButton = GuiHelper
                .getJButton(getVerticalText("Apply"));
        _apiMkcApplySettingsButton
                .setToolTipText("Applies manual changes to settings on the left.");

        // create a panel for vertical spacing purposes
        JPanel verticalSpacer = new JPanel();
        Dimension verticalSpacerDimension = new Dimension(30, 15);
        verticalSpacer.setPreferredSize(verticalSpacerDimension);
        verticalSpacer.setMaximumSize(verticalSpacerDimension);
        verticalSpacer.setMinimumSize(verticalSpacerDimension);

        // JPanel for layout purposes of the dateTimeTextField
        JPanel dateTimeTextFieldPanel = new JPanel();
        dateTimeTextFieldPanel.setLayout(new GridBagLayout());
        // dateTimeTextFieldPanel.setBackground(Color.yellow);
        dateTimeTextFieldPanel.setPreferredSize(new Dimension(209, 30));

        JPanel spacer1 = new JPanel();
        // spacer1.setBackground(Color.red);
        Dimension spacer1Dimension = new Dimension(50, 30);
        spacer1.setPreferredSize(spacer1Dimension);
        spacer1.setMaximumSize(spacer1Dimension);
        spacer1.setMinimumSize(spacer1Dimension);

        // add components to the special dateTimeTextFieldPanel
        // col, row numCols numRows Wcol wrow
        addComponent(dateTimeTextFieldPanel,
                _apiMkcCustomModelRunDateTimeTextField, gbc, 0, 0, 3, 1, 1, 0);
        addComponent(dateTimeTextFieldPanel, spacer1, gbc, 3, 0, 1, 1, 0, 0);

        // col, row numCols numRows Wcol wrow

        addComponent(panel, _apiMkcSettingsLabel, gbc, 0, 0, 2, 1, 0, 0);

        addComponent(panel, _apiMkcUseCustomModelRunTimeCheckBox, gbc, 0, 2, 1,
                1, 0, 0);
        addComponent(panel, dateTimeTextFieldPanel, gbc, 1, 2, 3, 1, 0, 0);

        addComponent(panel, _apiMkcInitialStagePanel.getLabel(), gbc, 0, 3, 1,
                1, 0, 0);
        addComponent(panel, _apiMkcInitialStagePanel.getTextField(), gbc, 1, 3,
                1, 1, 0, 0);
        addComponent(panel, _apiMkcInitialStagePanel.getButton(), gbc, 3, 3, 1,
                1, 0, 0);

        addComponent(panel, _apiMkcFfhPanel.getLabel(), gbc, 0, 4, 1, 1, 0, 0);
        addComponent(panel, _apiMkcFfhPanel.getTextField(), gbc, 1, 4, 1, 1, 0,
                0);
        addComponent(panel, _apiMkcFfhPanel.getButton(), gbc, 3, 4, 1, 1, 0, 0);

        addComponent(panel, _apiMkcThreshRPanel.getLabel(), gbc, 0, 5, 1, 1, 0,
                0);
        addComponent(panel, _apiMkcThreshRPanel.getTextField(), gbc, 1, 5, 1,
                1, 0, 0);
        addComponent(panel, _apiMkcThreshRPanel.getButton(), gbc, 3, 5, 1, 1,
                0, 0);

        // gbc.insets = new Insets(3, 3, 3, 3);
        addComponent(panel, _apiMkcApplySettingsButton, gbc, 2, 3, 1, 3, 0, 0);

        addComponent(panel, verticalSpacer, gbc, 1, 6, 1, 1, 0, 0);

    } // end initApiModelPanel()

    // ----------------------------------------------------------------------------------------------------

    private void initApiMkcStateComboBox(JComboBox comboBox) {
        String header = "AnalysisWindow.initApiMkcTimeComboBox(): ";
        // System.out.println(header + "starting");

        _reloadingApiMkcStartingStateComboBox = true;

        // load all of the possible values from the db
        List ffhDescriptorList = _streamModel.getFFHDescriptorList();
        boolean foundMatch = false;

        FFHDescriptor localSelectedDescriptor = null;

        // get a list of strings for this descriptor list

        comboBox.removeAllItems();

        // add the FFHDescriptors as items to the list

        // System.out.println(header + "initially selectedFFHDescriptor = " +
        // _selectedFFHDescriptor);

        for (int i = 0; i < ffhDescriptorList.size(); i++) {
            FFHDescriptor descriptor = (FFHDescriptor) ffhDescriptorList.get(i);

            // if the item being added equals the previously selected item,
            // note the item.
            if (_selectedFFHDescriptor != null) {
                // System.out.println(header + "selectedFFHDescriptor = " +
                // _selectedFFHDescriptor);
                // System.out.println(header +
                // "testing equality against Descriptor = " + descriptor);
                if ((!foundMatch)
                        && (_selectedFFHDescriptor.equals(descriptor)))

                {
                    localSelectedDescriptor = descriptor;
                    foundMatch = true;
                    // System.out.println(header + "localSelectedDescriptor = "
                    // + localSelectedDescriptor);
                }
            }

            // add it to the list
            comboBox.addItem(descriptor);
        }

        _reloadingApiMkcStartingStateComboBox = false;

        // select the already selected item, if there was one
        if (localSelectedDescriptor != null) {
            comboBox.setSelectedItem(localSelectedDescriptor);
        }

        return;

    } // end initKcApiTimeComboBox()

    // ----------------------------------------------------------------------------------------------------
    /*
     * private long getModelRunTimeFromKcApiComboBox() { long time = 0;
     * FFHDescriptor descriptor
     * =(FFHDescriptor)_apiMkcStartingStateComboBox.getSelectedItem();
     * 
     * if (descriptor != null) { time = descriptor.getTime(); }
     * 
     * return time; }
     */
    // ----------------------------------------------------------------------------------------------------
    private void initSacStateComboBox(JComboBox comboBox) {
        // prevent selection of the added items until it is done loading
        _reloadingSacStartingStateComboBox = true;

        comboBox.removeAllItems();

        List sacSmaStateDescriptorList = _streamModel
                .getSacSmaStateDescriptorList();
        SacSmaStateDescriptor localSelectedSacStateDescriptor = null;
        boolean foundMatch = false;

        // load each descriptor into the combobox. Also when you find the
        // selected descriptor store a reference to it.
        for (int i = 0; i < sacSmaStateDescriptorList.size(); i++) {
            SacSmaStateDescriptor descriptor = (SacSmaStateDescriptor) sacSmaStateDescriptorList
                    .get(i);

            if ((!foundMatch) && (_selectedSacStateDescriptor != null)
                    && (descriptor.equals(_selectedSacStateDescriptor))) {
                foundMatch = true;
                localSelectedSacStateDescriptor = descriptor;
            }

            comboBox.addItem(descriptor);

        }

        // set the combobox to the selected descriptor, if it exists
        _reloadingSacStartingStateComboBox = false;

        if (localSelectedSacStateDescriptor != null) {
            comboBox.setSelectedItem(localSelectedSacStateDescriptor);
        } else {
            comboBox.setSelectedIndex(0);
        }

        return;

    }

    // ----------------------------------------------------------------------------------
    private void initStatusAndReloadControls() {
        /*
         * init the following controls _modelRunStartTimeLabel
         * _modelRunStartTimeValueLabel
         */
        Dimension buttonDimension = new Dimension(120, 30);
        _modelRunStartTimeLabel = new JLabel("Model Run Start Time: ");

        // initialize the model run start time value label
        JPanel panel = _buttonPanel;

        _modelRunStartTimeValueLabel = new JLabel();
        String dateTimeString = getDateTimeStringToMinutes(_modelStartTimeHolder
                .getTime());
        _modelRunStartTimeValueLabel.setText(dateTimeString);

        return;
    }

    // ----------------------------------------------------------------------------------

    private void initModelControlPanel() {
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
        // _modelTypeComboBox.setPreferredSize(new Dimension(100, 35));

        _uhgComboBoxLabel = new JLabel("UHG:");
        initUHGComboBox();

        // API-MKC model state comboBox related controls

        // state refresh controls

        // _modelStateLabel = new JLabel("State:");
        _apiMkcStartingStateComboBox = GuiHelper.getJComboBox();
        initApiMkcStateComboBox(_apiMkcStartingStateComboBox);

        _apiMkcStateRefreshButton = GuiHelper.getJButton("Refresh State List");
        text = "Reload available API-MKC states from the database.";
        _apiMkcStateRefreshButton.setToolTipText(text);
        _apiMkcStateRefreshButton.setPreferredSize(buttonDimension);

        // SAC-SMA model state comboBox related controls
        _sacStartingStateComboBox = GuiHelper.getJComboBox();
        initSacStateComboBox(_sacStartingStateComboBox);
        _sacStateListRefreshButton = GuiHelper.getJButton("Refresh State List");
        text = "Reload available SAC-SMA states from the database.";
        _sacStateListRefreshButton.setToolTipText(text);
        _sacStateListRefreshButton.setPreferredSize(buttonDimension);

        // JSeparator separator1 = new JSeparator();

        // The forecast length spinner
        // set up the spinner control for time

        _forecastHoursSpinnerLabel = new JLabel("Fcst Hours:");

        int initialValue = _streamModel.getForecastHours();
        int min = 1;
        int max = _streamModel.getMaxForecastLengthInHours();
        int step = FORECAST_LENGTH_SPINNER_STEPS;

        SpinnerModel forecastHoursSpinnerModel = new SpinnerNumberModel(
                initialValue, min, max, step);

        _forecastHoursSpinner = new JSpinner(forecastHoursSpinnerModel);

        JPanel spinnerPanel = new JPanel(); // used to get the controls to stick
                                            // together nicely
        spinnerPanel.add(_forecastHoursSpinnerLabel);
        spinnerPanel.add(_forecastHoursSpinner);

        // ------- The forecast adjustment controls

        // Forecast Adjustment ON/OFF controls
        _adjustmentCheckBoxLabel = new JLabel("Adjust by Observed");
        _adjustmentCheckBox = new JCheckBox();
        _adjustmentCheckBox.setSelected(_streamModel.isAdjustmentOn());

        JPanel adjustmentCheckBoxPanel = new JPanel();
        adjustmentCheckBoxPanel.add(_adjustmentCheckBoxLabel);
        adjustmentCheckBoxPanel.add(_adjustmentCheckBox);

        // Blending period hours Spinner
        _blendPeriodSpinnerLabel = new JLabel("Blend Ahead Hours");
        String toolTipText = "Note: Zero hours blended can still result in 1 \"extra\" non-blended adjustment (by pairing).";
        _blendPeriodSpinnerLabel.setToolTipText(toolTipText);

        int initialAdjustmentHoursValue = _streamModel.getBlendingHours();

        // Initialize spinner to select the blending hours
        min = 0;
        max = _streamModel.getMaxBlendingHours();
        step = 6;

        if (initialAdjustmentHoursValue < 0) {
            initialAdjustmentHoursValue = 0;
        }
        if (initialAdjustmentHoursValue > max) {
            max = initialAdjustmentHoursValue;
        }

        SpinnerModel blendSpinnerModel = new SpinnerNumberModel(
                initialAdjustmentHoursValue, min, max, step);
        _blendPeriodSpinner = new JSpinner(blendSpinnerModel);
        _blendPeriodSpinner
                .setToolTipText("The number of hours after the last observed input time to blend.\n"
                        + "Zero hours will blend the last observed input hour if there is a match with the forecast.");

        JPanel adjustmentSpinnerPanel = new JPanel();
        adjustmentSpinnerPanel.add(_blendPeriodSpinnerLabel);
        adjustmentSpinnerPanel.add(_blendPeriodSpinner);

        // init the DateTimeTextField (our custom class)

        toolTipText = "Last Observed Input Time For Adjustment. Click on Text Box to edit.";
        _adjustmentLastObsInputTimeLabel = new JLabel(
                "Last Obs. Time For Adjust");
        _adjustmentLastObsInputTimeLabel.setToolTipText(toolTipText);

        _adjustmentLastObsInputTimeTextField = new DateTimeTextField(
                getLastObservedStageTime(), AnalysisWindow.this,
                "Last Observed Input Time For Adjustment", 20);

        _adjustmentLastObsInputTimeTextField.setPreferredSize(new Dimension(
                100, 30));

        // JSeparator separator2 = new JSeparator();

        JPanel modelTypePanel = new JPanel();

        modelTypePanel.add(_modelTypeComboBoxLabel);
        modelTypePanel.add(_modelTypeComboBox);
        modelTypePanel.add(_uhgComboBoxLabel);
        modelTypePanel.add(_uhgComboBox);

        modelTypePanel.setPreferredSize(new Dimension(100, 40));
        // _modelTypeComboBox.setPreferredSize(new Dimension(100, 40));
        // _modelTypeComboBoxLabel.setPreferredSize(new Dimension(50, 40));

        // col, row numCols numRows Wcol wrow
        addComponent(panel, _modelControlPanelLabel, gbc, 0, 0, 1, 1, 1, 0);

        // addComponent(panel, _modelTypeComboBoxLabel, gbc, 0, 1, 1, 1, 0, 0);
        // addComponent(panel, _modelTypeComboBox, gbc, 1, 1, 2, 1, 0, 0);
        addComponent(panel, modelTypePanel, gbc, 0, 1, 4, 1, 0, 0);

        // addComponent(panel, _uhgComboBoxLabel, gbc, 3, 1, 1, 1, 0, 0);
        // addComponent(panel, _uhgComboBox, gbc, 4, 1, 1, 1, 0, 0);

        // API_MKC controls

        addComponent(panel, _apiMkcStartingStateComboBox, gbc, 0, 2, 2, 1, 0, 0);
        addComponent(panel, _apiMkcStateRefreshButton, gbc, 0, 3, 1, 1, 0, 0);

        // /SAC-SMA controls
        addComponent(panel, _sacStartingStateComboBox, gbc, 0, 2, 2, 1, 0, 0);
        addComponent(panel, _sacStateListRefreshButton, gbc, 0, 3, 1, 1, 0, 0);

        addComponent(panel, spinnerPanel, gbc, 1, 3, 1, 1, 0, 0);

        // addComponent(panel, _modelStateLabel, gbc, 0, 3, 1, 1, 0, 0);
        // one or the other will be visible

        JSeparator separator = new JSeparator(JSeparator.HORIZONTAL);
        addComponent(panel, separator, gbc, 0, 4, 2, 1, 0, 0);

        // adjustment controls
        addComponent(panel, adjustmentCheckBoxPanel, gbc, 0, 5, 1, 1, 0, 0);

        addComponent(panel, adjustmentSpinnerPanel, gbc, 1, 5, 1, 1, 0, 0);

        addComponent(panel, _adjustmentLastObsInputTimeLabel, gbc, 0, 6, 1, 1,
                0, 0);
        addComponent(panel, _adjustmentLastObsInputTimeTextField, gbc, 1, 6, 1,
                1, 0, 0);

        return;

    }

    // ----------------------------------------------------------------------------------
    private void initSacControlPanel() {
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

        _sacControlPanelLabel = new JLabel(getCenteredText("SAC-SMA Settings:"));

        _sacParamsButton = new JButton("Edit Params...");
        _sacParamsButton.setToolTipText("Open the SAC-SMA Parameters Editor");

        _sacStateButton = new JButton("Edit States....");
        _sacStateButton.setToolTipText("Open the SAC-SMA State Editor");

        _sacMonthlyMapeValuesEditorButton = new JButton(
                "MAPE Monthly Values...");
        _sacMonthlyMapeValuesEditorButton
                .setToolTipText("Open the MAPE Monthly Values Editor");

        // init the (probably temporary) Setting adjustment button.

        _sacLiveStateAdjustmentButton = new JButton("Adjust State...");
        _sacLiveStateAdjustmentButton.setPreferredSize(new Dimension(50, 30));

        // panel to take up space

        // Note: setMinimumSize() does not work for this purpose, use
        // setPreferredSize() instead

        JPanel verticalSpaceFiller1 = new JPanel();
        verticalSpaceFiller1.setPreferredSize(new Dimension(50, 50));
        JPanel verticalSpaceFiller2 = new JPanel();
        verticalSpaceFiller2.setPreferredSize(new Dimension(85, 150));
        JPanel horizontalSpaceFiller1 = new JPanel();
        horizontalSpaceFiller1.setPreferredSize(new Dimension(85, 100));

        JPanel verticalSpaceFiller3 = new JPanel();
        verticalSpaceFiller2.setPreferredSize(new Dimension(85, 150));

        // just to make things look pretty
        // gbc.insets = new Insets(5, 5, 5, 5);

        // col, row numCols numRows Wcol wrow
        addComponent(panel, _sacControlPanelLabel, gbc, 0, 0, 1, 1, 1, 0);

        if (!_allowLiveSacAdjustment) {
            addComponent(panel, verticalSpaceFiller1, gbc, 0, 1, 1, 2, 1, 0);
            addComponent(panel, horizontalSpaceFiller1, gbc, 1, 1, 1, 2, 1, 1);
        }

        addComponent(panel, _sacParamsButton, gbc, 0, 5, 1, 1, 1, 0);

        if (!_allowLiveSacAdjustment) {
            addComponent(panel, verticalSpaceFiller2, gbc, 0, 6, 1, 1, 1, 1);
        }

        addComponent(panel, _sacStateButton, gbc, 0, 7, 1, 1, 1, 0);

        if (!_allowLiveSacAdjustment) {
            addComponent(panel, verticalSpaceFiller3, gbc, 0, 8, 1, 1, 1, 1);
        }

        addComponent(panel, _sacMonthlyMapeValuesEditorButton, gbc, 0, 11, 1,
                1, 1, 0);

        if (_allowLiveSacAdjustment) {
            gbc.insets = new Insets(5, 5, 5, 5);
            addComponent(panel, _sacLiveStateAdjustmentButton, gbc, 0, 12, 1,
                    1, 1, 0);
        }

    } // end initSacButtonPanel()

    // ------------------------------------------------------------
    // ------------------------------------------------------------

    private void initModelChoice() {
        // trace();

        String header = "AnalysisWindow.initModelChoice(): ";
        int initialIndex = 0;
        String initialSelectedTypeName = _streamModel
                .getRainfallRunoffModelTypeName();

        // System.out.println(header + "initialSelectedTypeName = " +
        // initialSelectedTypeName);

        RainfallRunoffModelType[] modelTypeArray = RainfallRunoffModelType
                .getModelTypeArray();

        // find initialSelectedType
        for (int i = 0; i < modelTypeArray.length; i++) {
            if (modelTypeArray[i].getName().equals(initialSelectedTypeName)) {
                initialIndex = i;
                break;
            }
        }

        _modelTypeComboBox.setSelectedIndex(initialIndex);

        return;
    }

    // ------------------------------------------------------------

    private void initModelComboBox() {
        // note: at some point might want to separate the presentation of the
        // name of the model from the built-in model type name

        RainfallRunoffModelType[] modelTypeArray = RainfallRunoffModelType
                .getModelTypeArray();

        _modelTypeNameArray = new String[modelTypeArray.length];

        // add the choices to the string array
        for (int i = 0; i < modelTypeArray.length; i++) {
            _modelTypeNameArray[i] = modelTypeArray[i].getName();
        }

        // create the comboBox with all the choices in it
        _modelTypeComboBox = GuiHelper.getJComboBox(_modelTypeNameArray);

        return;
    }

    // ------------------------------------------------------------
    private void initUHGChoice() {
        // trace();

        String header = "AnalysisWindow.initUHGChoice(): ";
        int initialIndex = 0;
        String initialSelectedTypeName = _streamModel
                .getRainfallRunoffModelTypeName();

        // System.out.println(header + "initialSelectedTypeName = " +
        // initialSelectedTypeName);

        RainfallRunoffModelType[] modelTypeArray = RainfallRunoffModelType
                .getModelTypeArray();

        // find initialSelectedType
        for (int i = 0; i < modelTypeArray.length; i++) {
            if (modelTypeArray[i].getName().equals(initialSelectedTypeName)) {
                initialIndex = i;
                break;
            }
        }

        _uhgComboBox.setSelectedIndex(initialIndex);

        return;
    }

    // -----------------------------------------------------------------

    private void initUHGComboBox() {

        String[] uhgNamesArray = _streamModel.getUnitHydrographNameArray();

        // create the comboBox with all the choices in it
        _uhgComboBox = GuiHelper.getJComboBox(uhgNamesArray);

        refreshUhgSelection();

        return;
    }

    // -----------------------------------------------------------------
    private void refreshUhgSelection() {
        String selectedUHGModelName = _streamModel
                .getUnitHydrographSelectionName();

        _uhgComboBox.setSelectedItem(selectedUHGModelName);

    }

    // -----------------------------------------------------------------

    private void addComponent(Container container, Component component,
            GridBagConstraints gbc, int column, int row, int columnCells,
            int rowCells, int weightX, int weightY) {

        // how much it can grow in the X and Y directions
        gbc.weightx = weightX;
        gbc.weighty = weightY;

        // what row and column it starts in
        gbc.gridx = column;
        gbc.gridy = row;

        // the number of columns and rows it takes up
        gbc.gridwidth = columnCells;
        gbc.gridheight = rowCells;

        container.add(component, gbc);

        return;
    }

    // -----------------------------------------------------------------

    private void initTitle() {
        String locationId = _streamModel.getLocationId();
        String locationName = _streamModel.getLocationName();
        String basinId = _streamModel.getBasinId();

        String rainfallRunoffModelName = _streamModel
                .getRainfallRunoffModelTypeName();

        String titleString = "SSHP Analysis Window - Modeling " + locationId
                + ", " + locationName + " With BASIN : " + basinId
                + " using the " + rainfallRunoffModelName + " model";
        setTitle(titleString);

        return;
    }

    // -----------------------------------------------------------------
    private void initPrecipCanvas(int x, int y, int width, int height,
            TimeHolder modelStartTimeHolder) {
        // _precipCanvas = new PrecipCanvas(x, y, width, height);
        _precipCanvas = new TsPaintableCanvas(MeasuringUnit.inches, x, y,
                width, height);

        _precipCanvas.setPreferredSize(new Dimension(width, height));
        _precipCanvas.setMinimumSize(new Dimension(100, 10));

        MeasuringUnit unit = _precipCanvas.getDisplayedMeasuringUnit();

        Color outlineColor = Color.WHITE;

        // background painter
        _precipBackgroundPainter = new TsBackgroundPainter(outlineColor,
                _precipCanvas, "inches", "mm");

        _precipValueMapper = new UnitValueMapper(MeasuringUnit.inches,
                MeasuringUnit.mm);

        _precipBackgroundPainter.setRightLabelValueMapper(_precipValueMapper);
        _precipCanvas.addCanvasPainter(_precipBackgroundPainter);

        // precip bar painters ------------------------------

        // add the precip painter
        Color barColor = Color.BLUE;
        _precipPainter = new PrecipPainter(barColor, outlineColor, _streamModel
                .getPrecipTimeSeriesHolder(), _showPrecipAmounts, _precipCanvas);
        _precipCanvas.addTsCanvasPainter(_precipPainter);

        // add the precip text total painter
        _precipTextTotalPainter = new PrecipTotalTextPainter(Color.yellow,
                _streamModel.getPrecipTimeSeriesHolder(), _showPrecipAmounts,
                _precipCanvas);
        _precipCanvas.addCanvasPainter(_precipTextTotalPainter);

        // add the prior runoff painter
        Color runoffColor = Color.YELLOW;
        PrecipPainter priorRunoffPrecipPainter = new PrecipPainter(runoffColor,
                outlineColor, _streamModel.getPriorRunoffTimeSeriesHolder(),
                false, _precipCanvas);

        priorRunoffPrecipPainter.setShouldDrawBars(false);

        _precipCanvas.addTsCanvasPainter(priorRunoffPrecipPainter);

        // add the runoff painter
        Color runoffColor2 = Color.MAGENTA;
        PrecipPainter runoffPrecipPainter = new PrecipPainter(runoffColor2,
                outlineColor, _streamModel.getRunoffTimeSeriesHolder(), false,
                _precipCanvas);

        runoffPrecipPainter.setShouldDrawBars(false);

        _precipCanvas.addTsCanvasPainter(runoffPrecipPainter);

        // Model time line color
        Color timeLineColor = Color.MAGENTA;
        TimeLinePainter modelStartTimeLinePainter = new TimeLinePainter(
                timeLineColor, _modelStartTimeHolder, _precipCanvas);

        _precipCanvas.addCanvasPainter(modelStartTimeLinePainter);

        // set the display time window
        setTimeWindowAroundModelStartTime(_precipCanvas);

        _precipCanvas.setTitle("1-Hour Mean Areal Precipitation Time Series");

    } // end initPrecipCanvas

    // -----------------------------------------------------------------
    private void initStageCanvas(int x, int y, int width, int height,
            TimeHolder modelStartTimeHolder) {
        _stageCanvas = new TsPaintableCanvas(MeasuringUnit.feet, x, y, width,
                height);

        _stageCanvas.setPreferredSize(new Dimension(width, height));
        _stageCanvas.setMinimumSize(new Dimension(100, 10));

        Color outlineColor = Color.WHITE;

        // background Painter
        _stageBackgroundPainter = new TsBackgroundPainter(outlineColor,
                _stageCanvas, "feet", "cfs");

        _stageValueMapper = _streamModel.getStageToFlowValueMapper();

        _stageBackgroundPainter.setRightLabelValueMapper(_stageValueMapper);
        _stageCanvas.addCanvasPainter(_stageBackgroundPainter);

        setUpSigRiverLevelDrawing();

        // observed hydrograph painter
        Color obsColor = Color.yellow;
        _observedStageTsPainter = new StagePainter(obsColor, _streamModel
                .getObservedStageTimeSeriesHolder(), _stageCanvas);
        _stageCanvas.addTsCanvasPainter(_observedStageTsPainter);

        // simulated hydrograph painter
        Color simulationColor = Color.blue;
        _simulatedStageTsPainter = new StagePainter(simulationColor,
                _streamModel.getSimulationStageTimeSeriesHolder(),
                _stageCanvas, true);
        _simulatedStageTsPainter.setPointString("X");
        _stageCanvas.addTsCanvasPainter(_simulatedStageTsPainter);
        // _simulatedStageTsPainter.setShiftAmount(new Point(-4, 3));

        // forecast hydrograph painter
        Color forecastColor = Color.green;
        StagePainter forecastPainter = new StagePainter(forecastColor,
                _streamModel.getForecastStageTimeSeriesHolder(), _stageCanvas,
                true);
        forecastPainter.setPointString("X");

        _stageCanvas.addTsCanvasPainter(forecastPainter);

        // prior forecast hydrograph painter
        // Color priorForecastColor = new Color(154, 205, 50); //yellow green
        Color priorForecastColor = new Color(160, 32, 240); // purple
        StagePainter priorForecastPainter = new StagePainter(
                priorForecastColor, _streamModel
                        .getPriorForecastStageTimeSeriesHolder(), _stageCanvas,
                true);
        priorForecastPainter.setPointString("X");
        _stageCanvas.addTsCanvasPainter(priorForecastPainter);

        // Model time line color
        Color timeLineColor = Color.MAGENTA;
        TimeLinePainter modelStartTimeLinePainter = new TimeLinePainter(
                timeLineColor, _modelStartTimeHolder, _stageCanvas);

        _stageCanvas.addCanvasPainter(modelStartTimeLinePainter);

        // int intervalInHours = 1;

        long startTime = modelStartTimeHolder.getTime() - (24 * _millisPerHour);
        int hoursForward = 72;
        long endTime = startTime + (hoursForward * _millisPerHour);

        _stageCanvas.setTimeWindow(startTime, endTime);

        // set the display time window
        setTimeWindowAroundModelStartTime(_stageCanvas);

        _stageCanvas.setTitle("Forecast Stage Time Series");

    } // end initStageCanvas
    // -----------------------------------------------------------------

    private void setUpSigRiverLevelDrawing() {
        // set up painters for significant stages
        SigRiverLevels sigStages = _streamModel.getSigRiverLevels();

        // action stage painter
        if ((sigStages != null) && (sigStages.hasActionStage()))

        {
            Color actionStageColor = Color.yellow;
            Measurement actionStageMeasurement = new Measurement(sigStages
                    .getActionStage(), MeasuringUnit.feet);
            SigStageLinePainter actionStageLinePainter = new SigStageLinePainter(
                    actionStageColor, actionStageMeasurement, _stageCanvas);

            _stageCanvas.addTsCanvasPainter(actionStageLinePainter);
        }

        // flood stage painter
        if ((sigStages != null) && (sigStages.hasFloodStage())) {
            // this is a darker orange, more easily distinguished
            // from Color.yellow than Color.orange
            Color betterOrange = new Color(0xFF8C00);
            Color floodStageColor = betterOrange;
            Measurement floodStageMeasurement = new Measurement(sigStages
                    .getFloodStage(), MeasuringUnit.feet);
            _floodStageLinePainter = new SigStageLinePainter(floodStageColor,
                    floodStageMeasurement, _stageCanvas);

            _stageCanvas.addTsCanvasPainter(_floodStageLinePainter);
        }

        if ((sigStages != null) && (sigStages.hasModerateFloodStage())) {
            Color moderateFloodStageColor = Color.red;
            Measurement moderateFloodStageMeasurement = new Measurement(
                    sigStages.getModerateFloodStage(), MeasuringUnit.feet);

            SigStageLinePainter moderateFloodStageLinePainter = new SigStageLinePainter(
                    moderateFloodStageColor, moderateFloodStageMeasurement,
                    _stageCanvas);

            _stageCanvas.addTsCanvasPainter(moderateFloodStageLinePainter);
        }

        if ((sigStages != null) && (sigStages.hasMajorFloodStage())) {
            Color majorFloodStageColor = Color.magenta;
            Measurement majorFloodStageMeasurement = new Measurement(sigStages
                    .getMajorFloodStage(), MeasuringUnit.feet);

            _majorFloodStageLinePainter = new SigStageLinePainter(
                    majorFloodStageColor, majorFloodStageMeasurement,
                    _stageCanvas);

            _stageCanvas.addTsCanvasPainter(_majorFloodStageLinePainter);
        }

    }

    // -----------------------------------------------------------------
    private void recenterTimeWindowsAroundModelStartTime() {
        setTimeWindowAroundModelStartTime(_precipCanvas);
        setTimeWindowAroundModelStartTime(_stageCanvas);
    }

    // -----------------------------------------------------------------

    private void setTimeWindowAroundModelStartTime(TsPaintableCanvas canvas) {
        int hoursBack = _hoursToShowBeforeModelStartTime;
        int hoursForward = _hoursToShowAfterModelStartTime;

        long startTime = _modelStartTimeHolder.getTime()
                - (hoursBack * _millisPerHour);

        long endTime = startTime + (hoursForward * _millisPerHour);
        canvas.setTimeWindow(startTime, endTime);

    }

    // -----------------------------------------------------------------

    public void updateDataPointText(DataPoint dataPoint,
            ValueMapper valueMapper, TsPaintableCanvas canvas) {
        // System.out.println("in TsMainWindow.update(DataPoint dp)");

        String leftUnitString = canvas.getDisplayedMeasuringUnit().toString();
        String rightUnitString = valueMapper.getOutputMeasuringUnit()
                .toString();

        String message = getDataPointString(dataPoint, valueMapper,
                leftUnitString, rightUnitString);

        // textField.setText(message);

        canvas.setToolTipText(message);

        return;
    }

    // -----------------------------------------------------------------
    private void setComponentVisibility() {
        RainfallRunoffModelType modelType = _streamModel
                .getRainfallRunoffModelType();

        boolean apiMkcVisible = false;
        boolean sacSmaVisible = false;

        if (modelType == RainfallRunoffModelType.API_MKC) {
            apiMkcVisible = true;
            sacSmaVisible = false;
        } else if (modelType == RainfallRunoffModelType.SAC_SMA) {
            apiMkcVisible = false;
            sacSmaVisible = true;
        }

        _apiMkcControlPanel.setVisible(apiMkcVisible);
        _apiMkcStartingStateComboBox.setVisible(apiMkcVisible);
        // _modelStateLabel.setVisible(apiMkcVisible);
        _apiMkcStateRefreshButton.setVisible(apiMkcVisible);

        _sacControlPanel.setVisible(sacSmaVisible);
        _sacStartingStateComboBox.setVisible(sacSmaVisible);
        _sacStateListRefreshButton.setVisible(sacSmaVisible);

        return;
    }

    // ----------------------------------------------------------------------
    private String getDateTimeStringToMinutes(long time) {

        SimpleDateFormat sdf = new SimpleDateFormat("yyyy/MM/dd HH:mm");
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        String timeString = sdf.format(new Date(time)) + " Z";

        return timeString;
    }

    // ----------------------------------------------------------------------
    private long getTimeOfLatestHour() {
        long time = System.currentTimeMillis();

        time = TimeHelper.truncateTimeInMillisToNearestHour(time, 1);

        return time;
    }

    // ---------------------------------------------------------------------
    private long getLastObservedStageTime() {
        long time = -1;

        try {
            time = _streamModel.getObservedStageTimeSeriesHolder()
                    .getTimeSeries().getEndTime();
        } catch (Throwable t) {
            System.out.println("getLastObservedStageTime(): "
                    + "caught a throwable, but don't worry");
        }

        return time;
    }

    // ------------------------------------------------------------------------

    // //----------------------------------------------------------------------

    private String getDataPointString(DataPoint dataPoint,
            ValueMapper valueMapper, String leftUnitString,
            String rightUnitString) {
        NumberFormat nf = new DecimalFormat("####.##");
        String valueString = nf.format(dataPoint.getY());

        String mappedValueString = "";

        if (valueMapper != null) {
            MeasuringUnit inputUnit = valueMapper.getInputMeasuringUnit();
            Measurement inputMeasurement = new Measurement(dataPoint.getY(),
                    inputUnit);

            Measurement mappedMeasurement = valueMapper
                    .getResultMeasurement(inputMeasurement);

            double mappedValue = mappedMeasurement.getValue();

            mappedValueString = nf.format(mappedValue);
        }

        long time = (long) Math.floor(dataPoint.getX());

        String timeString = getDateTimeStringToMinutes(time);

        String dataPointString = timeString + ", " + valueString + " "
                + leftUnitString + ", " + mappedValueString + " "
                + rightUnitString;

        return dataPointString;
    }

    // event handler --------------------------------------------------

    // ----------------------------------------------------------------------

    private void replaceTimeSeriesValue(
            RegularTimeSeriesHolder timeSeriesHolder,
            TimeIntervalMeasurement newMeasurement) {
        // String header = "AnalysisWindow.replaceTimeSeriesValue(): ";

        RegularTimeSeries timeSeries = timeSeriesHolder.getTimeSeries();

        // System.out.println(header + "before conversion, newMeasurement is " +
        // newMeasurement);

        newMeasurement = TimeIntervalMeasurement.getConvertedCopy(
                newMeasurement, timeSeries.getMeasuringUnit());

        // System.out.println(header + "after conversion, newMeasurement is " +
        // newMeasurement);

        long endTimeInMillis = newMeasurement.getEndTime();

        timeSeries.setMeasurementByTime(newMeasurement, endTimeInMillis);

    } // end replaceTimeSeriesValue()

    // ----------------------------------------------------------------------

    private void replaceTimeSeriesValue(
            RegularTimeSeriesHolder timeSeriesHolder,
            AbsTimeMeasurement newMeasurement) {
        String header = "AnalysisWindow.replaceTimeSeriesValue(): ";

        RegularTimeSeries timeSeries = timeSeriesHolder.getTimeSeries();

        // System.out.println(header + "timeSeries = " + timeSeries);

        if (timeSeries != null) {
            newMeasurement = AbsTimeMeasurement.getConvertedCopy(
                    newMeasurement, timeSeries.getMeasuringUnit());

            timeSeries.setMeasurementByTime(newMeasurement, newMeasurement
                    .getTime());
        } else {
            // System.err.println(header + " time series is null ");
        }

    } // end replaceTimeSeriesValue()

    // ----------------------------------------------------------------
    private void runModel() {

        // String header = "AnalysisWindow.runModel(): ";
        // System.out.println(header + "inside the method");

        _streamModel.runModel();

        redrawCanvases();

        announceModelRun();

    }

    // ----------------------------------------------------------------

    private void announceModelRun() {
        announceTimeSeriesChange("Model Run");
    }

    // -----------------------------------------------------------------

    private void announceForecastStageEdited() {
        String message = "Manually Edited Fcst Stage";
        TimeSeriesEvent event = new TimeSeriesEvent(message,
                AnalysisWindow.this);

        _streamModel.getForecastStageTimeSeriesHolder().forwardEvent(event);
    }

    // -----------------------------------------------------------------
    private void announceTimeSeriesChange(String message) {
        TimeSeriesEvent event = new TimeSeriesEvent(message,
                AnalysisWindow.this);

        _streamModel.getPrecipTimeSeriesHolder().forwardEvent(event);
        _streamModel.getForecastStageTimeSeriesHolder().forwardEvent(event);

        _streamModel.getEvaporationTimeSeriesHolder().forwardEvent(event);
        _streamModel.getPriorRunoffTimeSeriesHolder().forwardEvent(event);

        _streamModel.getRunoffTimeSeriesHolder().forwardEvent(event);

    }

    // -----------------------------------------------------------------

    private void setModelTypeToComboBoxSelection() {

        int selectedIndex = _modelTypeComboBox.getSelectedIndex();

        RainfallRunoffModelType[] typeArray = RainfallRunoffModelType
                .getModelTypeArray();

        RainfallRunoffModelType type = typeArray[selectedIndex];

        changeModelType(type);
    }

    // -----------------------------------------------------------------

    private void changeModelType(RainfallRunoffModelType newType) {

        RainfallRunoffModelType oldType = _streamModel
                .getRainfallRunoffModelType();

        String header = "AnalysisWindow.changeModelType()";
        // System.out.println(header +": changing type to " + newType);

        // _tracer.trace("in " + header);

        boolean success = _streamModel.setRainfallRunoffModelType(newType);

        if (success) {

            if ((oldType != null) && (oldType == newType)) {
                // System.out.println(header +
                // "changing to same RR model type");
            }

            setComponentVisibility();

            // the title needs to update because the
            // model may have changed
            initTitle();

            // take any already graphically set parameters and put them back
            // into the
            // streamModel to be used for model runs
            updateModelSettingsFromGui();

            if (newType == RainfallRunoffModelType.API_MKC) {
                setApiMkcModelState();
            } else if (newType == RainfallRunoffModelType.SAC_SMA) {
                setSacModelState();
            }

            // recenter the canvases around the model run start time
            recenterTimeWindowsAroundModelStartTime();

            // refresh the selected Unit HydroGraph
            refreshUhgSelection();

            // run the stream model again
            // runModel();
        } else // failed, so change it back to MKC_API
        {

            String locationId = _streamModel.getLocationId();

            String message = "Location: "
                    + locationId
                    + " is not configured properly to run the SAC-SMA.\n"
                    + " Please add a record in the SSHPConfig Dialog to configure it.";

            String dialogTitle = locationId + " Configuration Message";
            DialogHelper.displayMessageDialog(this, message, dialogTitle);

            int index = getIndexOfModelType(RainfallRunoffModelType.API_MKC);
            _modelTypeComboBox.setSelectedIndex(index);

        }
        return;
    }

    // -----------------------------------------------------------------
    private int getIndexOfModelType(RainfallRunoffModelType type) {
        int index = -1;

        for (int i = 0; i < _modelTypeNameArray.length; i++) {
            if (type.getName().equalsIgnoreCase(_modelTypeNameArray[i])) {
                index = i;
            }
        }

        return index;
    }

    // -----------------------------------------------------------------

    private void setUnitHydrographToComboBoxSelection() {

        Object selectedItem = _uhgComboBox.getSelectedItem();

        String selectedUnitHydrographName = (String) selectedItem;

        _streamModel.changeUnitHydrograph(selectedUnitHydrographName);

        runModel();
    }

    // -----------------------------------------------------------------

    private void updateModelSettingsFromGui() {
        RainfallRunoffModelType modelType = _streamModel
                .getRainfallRunoffModelType();

        if (modelType == RainfallRunoffModelType.API_MKC) {
            seInitialStageMeasurementFromGui();
            setFfhFromGui();
            setThresholdRunoffFromGui();
        } else if (modelType == RainfallRunoffModelType.SAC_SMA) {
            // do nothing
        }

        return;
    }

    // -----------------------------------------------------------------

    // add all of the event handlers
    private void addListeners() {

        // allow the frame to close when the user presses the close-box
        addWindowListener(new FrameCloseWindowListener());

        // addComponentListener(new WindowResizeComponentListener());
        // this object does not get garbage collected right away, since in the
        // constructor, it adds itself as a Window listener
        WindowResizingManager mgr = new WindowResizingManager(this,
                _minDimension, _maxDimension);

        // set up the action to perform when the close button is pressed
        _closeButton.addActionListener(new CloseListener());

        // setup of the action for the screen capture button
        _screenCaptureButton
                .addActionListener(new ScreenCaptureActionListener());

        // set up the window launcher button
        _windowLauncherButton
                .addActionListener(new ControlWindowFocusActionListener());

        // set up the action to perform when the forward button is pressed
        _forwardButton.addActionListener(new TimeWindowSlideListener(1));

        // set up the action to perform when the forward button is pressed
        _forward1DayButton.addActionListener(new TimeWindowSlideListener(24));

        // setup of the action to perform when the recenter button is pressed
        _recenterButton
                .addActionListener(new TimeWindowRecenterActionListener());

        // set up the action to perform when the back button is pressed
        _back1DayButton.addActionListener(new TimeWindowSlideListener(-24));

        // set up the action to perform when the back button is pressed
        _backButton.addActionListener(new TimeWindowSlideListener(-1));

        // set up the action to perform when the time mode check box is pressed
        // _timeModeCheckBox.addActionListener( new TimeModeCheckBoxListener());

        // set up the action to perform when the show flood stage check box is
        // pressed
        _showFloodStageCheckBox
                .addActionListener(new FloodStageCheckBoxListener());

        // set up the action to perform when the show major flood stage check
        // box is pressed
        _showMajorFloodStageCheckBox
                .addActionListener(new MajorFloodStageCheckBoxListener());

        // set up the action to perform when the _showPrecipAmountsCheckBox is
        // pressed
        _showPrecipAmountsCheckBox
                .addActionListener(new ShowPrecipAmountsCheckBoxListener());

        // set up the action to perform when the show obs stage time series
        // check box is pressed
        _showObsHeightTsCheckBox
                .addActionListener(new ShowObservedHeightTsCheckBoxListener());

        // addListener for the delayRerunCheckBox
        _delayRerunCheckBox.addActionListener(new DelayRerunCheckBoxListener());

        // SHOW MINOR PRECIP TICKS
        _showMinorPrecipTicksCheckBox
                .addActionListener(new ShowMinorPrecipTicksCheckBoxListener());

        // SHOW MINOR STAGE TICKS
        _showMinorStageTicksCheckBox
                .addActionListener(new ShowMinorStageTicksCheckBoxListener());

        // SHOW Simulated Stage
        _showSimulatedStageCheckBox
                .addActionListener(new ShowSimulatedStageCheckBoxListener());

        // CANVAS LISTENERS

        // addlistener for the PrecipCanvas
        _precipCanvas.addMouseMotionListener(new CanvasMouseMovedListener(
                _precipCanvas, _precipValueMapper));

        // addlistener for the StageCanvas
        _stageCanvas.addMouseMotionListener(new CanvasMouseMovedListener(
                _stageCanvas, _stageValueMapper));

        // --- KC API model settings

        // baseflow reset
        _apiMkcInitialStagePanel.getButton().addActionListener(
                new InitialStageResetListener());

        // baseflow adjustment
        _apiMkcInitialStagePanel.getTextField().addFocusListener(
                new InitalStageFocusListener());

        // FFH reset
        _apiMkcFfhPanel.getButton().addActionListener(new FfhResetListener());

        // FFH adjustment
        _apiMkcFfhPanel.getTextField().addFocusListener(new FfhFocusListener());

        _apiMkcStartingStateComboBox
                .addActionListener(new ModelRunStartingStateActionListener(
                        _apiMkcStartingStateComboBox));

        // threshold Runoff reset
        _apiMkcThreshRPanel.getButton().addActionListener(
                new ThreshRunoffResetListener());

        // threshold Runoff adjustment
        _apiMkcThreshRPanel.getTextField().addFocusListener(
                new ThreshRunoffFocusListener());

        _apiMkcUseCustomModelRunTimeCheckBox
                .addActionListener(new UseCustomModelRunStartTimeCheckBoxListener());

        // the apply button for API-MKC
        _apiMkcApplySettingsButton
                .addActionListener(new ApplySettingsActionListener());

        // refresh button for API-MKC state/time list
        _apiMkcStateRefreshButton
                .addActionListener(new RefreshApiMkcStateListActionListener());

        _apiMkcCustomModelRunDateTimeTextField.getDocument()
                .addDocumentListener(
                        new CustomModelRunDateTimeChangedDocumentListener());

        // add listener for Model Change
        _modelTypeComboBox.addActionListener(new ModelChoiceListener());

        // Unithydrograph manual change
        _uhgComboBox.addActionListener(new UnitHydrographChoiceListener());

        // add listener for the Mouse events from TsCanvas
        AdjustPrecipMouseListener precipAdjustmentListener = new AdjustPrecipMouseListener();
        _precipCanvas.addMouseListener(precipAdjustmentListener);
        _precipCanvas.addMouseMotionListener(precipAdjustmentListener);
        _precipCanvas.addKeyListener(precipAdjustmentListener);

        // add listener for the Mouse events from TsCanvas
        // _stageCanvas.addTsCanvasEventListener(new
        // ModelTimeAdjustmentTsCanvasEventListener());
        AdjustStageMouseListener adjustStageListener = new AdjustStageMouseListener();
        _stageCanvas.addMouseListener(adjustStageListener);
        _stageCanvas.addMouseMotionListener(adjustStageListener);

        // SAC_SMA controls

        // add Frame launching button actions
        _sacParamsButton.addActionListener(new SacParamsLaunchActionListener());
        _sacStateButton.addActionListener(new SacStateLaunchActionListener());

        _sacMonthlyMapeValuesEditorButton
                .addActionListener(new MonthlyMapeEditorLaunchActionListener());

        _sacStartingStateComboBox
                .addActionListener(new ModelRunStartingStateActionListener(
                        _sacStartingStateComboBox));

        // model controls
        // add listener for setting the length of forecast
        _forecastHoursSpinner
                .addChangeListener(new ForecastHoursChangeListener());

        _adjustmentCheckBox.addActionListener(new AdjustToggleActionListener());

        _adjustmentLastObsInputTimeTextField.getDocument().addDocumentListener(
                new LastObsTimeForAdjustmentChangedDocumentListener());

        _blendPeriodSpinner
                .addChangeListener(new AdjustmentHoursChangeListener());

        // launches the LiveSacState editor dialog (the one with sliders)
        _sacLiveStateAdjustmentButton
                .addActionListener(new LiveSacStateLaunchActionListener());

        // refresh button for SAC_SMA state/time list
        _sacStateListRefreshButton
                .addActionListener(new RefreshSacSmaStateListActionListener());

        // create listeners for the time series that can be editted
        _streamModel.getEvaporationTimeSeriesHolder().addListener(
                new EvaporationTimeSeriesChangeListener());
        _streamModel.getPrecipTimeSeriesHolder().addListener(
                new PrecipTimeSeriesChangeListener());
        _streamModel.getForecastStageTimeSeriesHolder().addListener(
                new ForecastHeightTimeSeriesChangeListener());

    } // end addListeners

    // -------used by inner classes
    // --------------------------------------------------------------------
    private void usePrecipFileChooserForReading() {
        String defaultDirectory = null;

        if (_precipFileChooserHelper == null) {
            String[] extensions = { "work" };
            _precipFileChooserHelper = new FileChooserHelper(
                    _sshp_precip_directoryString, extensions,
                    "Precip Work files .work");
        }

        String filePath = _precipFileChooserHelper
                .displayForOpen(AnalysisWindow.this);

        if (filePath != null) {
            _streamModel.reloadFilePrecipTimeSeries(filePath);
            runModel();
        }

        return;
    }

    // -----------------------------------------------------------------------------------
    private void usePrecipFileChooserForSaving() {
        String defaultDirectory = null;

        if (_precipFileChooserHelper == null) {
            String[] extensions = { "work" };
            _precipFileChooserHelper = new FileChooserHelper(
                    _sshp_precip_directoryString, extensions,
                    "Precip Work files .work");
        }

        String filePath = _precipFileChooserHelper
                .displayForSave(AnalysisWindow.this);

        if (filePath != null) {
            _streamModel.savePrecipTimeSeriesToFile(filePath);
        }

        return;
    }

    // -----------------------------------------------------------------------------------
    private void useCaptureScreenFileChooserForSaving() {
        BufferedImage image = captureScreenToImage();

        String defaultDirectory = null;

        if (_imageFileChooserHelper == null) {
            String[] extensions = { "jpg", "JPEG", "JPG" };
            _imageFileChooserHelper = new FileChooserHelper(
                    _whfs_image_directoryString, extensions,
                    "JPEG files .jpg .JPEG .JPG ");
        }

        String filePath = _imageFileChooserHelper
                .displayForSave(AnalysisWindow.this);

        if (filePath != null) {
            writeImageToFile(image, new File(filePath));
        }

        return;
    }

    // --------------------------------------------------------------------------------
    private void displayCaptureScreenFileChooser() {
        BufferedImage image = captureScreenToImage();

        if (_imageFileChooser == null) {
            _imageFileChooser = new JFileChooser(_whfs_image_directoryString);
        }

        // add a FileFilter for JPG files
        List filterList = new ArrayList();

        filterList.add("JPEG");
        filterList.add("JPG");
        FileFilter fileFilter = new ExtensionFileFilter(filterList,
                "JPEG Image Files (*.jpg, *.JPG) ");

        // set the file filter
        _imageFileChooser.setFileFilter(fileFilter);
        // _imageFileChooser.set

        // open the dialog
        int returnVal = _imageFileChooser.showSaveDialog(this);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = _imageFileChooser.getSelectedFile();

            // ensure ends in ".jpg"
            if ((!file.getName().endsWith(".jpg"))
                    && (!file.getName().endsWith(".JPG"))) {
                String newFilePath = file.getPath() + ".jpg";
                file = new File(newFilePath);
            }

            writeImageToFile(image, file);
        }

        else {
            // System.out.println("AnalysisWindow.displayCaptureScreenFileChooser(): ");
            // JOptionPane.showMessageDialog(AnalysisWindow.this,
            // "Image file not saved.");
            DialogHelper.displayMessageDialog(AnalysisWindow.this,
                    "Image file not saved.");

        }

        return;
    }

    // -----------------------------------------------------------------
    private void sleep(int millis) {

        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

    }

    // -----------------------------------------------------------------
    private BufferedImage captureScreenToImage() {
        BufferedImage bufferedImage = null;
        try {

            Robot robot = new Robot();
            int x = AnalysisWindow.this.getX();
            int y = AnalysisWindow.this.getY();
            int width = AnalysisWindow.this.getWidth();
            int height = AnalysisWindow.this.getHeight();
            Rectangle rectangleBounds = new Rectangle(x, y, width, height);

            // System.out.println("bounds rectangle of image capture = " +
            // rectangleBounds);
            bufferedImage = robot.createScreenCapture(rectangleBounds);

        } // end try

        catch (AWTException e) {
            e.printStackTrace();
        }

        return bufferedImage;
    }

    // -----------------------------------------------------------------

    private void writeImageToFile(BufferedImage image, File file) {
        try {
            ImageOutputStream imageOutputStream = new FileImageOutputStream(
                    file);
            ImageIO.write(image, "JPG", imageOutputStream);
            imageOutputStream.close();
        }

        catch (java.io.IOException e) {
            e.printStackTrace();
        }

    }

    // -----------------------------------------------------------------

    private void seInitialStageMeasurementFromGui() {
        String valueString = _apiMkcInitialStagePanel.getTextField().getText();

        setInitalStageMeasurementInModelFromString(valueString);
    }

    // -----------------------------------------------------------------

    private void setInitalStageMeasurementInModelFromString(
            String initialStageString) {
        double value = Double.parseDouble(initialStageString);

        Measurement measurement = new Measurement(value, _stageUnit);

        _streamModel.setInitialStageMeasurement(measurement);

    }

    // -----------------------------------------------------------------

    private void resetInitialStageMeasurementFromModel() {
        // uses a streamModel call to get the Baseflow value, but does not
        // change the
        // value STORED by the streamModel object
        Measurement initialStageMeasurement = _streamModel
                .findInitialStageMeasurement();

        // set the value STORED by the streamModel
        _streamModel.setInitialStageMeasurement(initialStageMeasurement);

        NumberFormat nf = new DecimalFormat("####.##");
        String valueString = nf.format(initialStageMeasurement.getValue());

        _apiMkcInitialStagePanel.getTextField().setText("" + valueString);
    }

    // -----------------------------------------------------------------

    private void setFfhFromGui() {
        String valueString = _apiMkcFfhPanel.getTextField().getText();

        setFfhFromString(valueString);
    }

    // -----------------------------------------------------------------

    private void setFfhFromString(String ffhString) {
        double value = Double.parseDouble(ffhString);
        _streamModel.setFfhValue(value);
    }

    private void resetFfhFromModel() {
        // uses a streamModel call to get the FFH value, but does not change the
        // value STORED by the streamModel object
        double ffh = _streamModel.findFfhValue();

        // set the value STORED by the streamModel
        _streamModel.setFfhValue(ffh);

        NumberFormat nf = new DecimalFormat("####.##");
        String valueString = nf.format(ffh);
        // System.out.println(
        // "AnalysisWindow:resetFfhFromModel(): FFHValue = : " + valueString );
        _apiMkcFfhPanel.getTextField().setText(valueString);
    }

    // -----------------------------------------------------------------
    private void loadModelData() {
        _streamModel.reloadModelData();

        if (_streamModel.getRainfallRunoffModelType() == RainfallRunoffModelType.SAC_SMA) {
            // do nothing
        } else if (_streamModel.getRainfallRunoffModelType() == RainfallRunoffModelType.API_MKC) {
            resetFfhFromModel();
            resetInitialStageMeasurementFromModel();
            resetThresholdRunoffFromModel();
        }
    }

    // -----------------------------------------------------------------
    private void setApiMkcModelState() {

        String header = "AnalysisWindow.setApiMkcModelState():";

        FFHDescriptor descriptorToUse = null;
        MeasuringUnit ffhUnit = MeasuringUnit.inches;

        if (_apiMkcUseCustomModelRunTime) {
            // should use some other method for this
            double value = Double.parseDouble(_apiMkcFfhPanel.getTextField()
                    .getText());

            long time = _apiMkcCustomModelRunDateTimeTextField.getTime();
            AbsTimeMeasurement measurement = new AbsTimeMeasurement(value,
                    time, ffhUnit);
            descriptorToUse = new FFHDescriptor(_streamModel.getLocationId(),
                    measurement, StreamModel.DEFAULT_FFG_DURATION_CODE, false);

        } else // use the standard selection method
        {

            _selectedFFHDescriptor = (FFHDescriptor) _apiMkcStartingStateComboBox
                    .getSelectedItem();

            descriptorToUse = _selectedFFHDescriptor;
        }

        // if there are no products,
        if (descriptorToUse == null) {

            AbsTimeMeasurement fakeFfmMeasurement = new AbsTimeMeasurement(10,
                    getTimeOfLatestHour(), MeasuringUnit.inches);

            String lid = _streamModel.getLocationId();
            descriptorToUse = new FFHDescriptor(lid, fakeFfmMeasurement,
                    StreamModel.DEFAULT_FFG_DURATION_CODE, false);
        }

        selectFFHProduct(descriptorToUse);

        // System.out.println("AnalysisWindow.setApiMkcModelState : descriptor = "
        // +
        // descriptorToUse.toString());

        long newTime = descriptorToUse.getTime();

        setModelRunStartTime(newTime);

        recenterTimeWindowsAroundModelStartTime();

        // runModel();
    }

    // -----------------------------------------------------------------

    private void setSacModelState() {
        String header = "AnalysisWindow.setSacModelState():";

        _selectedSacStateDescriptor = (SacSmaStateDescriptor) _sacStartingStateComboBox
                .getSelectedItem();

        selectSacSmaStateDescriptor(_selectedSacStateDescriptor);

        // make sure that we reload the prior runoff time series, which happens
        // in here

        // System.out.println(" ********* AnalysisWindow.setSacModelState() : descriptor = "
        // +
        // _selectedSacStateDescriptor.toString());

        long newTime = _selectedSacStateDescriptor.getValidTime();
        setModelRunStartTime(newTime);

        recenterTimeWindowsAroundModelStartTime();

        // runModel();
    }

    // -----------------------------------------------------------------
    private void selectFFHProduct(FFHDescriptor descriptor) {
        _streamModel.setFFHProductDescriptor(descriptor);

    }

    // -----------------------------------------------------------------
    private void selectSacSmaStateDescriptor(SacSmaStateDescriptor descriptor) {
        _streamModel.setSacSmaStateDescriptor(descriptor);
    }

    // -----------------------------------------------------------------
    private void setModelRunStartTime(long newStartTime) {

        newStartTime = TimeHelper.truncateTimeInMillisToNearestHour(
                newStartTime, 1);
        _streamModel.setModelStartTime(newStartTime);

        // to make the response time to the clicking faster
        // this will happen eventually, anyway after the model run
        // _precipCanvas.refresh();

        String dateTimeString = getDateTimeStringToMinutes(newStartTime);

        // _modelRunStartTimeValueLabel.setText("Model Run Start Time: " +
        // dateTimeString);
        _modelRunStartTimeValueLabel.setText(dateTimeString);

        // reload the model data, so that the correct parameters and
        // states are used.
        loadModelData();

        runModel();

    }

    // -----------------------------------------------------------------

    private void setThresholdRunoffFromGui() {
        String valueString = _apiMkcThreshRPanel.getTextField().getText();

        setThresholdRunoffFromString(valueString);
    }

    // -----------------------------------------------------------------

    private void setThresholdRunoffFromString(String thresholdRunoffString) {
        double value = Double.parseDouble(thresholdRunoffString);
        _streamModel.setThresholdRunoff(value);
    }

    // -----------------------------------------------------------------

    private void resetThresholdRunoffFromModel() {
        // uses a streamModel call to get the threshold Runoff value,
        // but does not change the
        // value STORED by the streamModel object
        double thresholdRunoff = _streamModel.findThresholdRunoffValue();

        // set the value STORED by the streamModel
        _streamModel.setThresholdRunoff(thresholdRunoff);

        NumberFormat nf = new DecimalFormat("####.##");
        String valueString = nf.format(thresholdRunoff);

        _apiMkcThreshRPanel.getTextField().setText(valueString);
    }

    // -----------------------------------------------------------------------

    private void redrawCanvases() {
        String header = "AnalysisWindow.redrawCanvases()";

        _precipCanvas.refresh();

        _stageCanvas.refresh();
    }

    // -----------------------------------------------------------------------

    private void initMenuBar() {
        _menuBar = new JMenuBar();

        JMenu menu = null;
        JMenuItem menuItem = null;

        // File Menu
        menu = new JMenu("File");
        menu.setMnemonic(KeyEvent.VK_F);
        menu.getAccessibleContext().setAccessibleDescription(
                "Access File Menus");
        _menuBar.add(menu);

        menuItem = new JMenuItem("Close Window");
        menuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C,
                ActionEvent.ALT_MASK));
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

        // Precip editor
        menuItem = new JMenuItem("Precip Editor...");
        menuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P,
                ActionEvent.ALT_MASK));
        menuItem.getAccessibleContext().setAccessibleDescription("");
        menu.add(menuItem);

        menuItem.addActionListener(new PrecipTimeSeriesLaunchActionListener());

        // Forecast Stage Editor
        menuItem = new JMenuItem("Forecast Stage Editor...");
        menuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_I,
                ActionEvent.ALT_MASK));
        menuItem.getAccessibleContext().setAccessibleDescription("");
        menu.add(menuItem);

        menuItem
                .addActionListener(new ForecastHeightTimeSeriesLaunchActionListener());

        // ET Time Series editor
        menuItem = new JMenuItem("Evapotranspiration Editor...");
        menuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_T,
                ActionEvent.ALT_MASK));
        menuItem.getAccessibleContext().setAccessibleDescription("");
        menu.add(menuItem);

        menuItem
                .addActionListener(new EvaporationTimeSeriesLaunchActionListener());

        // Prior Computed Runoff Viewer
        menuItem = new JMenuItem("Prior Runoff Viewer...");
        menuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O,
                ActionEvent.ALT_MASK));
        menuItem.getAccessibleContext().setAccessibleDescription("");
        menu.add(menuItem);

        menuItem
                .addActionListener(new PriorRunoffTimeSeriesLaunchActionListener());

        // Computed Runoff Viewer
        menuItem = new JMenuItem("Runoff Viewer...");
        menuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_R,
                ActionEvent.ALT_MASK));
        menuItem.getAccessibleContext().setAccessibleDescription("");
        menu.add(menuItem);

        menuItem.addActionListener(new RunoffTimeSeriesLaunchActionListener());

        // Save Menu
        menu = new JMenu("Save");
        menu.setMnemonic(KeyEvent.VK_S);
        menu.getAccessibleContext().setAccessibleDescription(
                "Save time series changes to database.");
        _menuBar.add(menu);

        // Save Forecast Stage
        menuItem = new JMenuItem("Save Forecast Stage...");
        menuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_G,
                ActionEvent.ALT_MASK));
        menuItem.getAccessibleContext().setAccessibleDescription("");
        menu.add(menuItem);

        menuItem
                .addActionListener(new ForecastHeightSaveLaunchActionListener());

        // Save Forecast Discharge
        menuItem = new JMenuItem("Save Forecast Discharge Time Series...");
        menuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_D,
                ActionEvent.ALT_MASK));
        menuItem.getAccessibleContext().setAccessibleDescription("");
        menu.add(menuItem);

        menuItem
                .addActionListener(new ForecastDischargeSaveLaunchActionListener());

        // Save Evaporation Time Series
        menuItem = new JMenuItem("Save Evap Time Series..");
        menuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_E,
                ActionEvent.ALT_MASK));
        menuItem.getAccessibleContext().setAccessibleDescription("");
        menu.add(menuItem);

        menuItem.addActionListener(new EvapSaveLaunchActionListener());

        // Save Edited MAP Time Series
        menuItem = new JMenuItem("Save Edited MAP Time Series..");
        menuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_M,
                ActionEvent.ALT_MASK));
        menuItem.getAccessibleContext().setAccessibleDescription("");
        menu.add(menuItem);

        menuItem.addActionListener(new MAPSaveLaunchActionListener());

        // reloadPrecipButton directly on menuBar
        Color menuBarColor = _menuBar.getBackground();
        Border invisibleBorder = BorderFactory.createLineBorder(menuBarColor);

        JButton button = new JButton("   Reload Precip  ");
        button.addActionListener(new ReloadPrecipTimeSeriesActionListener());
        button.setBorder(invisibleBorder);
        button.setSize(60, 30);
        String text = "Reload the MAP time series from the database.\n"
                + "Changes will be lost.";
        button.setToolTipText(text);
        _menuBar.add(button);

        // reloadFilePrecipButton directly on menuBar
        menuBarColor = _menuBar.getBackground();
        invisibleBorder = BorderFactory.createLineBorder(menuBarColor);

        button = new JButton("   Reload File Precip  ");
        button
                .addActionListener(new ReloadFilePrecipTimeSeriesActionListener());
        button.setBorder(invisibleBorder);
        button.setSize(60, 30);
        text = "Reload the MAP time series from a workfile.\n"
                + "Changes will be lost.";
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

        // JSeparator menuSeparator = new JSeparator();
        // menuSeparator.setOrientation(SwingConstants.VERTICAL);
        // _menuBar.add(menuSeparator);

        // acts as a separator to move the Help menu all the way to the right
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
        menuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A,
                ActionEvent.ALT_MASK));
        menuItem.getAccessibleContext().setAccessibleDescription(
                "Display the About Dialog.");
        menu.add(menuItem);

        menuItem.addActionListener(new AboutLaunchListener());

    }

    // -----------------------------------------------------------------------

    // -----inner classes ------------------------------------------------------
    private class AboutLaunchListener implements ActionListener {
        public void actionPerformed(ActionEvent evt) {
            String aboutText = AboutInfo.getText();
            DialogHelper.displayMessageDialog(AnalysisWindow.this, aboutText,
                    "About SSHP");
        }
    }

    // -----------------------------------------------------------------

    private class InitialStageResetListener implements ActionListener {
        public void actionPerformed(ActionEvent evt) {
            resetInitialStageMeasurementFromModel();
            runModel();

        }
    }

    // -----------------------------------------------------------------

    private class InitalStageFocusListener implements FocusListener {

        public InitalStageFocusListener() {

        }

        public void focusGained(FocusEvent event) {

        }

        public void focusLost(FocusEvent event) {
            // System.out.println("InitalStageFocusListener triggered");

            JTextField textField = (JTextField) event.getSource();

            String initialStageString = textField.getText().trim();
            if (initialStageString.length() == 0) {
                // consider putting an error dialog here
            } else {
                try {
                    setInitalStageMeasurementInModelFromString(initialStageString);
                } catch (Throwable t) {
                    // consider putting an error dialog here
                }
            }

            // the apply button does this now
            // runModel();

        } // end focusLost

    }

    // -----------------------------------------------------------------

    private class FfhResetListener implements ActionListener {
        public void actionPerformed(ActionEvent evt) {
            resetFfhFromModel();
            runModel();
        }
    }

    // -----------------------------------------------------------------

    private class ApplySettingsActionListener implements ActionListener {
        public void actionPerformed(ActionEvent evt) {
            // System.out.println("ApplySettingsActionListener triggered");
            updateModelSettingsFromGui();
            // setApiMkcModelState();
            runModel();
        }
    }

    // -----------------------------------------------------------------

    private class RefreshSacSmaStateListActionListener implements
            ActionListener {
        public void actionPerformed(ActionEvent evt) {
            // System.out.println("RefreshSacSmaStateListActionListener triggered");

            initSacStateComboBox(_sacStartingStateComboBox);
        }
    }

    // -----------------------------------------------------------------

    private class RefreshApiMkcStateListActionListener implements
            ActionListener {
        public void actionPerformed(ActionEvent evt) {
            // System.out.println("RefreshApiMkcStateListActionListener triggered");

            initApiMkcStateComboBox(_apiMkcStartingStateComboBox);
        }
    }

    private class ReloadPrecipTimeSeriesActionListener implements
            ActionListener {
        public void actionPerformed(ActionEvent evt) {
            DialogHelper.setWaitCursor(_menuBar);
            _streamModel.reloadPrecipTimeSeries();
            runModel();
            DialogHelper.setDefaultCursor(_menuBar);
        }
    }

    // -----------------------------------------------------------------

    private class ReloadFilePrecipTimeSeriesActionListener implements
            ActionListener {
        public void actionPerformed(ActionEvent evt) {
            DialogHelper.setWaitCursor(_menuBar);

            usePrecipFileChooserForReading();

            DialogHelper.setDefaultCursor(_menuBar);
        }
    }

    // -----------------------------------------------------------------

    private class ReloadObsStageTimeSeriesActionListener implements
            ActionListener {
        public void actionPerformed(ActionEvent evt) {
            DialogHelper.setWaitCursor(_menuBar);

            _streamModel.reloadObservedStageTimeSeries();
            runModel();

            DialogHelper.setDefaultCursor(_menuBar);

        }
    }

    // -----------------------------------------------------------------

    private class FfhFocusListener implements FocusListener {

        public FfhFocusListener() {

        }

        public void focusGained(FocusEvent event) {

        }

        public void focusLost(FocusEvent event) {
            // System.out.println("FfhFocusListener triggered");

            JTextField textField = (JTextField) event.getSource();

            String ffhString = textField.getText().trim();
            if (ffhString.length() == 0) {
                resetFfhFromModel(); // just set it back to the value in the
                                     // streamModel.
            } else {
                try {
                    setFfhFromString(ffhString);

                } catch (Throwable t) {
                    resetFfhFromModel(); // just set it back to the value in the
                                         // streamModel.
                }
            }

            // the apply button does this now
            // runModel();

        } // end focusLost

    }

    // -----------------------------------------------------------------

    private class ThreshRunoffResetListener implements ActionListener {
        public void actionPerformed(ActionEvent evt) {
            resetThresholdRunoffFromModel();
            runModel();

        }
    }

    // -----------------------------------------------------------------

    private class ThreshRunoffFocusListener implements FocusListener {

        public ThreshRunoffFocusListener() {

        }

        public void focusGained(FocusEvent event) {

        }

        public void focusLost(FocusEvent event) {
            // System.out.println("ThreshRunoffFocusListener triggered");

            JTextField textField = (JTextField) event.getSource();

            // System.out.println("AnalysisWindow.ThreshRFocusLost listener activated");

            String thresholdRunoffString = textField.getText().trim();
            if (thresholdRunoffString.length() == 0) {
                resetThresholdRunoffFromModel(); // just set it back to the
                                                 // value in the streamModel.
            } else {
                try {
                    setThresholdRunoffFromString(thresholdRunoffString);
                } catch (Throwable t) {
                    resetThresholdRunoffFromModel(); // just set it back to the
                                                     // value in the
                                                     // streamModel.
                }
            }

            // the apply button does this now
            // runModel();

        } // end focusLost

    }

    // -----------------------------------------------------------------

    private class CanvasMouseMovedListener extends MouseMotionAdapter {
        private TsPaintableCanvas _canvas = null;

        private ValueMapper _valueMapper = null;

        public CanvasMouseMovedListener(TsPaintableCanvas canvas,
                ValueMapper valueMapper) {
            _canvas = canvas;
            _valueMapper = valueMapper;
        }

        @Override
        public void mouseMoved(MouseEvent event) {
            super.mouseMoved(event);
            Point screenPoint = event.getPoint();

            DataPoint dataPoint = _canvas.getViewport().getDataPoint(
                    screenPoint);

            if (_canvas.getViewport().isViewable(dataPoint)) {
                updateDataPointText(dataPoint, _valueMapper, _canvas);
            }
        }

    }

    // -----------------------------------------------------------------

    private class FrameCloseWindowListener extends WindowAdapter {
        @Override
        public void windowClosing(WindowEvent evt) {
            dispose();
        }

    }

    // -----------------------------------------------------------------

    private class CloseListener implements ActionListener {
        public void actionPerformed(ActionEvent evt) {
            dispose();
        }
    }

    // -----------------------------------------------------------------
    private class ControlWindowFocusActionListener implements ActionListener {
        public void actionPerformed(ActionEvent evt) {
            _controller.bringLaunchWindowToFront();
        }
    }

    // -----------------------------------------------------------------
    private class FloodStageCheckBoxListener implements ActionListener {
        public void actionPerformed(ActionEvent evt) {
            _forceShowFloodStage = _showFloodStageCheckBox.isSelected();
            _floodStageLinePainter.setShouldPaint(_forceShowFloodStage);
            redrawCanvases();
        }

    }

    // -----------------------------------------------------------------

    private class MajorFloodStageCheckBoxListener implements ActionListener {
        public void actionPerformed(ActionEvent evt) {
            if (_majorFloodStageLinePainter != null) {
                _forceShowMajorFloodStage = _showMajorFloodStageCheckBox
                        .isSelected();
                _majorFloodStageLinePainter
                        .setShouldPaint(_forceShowMajorFloodStage);
                redrawCanvases();
            }
        }
    }

    // -----------------------------------------------------------------
    private class ShowPrecipAmountsCheckBoxListener implements ActionListener {
        public void actionPerformed(ActionEvent evt) {
            _showPrecipAmounts = _showPrecipAmountsCheckBox.isSelected();
            _precipPainter.setShowPrecipAmounts(_showPrecipAmounts);
            _precipTextTotalPainter.setShouldPaint(_showPrecipAmounts);
            redrawCanvases();
        }

    }

    // -----------------------------------------------------------------
    private class ShowObservedHeightTsCheckBoxListener implements
            ActionListener {
        public void actionPerformed(ActionEvent evt) {
            _showObservedHeightData = _showObsHeightTsCheckBox.isSelected();
            _observedStageTsPainter.setShouldPaint(_showObservedHeightData);

            redrawCanvases();
        }
    }

    // -----------------------------------------------------------------
    private class DelayRerunCheckBoxListener implements ActionListener {
        public void actionPerformed(ActionEvent evt) {
            _delayRerunModelWhileDrawing = _delayRerunCheckBox.isSelected();
        }

    }

    // -----------------------------------------------------------------
    private class ShowMinorPrecipTicksCheckBoxListener implements
            ActionListener {
        public void actionPerformed(ActionEvent evt) {
            _showMinorPrecipTicks = _showMinorPrecipTicksCheckBox.isSelected();
            _precipBackgroundPainter.setShowMinorTicks(_showMinorPrecipTicks);
            redrawCanvases();
        }

    }

    // -----------------------------------------------------------------
    private class ShowMinorStageTicksCheckBoxListener implements ActionListener {
        public void actionPerformed(ActionEvent evt) {
            _showMinorStageTicks = _showMinorStageTicksCheckBox.isSelected();
            _stageBackgroundPainter.setShowMinorTicks(_showMinorStageTicks);
            redrawCanvases();
        }

    }

    private class ShowSimulatedStageCheckBoxListener implements ActionListener {
        public void actionPerformed(ActionEvent evt) {
            _showSimulatedStage = _showSimulatedStageCheckBox.isSelected();
            _simulatedStageTsPainter.setShouldPaint(_showSimulatedStage);
            redrawCanvases();
        }

    }

    // -----------------------------------------------------------------
    private class UseCustomModelRunStartTimeCheckBoxListener implements
            ActionListener {
        public void actionPerformed(ActionEvent evt) {

            _apiMkcUseCustomModelRunTime = _apiMkcUseCustomModelRunTimeCheckBox
                    .isSelected();
            setApiMkcModelState();
            runModel();
        }

    }

    // -----------------------------------------------------------------
    private class TimeWindowRecenterActionListener implements ActionListener {

        public void actionPerformed(ActionEvent evt) {
            recenterTimeWindowsAroundModelStartTime();
        }

    } // end TimeWindowRecenterActionListener

    // -----------------------------------------------------------------
    private class TimeWindowSlideListener implements ActionListener {
        private int _slideHours = 0;

        public TimeWindowSlideListener(int slideHours) {
            _slideHours = slideHours;
        }

        public void actionPerformed(ActionEvent evt) {

            _precipCanvas.slideTimeWindow(_slideHours);
            _stageCanvas.slideTimeWindow(_slideHours);

        }

    } // end TimeWindowSlideListener;

    // -----------------------------------------------------------------
    private class AdjustStageMouseListener extends MouseAdapter implements
            MouseMotionListener {
        private boolean _button3Down = false;

        private boolean _tipShownOnce = false;

        @Override
        public void mouseDragged(MouseEvent event) {

            if (_button3Down) {
                Point point = event.getPoint();
                adjustStage(point);
            }

        }

        @Override
        public void mouseMoved(MouseEvent event) {
            // do nothing
            return;
        }

        @Override
        public void mouseReleased(MouseEvent event) {
            int button = event.getButton();

            if (button == MouseEvent.BUTTON3) {
                _button3Down = false;
            }

            return;
        }

        @Override
        public void mousePressed(MouseEvent event) {
            int button = event.getButton();

            if (button == MouseEvent.BUTTON3) {
                _button3Down = true;
            }

            return;

        }

        @Override
        public void mouseClicked(MouseEvent event) {
            if (event.getButton() == MouseEvent.BUTTON3) {
                adjustStage(event.getPoint());
            } else if ((event.getButton() == MouseEvent.BUTTON1)
                    || ((event.getButton() == MouseEvent.BUTTON2))) {
                if (!_tipShownOnce) {
                    _tipShownOnce = true;
                    DialogHelper
                            .displayMessageDialog(
                                    AnalysisWindow.this,
                                    "TIP: To avoid accidental editing of the forecast hydrograph,\n the"
                                            + " application requires the user to click the right mouse\n button to edit the hydrograph.");
                }
            }
            _button3Down = false;

        } // end mouseClicked

        private void adjustStage(Point point) {
            AbsTimeMeasurement measurement = _stageCanvas
                    .getAbsTimeMeasurementByRounding(point);

            if (measurement.getValue() < 0.0) {
                measurement.setValue(0.0);
            }

            if (measurement.getTime() >= _modelStartTimeHolder.getTime())// forecast
                                                                         // precip
            {
                if (measurement.getValue(MeasuringUnit.feet) <= MAX_STAGE_IN_FEET) {
                    replaceTimeSeriesValue(_streamModel
                            .getForecastStageTimeSeriesHolder(), measurement);

                    // redraw the StageCanvas
                    _stageCanvas.refresh();

                    // let any observer know of the change
                    announceForecastStageEdited();
                }
            }

        }

    } // end StageAdjustmentTsCanvasEventListener

    // -----------------------------------------------------------------
    private class AdjustPrecipMouseListener extends MouseAdapter implements
            MouseMotionListener, KeyListener

    {
        private boolean _button1Down = false;

        private boolean _tipShownOnce = false;

        // variables related to keyboard commands
        private boolean _hasFocus = false;

        private Point _mousePosition = null;

        // List _pointList = new ArrayList();

        @Override
        public void mouseDragged(MouseEvent event) {
            // Point point = event.getPoint();
            // _pointList.add(point);

            if (_button1Down) {
                Point point = event.getPoint();
                // System.out.println("activated mouseDragged");
                // System.out.println("activated mouseDragged, button = " +
                // button );
                adjustPrecip(point);
            }

        }

        @Override
        public void mouseMoved(MouseEvent event) {
            _mousePosition = event.getPoint();
            // do nothing
        }

        @Override
        public void mousePressed(MouseEvent event) {
            // System.out.println("Precip adjust listener: mousePressed()");
            int button = event.getButton();
            if (button == MouseEvent.BUTTON1) {
                _button1Down = true;
            } else {
                if (!_tipShownOnce) {
                    _tipShownOnce = true;
                    DialogHelper.displayMessageDialog(AnalysisWindow.this,
                            "TIP: Use the left mouse button to edit precip.");

                }
            }

            return;

        }

        @Override
        public void mouseReleased(MouseEvent event) {
            // System.out.println("Precip adjust listener: mouseReleased()");
            int button = event.getButton();

            if (button == MouseEvent.BUTTON1) {
                _button1Down = false;

                adjustPrecip(event.getPoint());

                // if I am delaying, I need to redraw when button released,
                // otherwise, adjustPrecip
                if (_delayRerunModelWhileDrawing) {
                    runModel();
                }
            }

        }

        @Override
        public void mouseEntered(MouseEvent event) {
            // get the focus on this canvas, so that the keyboard commands will
            // work on it
            AnalysisWindow.this._precipCanvas.requestFocusInWindow();
            // System.out.println("mouseEntered():" + " requested focus ");
            _hasFocus = true;
        }

        @Override
        public void mouseExited(MouseEvent event) {
            // System.out.println("mouseExited():" +
            // " requested focus elsewhere ");
            // System.out.println("Precip adjust listener: mouseExited()");

            // when exit the window, if was delaying drawing, stop delaying
            if ((_delayRerunModelWhileDrawing) && (_button1Down)) {
                runModel();
            }

            // make sure that some other widget has focus,
            // so that keyboard clicks are not registered by the
            // precip canvas listeners
            AnalysisWindow.this._stageCanvas.requestFocusInWindow();

            _hasFocus = false;
        }

        public void keyPressed(KeyEvent e) {
            int keyCode = e.getKeyCode();
            char keyChar = e.getKeyChar();

            System.out.println("keyPressed: " + keyChar);

            if ((keyCode == KeyEvent.VK_UP) || (keyCode == KeyEvent.VK_RIGHT)) {
                System.out.println("Up typed");
                adjustPrecipByDeltaValue(0.01, _mousePosition);
            }

            if ((keyCode == KeyEvent.VK_DOWN) || (keyCode == KeyEvent.VK_LEFT)) {
                System.out.println("Down typed");
                adjustPrecipByDeltaValue(-0.01, _mousePosition);
            }
        }

        public void keyReleased(KeyEvent e) {

        }

        public void keyTyped(KeyEvent e) {

        }

        private void adjustPrecipByDeltaValue(double changeInValue, Point point) {
            System.out
                    .println("Precip adjust listener: adjustPrecipByDeltaValue()");

            AbsTimeMeasurement measurement = _precipCanvas
                    .getAbsTimeMeasurementByTruncation(point);

            // for precip, we are focused on the end time, so add an hour
            long endTime = measurement.getTime()
                    + (_intervalInHours * MILLIS_PER_HOUR);

            RegularTimeSeriesHolder tsHolder = _streamModel
                    .getPrecipTimeSeriesHolder();

            AbsTimeMeasurement oldMeasurement = tsHolder.getTimeSeries()
                    .getAbsTimeMeasurementByTime(endTime);

            measurement.setValue(oldMeasurement.getValue() + changeInValue);
            measurement.setTime(endTime);

            if (measurement.getValue() < 0.0) {
                measurement.setValue(0.0);
            }

            TimeIntervalMeasurement newMeasurement = new TimeIntervalMeasurement(
                    measurement.getValue(), measurement.getTime(),
                    _intervalInHours, measurement.getUnit());

            double value = measurement.getValue(MeasuringUnit.inches);
            boolean valueIsInRange = false;
            boolean timeIsInRange = false;

            if (value <= MAX_PRECIP_IN_INCHES_PER_HOUR) {
                valueIsInRange = true;
            }

            // ending time at or before model start time is not allowed, since
            // the period is before
            if (measurement.getTime() > _streamModel.getModelStartTime()) {
                timeIsInRange = true;
            }

            if (valueIsInRange && timeIsInRange) {
                RegularTimeSeriesHolder timeSeriesHolder = null;

                timeSeriesHolder = _streamModel.getPrecipTimeSeriesHolder();
                replaceTimeSeriesValue(timeSeriesHolder, newMeasurement);

                // redraw the PrecipCanvas
                _precipCanvas.refresh();

                // if not delaying, then rerun the model right now
                if (!_delayRerunModelWhileDrawing) {
                    runModel();
                }
            }

        } // end adjustPrecipByValue

        private void adjustPrecip(Point point) {
            // System.out.println("Precip adjust listener: adjustPrecip()");

            AbsTimeMeasurement measurement = _precipCanvas
                    .getAbsTimeMeasurementByTruncation(point);

            // System.out.println(header + measurement);

            if (measurement.getValue() < 0.0) {
                measurement.setValue(0.0);
            }

            // for precip, we are focused on the end time, so add an hour
            long endTime = measurement.getTime()
                    + (_intervalInHours * MILLIS_PER_HOUR);
            measurement.setTime(endTime);

            TimeIntervalMeasurement newMeasurement = new TimeIntervalMeasurement(
                    measurement.getValue(), measurement.getTime(),
                    _intervalInHours, measurement.getUnit());

            double value = measurement.getValue(MeasuringUnit.inches);
            boolean valueIsInRange = false;
            boolean timeIsInRange = false;

            if (value <= MAX_PRECIP_IN_INCHES_PER_HOUR) {
                valueIsInRange = true;
            }

            // ending time at or before model start time is not allowed, since
            // the period is before
            if (measurement.getTime() > _streamModel.getModelStartTime()) {
                timeIsInRange = true;
            }

            if (valueIsInRange && timeIsInRange) {
                RegularTimeSeriesHolder timeSeriesHolder = null;

                timeSeriesHolder = _streamModel.getPrecipTimeSeriesHolder();
                replaceTimeSeriesValue(timeSeriesHolder, newMeasurement);

                // redraw the PrecipCanvas
                _precipCanvas.refresh();

                // if not delaying, then rerun the model right now
                if (!_delayRerunModelWhileDrawing) {
                    runModel();
                }
            }

        } // end adjustPrecip

    } // end PrecipAdjustmentTsCanvasEventListener

    // --------------------------------------------------------------------
    private class ModelRunStartingStateActionListener implements ActionListener {

        private JComboBox _comboBox = null;

        public ModelRunStartingStateActionListener(JComboBox comboBox) {
            _comboBox = comboBox;
        }

        public void actionPerformed(ActionEvent evt) {

            if (_comboBox == AnalysisWindow.this._apiMkcStartingStateComboBox) {
                if (!_reloadingApiMkcStartingStateComboBox) {
                    setApiMkcModelState();
                }
            } else if (_comboBox == AnalysisWindow.this._sacStartingStateComboBox) {
                if (!_reloadingSacStartingStateComboBox) {
                    setSacModelState();
                }
            }

        }

    } // end ModelRunStartTimeActionListener;

    // --------------------------------------------------------------------
    private class CustomModelRunDateTimeChangedDocumentListener implements
            DocumentListener {

        public void insertUpdate(DocumentEvent arg0) {
            // don't bother to rerun the model if you aren't supposed to use
            // this time anyway
            if (_apiMkcUseCustomModelRunTime) {
                setApiMkcModelState();
                runModel();
            }
        }

        public void removeUpdate(DocumentEvent arg0) {
            // System.out.println("CustomModelRunDateTimeChangedActionListener.removeUpdate()");

        }

        public void changedUpdate(DocumentEvent arg0) {
            // System.out.println("CustomModelRunDateTimeChangedActionListener.changedUpdate()");
            // setApiMkcModelState();
            // runModel();

        }

    }

    private class LastObsTimeForAdjustmentChangedDocumentListener implements
            DocumentListener {

        public void insertUpdate(DocumentEvent arg0) {

            // String header =
            // "LastObsTimeForAdjustmentChangedDocumentListener.insertUpdate(): ";

            long lastTime = _adjustmentLastObsInputTimeTextField.getTime();

            // System.out.println(header + " lastTime = " +
            // DbTimeHelper.getDateTimeStringFromLongTime(lastTime));

            _streamModel.setLastObservedTimeForAdjustment(lastTime);

            // don't bother to rerun the model if you aren't supposed to use
            // this time anyway
            if (_streamModel.isAdjustmentOn()) {

                runModel();
            }
        }

        public void removeUpdate(DocumentEvent arg0) {

        }

        public void changedUpdate(DocumentEvent arg0) {

        }

    }

    // --------------------------------------------------------------------
    private class ModelChoiceListener implements ActionListener {
        // private int _invocationCount = 0;

        public void actionPerformed(ActionEvent event) {
            // _invocationCount ++;
            // System.out.println("---------------ModelChoiceListener.actionPerformed() invoked "
            // + _invocationCount + " times.");

            setModelTypeToComboBoxSelection();
        }
    }

    // --------------------------------------------------------------------
    private class UnitHydrographChoiceListener implements ActionListener {
        // private int _invocationCount = 0;

        public void actionPerformed(ActionEvent event) {
            setUnitHydrographToComboBoxSelection();
        }
    }

    // --------------------------------------------------------------------
    private class ForecastHeightTimeSeriesLaunchActionListener implements
            ActionListener {
        public void actionPerformed(ActionEvent evt) {
            RegularTimeSeriesEditor editor = null;

            RegularTimeSeriesHolder tsHolder = _streamModel
                    .getForecastStageTimeSeriesHolder();
            String titleString = "Forecast Stage Editor";
            String unitString = "Unknown Units";
            if (tsHolder.getTimeSeries() != null) {
                unitString = tsHolder.getTimeSeries().getMeasuringUnit()
                        .getName();
            }
            String valueLabelString = "Stage (" + unitString + ")";
            ValueMapper valueMapper = new StageToFlowValueMapper(_streamModel
                    .getRatingCurve());

            String mappedUnitString = valueMapper.getOutputMeasuringUnit()
                    .getName();
            String mappedValueLabelString = "Discharge(" + mappedUnitString
                    + ")";

            boolean isModal = false;
            boolean isEditable = true;

            long initialSearchTime = 0;
            double missingValue = _defaultMissingValue;

            RegularTimeSeriesEditorDescriptor descriptor = new RegularTimeSeriesEditorDescriptor(
                    AnalysisWindow.this, tsHolder, titleString,
                    valueLabelString, _defaultFormatString, isEditable,
                    isModal, valueMapper, mappedValueLabelString,
                    initialSearchTime, missingValue, MIN_STAGE_IN_FEET,
                    MAX_STAGE_IN_FEET);

            editor = new RegularTimeSeriesEditor(descriptor);

            // editor.addApplyActionListener(new
            // ForecastHeightTimeSeriesApplyActionListener());
            editor.setVisible(true);
        }
    }

    // --------------------------------------------------------------------
    private class PrecipTimeSeriesLaunchActionListener implements
            ActionListener {
        public void actionPerformed(ActionEvent evt) {
            RegularTimeSeriesEditor editor = null;

            RegularTimeSeriesHolder tsHolder = _streamModel
                    .getPrecipTimeSeriesHolder();
            String titleString = "Mean Areal Precip Editor";

            String unitString = "Unknown Units";
            if (tsHolder.getTimeSeries() != null) {
                unitString = tsHolder.getTimeSeries().getMeasuringUnit()
                        .getName();
            }
            String valueLabelString = "Precip (" + unitString + ")";

            boolean isEditable = true;
            boolean isModal = false;

            ValueMapper valueMapper = null;
            String mappedValueLabelString = "BOGUS";

            long initialSearchTime = _streamModel.getModelStartTime()
                    + _millisPerHour;
            double missingValue = _defaultMissingValue;

            RegularTimeSeriesEditorDescriptor descriptor = new RegularTimeSeriesEditorDescriptor(
                    AnalysisWindow.this, tsHolder, titleString,
                    valueLabelString, _defaultFormatString, isEditable,
                    isModal, valueMapper, mappedValueLabelString,
                    initialSearchTime, missingValue,
                    MIN_PRECIP_IN_INCHES_PER_HOUR,
                    MAX_PRECIP_IN_INCHES_PER_HOUR);

            editor = new RegularTimeSeriesEditor(descriptor);
            editor.setVisible(true);

            return;
        }
    }

    // --------------------------------------------------------------------
    private class EvaporationTimeSeriesLaunchActionListener implements
            ActionListener {
        public void actionPerformed(ActionEvent evt) {
            RegularTimeSeriesEditor editor = null;

            RegularTimeSeriesHolder tsHolder = _streamModel
                    .getEvaporationTimeSeriesHolder();
            String titleString = "Mean Areal Potential Evaporation";

            String unitString = "Unknown Units";
            if (tsHolder.getTimeSeries() != null) {
                unitString = tsHolder.getTimeSeries().getMeasuringUnit()
                        .getName();
            }
            String valueLabelString = "Evap. (" + unitString + ")";

            // editor = new RegularTimeSeriesEditor(AnalysisWindow.this,
            // tsHolder, title, valueLabelString);

            boolean isEditable = true;
            boolean isModal = false;

            ValueMapper valueMapper = null;
            String mappedValueLabelString = "BOGUS";

            long initialSearchTime = _streamModel.getModelStartTime();
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

    // --------------------------------------------------------------------
    private class PriorRunoffTimeSeriesLaunchActionListener implements
            ActionListener {
        public void actionPerformed(ActionEvent evt) {
            RegularTimeSeriesEditor editor = null;

            RegularTimeSeriesHolder tsHolder = _streamModel
                    .getPriorRunoffTimeSeriesHolder();
            String titleString = "Prior Computed Runoff ";

            RegularTimeSeries ts = tsHolder.getTimeSeries();
            MeasuringUnit unit = null;

            String unitString = "Unknown Units";
            if (tsHolder.getTimeSeries() != null) {
                unitString = tsHolder.getTimeSeries().getMeasuringUnit()
                        .getName();
                String valueLabelString = "Runoff (" + unitString + ")";

                boolean isEditable = false;
                boolean isModal = false;

                ValueMapper valueMapper = null;
                String mappedValueLabelString = "BOGUS";

                long initialSearchTime = _streamModel.getModelStartTime();
                double missingValue = _defaultMissingValue;

                RegularTimeSeriesEditorDescriptor descriptor = new RegularTimeSeriesEditorDescriptor(
                        AnalysisWindow.this, tsHolder, titleString,
                        valueLabelString, _defaultFormatString, isEditable,
                        isModal, valueMapper, mappedValueLabelString,
                        initialSearchTime, missingValue,
                        MIN_RUNOFF_IN_INCHES_PER_HOUR,
                        MAX_RUNOFF_IN_INCHES_PER_HOUR);

                editor = new RegularTimeSeriesEditor(descriptor);

                // this is not editable, so there is no apply action listener
                // editor.addApplyActionListener(new
                // PriorRunoffTimeSeriesApplyActionListener());

                editor.setVisible(true);

            }

            else {
                // JOptionPane.showMessageDialog(AnalysisWindow.this,
                // "Sorry, there is no prior computed runoff.");
                DialogHelper.displayMessageDialog(AnalysisWindow.this,
                        "Sorry, there is no prior computed runoff.");
            }

        }
    }

    // --------------------------------------------------------------------
    private class RunoffTimeSeriesLaunchActionListener implements
            ActionListener {
        public void actionPerformed(ActionEvent evt) {
            RegularTimeSeriesEditor editor = null;

            RegularTimeSeriesHolder tsHolder = _streamModel
                    .getRunoffTimeSeriesHolder();
            String titleString = "Computed Runoff ";

            String unitString = "Unknown Units";
            if (tsHolder.getTimeSeries() != null) {
                unitString = tsHolder.getTimeSeries().getMeasuringUnit()
                        .getName();
            }
            String valueLabelString = "Runoff (" + unitString + ")";

            boolean isEditable = false;
            boolean isModal = false;

            ValueMapper valueMapper = null;
            String mappedValueLabelString = "BOGUS";

            long initialSearchTime = _streamModel.getModelStartTime()
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

    // --------------------------------------------------------------------
    private class ForecastHeightSaveLaunchActionListener implements
            ActionListener {
        public void actionPerformed(ActionEvent evt) {
            ForecastTsSaveDialog dialog = null;

            RegularTimeSeriesHolder tsHolder = _streamModel
                    .getForecastStageTimeSeriesHolder();
            String title = "Forecast Height Time Series Save Dialog";
            String locId = _streamModel.getLocationId();

            String tableName = "FcstHeight";
            // String[] physicalElementStringArray = {"HG", "HP", "HT"};

            String[] physicalElementStringArray = _streamModel.getPeArray('H');

            String primaryPe = _streamModel.getPrimaryPe();
            String preferredPhysicalElement = primaryPe;

            if (primaryPe.charAt(0) != 'H') {
                preferredPhysicalElement = "HG";
            }

            dialog = new ForecastTsSaveDialog(AnalysisWindow.this, locId,
                    title, tsHolder, _streamModel.getDataMgr(), tableName,
                    physicalElementStringArray, preferredPhysicalElement);

            dialog.setVisible(true);

        }
    }

    // --------------------------------------------------------------------
    private class ForecastDischargeSaveLaunchActionListener implements
            ActionListener {
        public void actionPerformed(ActionEvent evt) {
            ForecastTsSaveDialog dialog = null;

            RegularTimeSeries dischargeTimeSeries = _streamModel
                    .getDischargeTimeSeries();
            RegularTimeSeriesHolder dischargeTimeSeriesHolder = new RegularTimeSeriesHolder();
            dischargeTimeSeriesHolder.setTimeSeries(dischargeTimeSeries);

            String title = "Forecast Discharge Time Series Save Dialog";
            String locId = _streamModel.getLocationId();

            String tableName = "FcstDischarge";
            // String[] physicalElementStringArray = {"QR"};

            String[] physicalElementStringArray = _streamModel.getPeArray('Q');

            String primaryPe = _streamModel.getPrimaryPe();
            String preferredPhysicalElement = primaryPe;
            if (primaryPe.charAt(0) != 'Q') {
                preferredPhysicalElement = "QR";
            }

            dialog = new ForecastTsSaveDialog(AnalysisWindow.this, locId,
                    title, dischargeTimeSeriesHolder,
                    _streamModel.getDataMgr(), tableName,
                    physicalElementStringArray, preferredPhysicalElement);

            dialog.setVisible(true);

        }
    }

    // --------------------------------------------------------------------
    private class EvapSaveLaunchActionListener implements ActionListener {
        public void actionPerformed(ActionEvent evt) {
            ForecastTsSaveDialog dialog = null;

            RegularTimeSeriesHolder tsHolder = _streamModel
                    .getEvaporationTimeSeriesHolder();
            String title = "Evapotranspiration Time Series Save Dialog";
            String locId = _streamModel.getLocationId();

            String tableName = "FcstOther";
            String[] physicalElementStringArray = { "EA" };
            String preferredPhysicalElement = "EA";

            dialog = new ForecastTsSaveDialog(AnalysisWindow.this, locId,
                    title, tsHolder, _streamModel.getDataMgr(), tableName,
                    physicalElementStringArray, preferredPhysicalElement);
            dialog
                    .addApplyActionListener(new ForecastOtherSaveCompleteListener());

            dialog.setVisible(true);

        }
    }

    // ----------------------------------------------------------------------------------------------

    private class MAPSaveLaunchActionListener implements ActionListener {
        public void actionPerformed(ActionEvent evt) {
            usePrecipFileChooserForSaving();
        }
    }

    // --------------------------------------------------------------------
    private class SacParamsLaunchActionListener implements ActionListener {
        public void actionPerformed(ActionEvent evt) {

            String basinId = _streamModel.getBasinId();

            SacParamsEditor editor = null;
            editor = new SacParamsEditor(AnalysisWindow.this, _streamModel
                    .getDataMgr(), basinId);

            editor
                    .addApplyActionListener(new SacParamsSaveCompleteActionListener());

            editor.setVisible(true);
        }
    }

    // --------------------------------------------------------------------
    private class AdjustToggleActionListener implements ActionListener {
        public void actionPerformed(ActionEvent evt) {
            boolean isAdjustmentOn = _adjustmentCheckBox.isSelected();
            _streamModel.setAdjustmentOn(isAdjustmentOn);
            runModel();
        }
    }

    // --------------------------------------------------------------------
    private class AdjustmentHoursChangeListener implements ChangeListener {
        public void stateChanged(ChangeEvent e) {

            Integer integer = (Integer) _blendPeriodSpinner.getValue();

            int newValue = integer.intValue();

            // _blendPeriodValueLabel.setText(newValue + "" );

            if (_streamModel.isAdjustmentOn()) {
                _streamModel.setAdjustmentHours(newValue);
                runModel();
            }
        }

    }

    // --------------------------------------------------------------------

    private class LiveSacStatesAdjustmentActionListener implements
            ActionListener {
        public void actionPerformed(ActionEvent event) {
            // final String header =
            // "LiveSacStatesAdjustmentActionListener.actionPeformed(): ";
            // System.out.println(header + "message = " +
            // event.getActionCommand());

            runModel();
        }
    }

    // --------------------------------------------------------------------

    private class ForecastHoursChangeListener implements ChangeListener {
        public void stateChanged(ChangeEvent e) {
            Integer integer = (Integer) _forecastHoursSpinner.getValue();
            int newValue = integer.intValue();

            _streamModel.setForecastLengthInHours(newValue);
            runModel();

        }
    }

    // --------------------------------------------------------------------

    private class SacStateLaunchActionListener implements ActionListener {
        public void actionPerformed(ActionEvent evt) {
            String basinId = _streamModel.getBasinId();

            SacStateEditor editor = null;
            editor = new SacStateEditor(AnalysisWindow.this, _streamModel
                    .getDataMgr(), basinId);

            editor
                    .addApplyActionListener(new SacStateSaveCompleteActionListener());
            editor.setVisible(true);

        }
    }

    private class LiveSacStateLaunchActionListener implements ActionListener {
        public void actionPerformed(ActionEvent evt) {
            boolean isModal = false;

            // used by AnalysisWindow to call runModel();
            ActionListener liveSacStatesAdjustmentActionListener = new LiveSacStatesAdjustmentActionListener();

            LiveSacStateEditor editor = new LiveSacStateEditor(
                    AnalysisWindow.this, _streamModel.getSacState(),
                    _streamModel.getSacParams(), isModal,
                    liveSacStatesAdjustmentActionListener);

            editor.setVisible(true);

        }
    }

    // --------------------------------------------------------------------

    private class SacStateSaveCompleteActionListener implements ActionListener {
        // make sure that the model settings are reloaded and
        // then rerun the model
        public void actionPerformed(ActionEvent evt) {

            _streamModel.reloadModelData();

            _streamModel.runModel();

        }
    }

    // --------------------------------------------------------------------

    private class SacParamsSaveCompleteActionListener implements ActionListener {
        // make sure that the model settings are reloaded and
        // then rerun the model
        public void actionPerformed(ActionEvent evt) {
            _streamModel.reloadModelData();

            _streamModel.runModel();

        }
    }

    // --------------------------------------------------------------------

    private class MonthlyMapeEditorLaunchActionListener implements
            ActionListener {
        public void actionPerformed(ActionEvent evt) {
            MonthlyValueEditor editor = null;

            JFrame ownerFrame = AnalysisWindow.this;
            DataMgr dataMgr = _streamModel.getDataMgr();
            String basinId = _streamModel.getBasinId();
            boolean isModal = false;
            String pe = "EA";

            editor = new MonthlyValueEditor(ownerFrame, dataMgr, basinId, pe,
                    isModal);
            editor.setVisible(true);

        }
    }

    // --------------------------------------------------------------------

    private class ForecastOtherSaveCompleteListener implements ActionListener {
        // a listener for the forecast save dialog
        public void actionPerformed(ActionEvent event) {
            // System.out.println("ForecastOtherSaveCompleteListener invoked");
            // do nothing, needed to fulfill editor class obligations
        }

    }

    // --------------------------------------------------------------------

    private class ForecastHeightTimeSeriesChangeListener implements
            TimeSeriesListener {
        public Object getReceiver() {
            return AnalysisWindow.this;
        }

        public void handleTimeSeriesEvent(TimeSeriesEvent event) {
            // String header =
            // "ForecastHeightTimeSeriesChangeListener.handleTimeSeriesEvent()";
            // System.out.println(header + "event = " + event.toString());
            redrawCanvases();

        }
    }

    // --------------------------------------------------------------------

    private class PrecipTimeSeriesChangeListener implements TimeSeriesListener {

        public Object getReceiver() {
            return AnalysisWindow.this;
        }

        // used for receiving events from precip tabular editing
        public void handleTimeSeriesEvent(TimeSeriesEvent event) {
            // String header =
            // "PrecipTimeSeriesChangeListener.handleTimeSeriesEvent()";
            // System.out.println(header + "event = " + event.toString());
            runModel();

        }

    }

    // --------------------------------------------------------------------

    private class EvaporationTimeSeriesChangeListener implements
            TimeSeriesListener {

        public Object getReceiver() {
            return AnalysisWindow.this;
        }

        // used for tabular editing of the evaporation time series
        public void handleTimeSeriesEvent(TimeSeriesEvent event) {
            // String header =
            // "EvaporationTimeSeriesChangeListener.handleTimeSeriesEvent()";
            // System.out.println(header + "event = " + event.toString());
            runModel();

        }

    }

    // --------------------------------------------------------------------

    private class ScreenCaptureActionListener implements ActionListener {
        public void actionPerformed(ActionEvent event) {

            useCaptureScreenFileChooserForSaving();
            // AnalysisWindow.this.displayCaptureScreenFileChooser();

        } // end actionPerformed

    } // end ScreenCaptureActionListener

    // --------------------------------------------------------------------

    // -----------------------------------------------------------------
} // end class AnalysisWindow
