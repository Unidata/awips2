package ohd.hseb.monitor.river.manager;

import java.awt.Container;
import java.awt.GridBagLayout;

import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JToolBar;

import ohd.hseb.alertalarm.AlertAlarmDataManager;
import ohd.hseb.db.Database;
import ohd.hseb.monitor.MonitorFrame;
import ohd.hseb.monitor.MonitorSplitPane;
import ohd.hseb.monitor.MonitorTimeSeriesLiteManager;
import ohd.hseb.monitor.derivedcolumns.DerivedColumnsFileManager;
import ohd.hseb.monitor.messaging.MessageSystemManager;
import ohd.hseb.monitor.river.RiverMonitorDataManager;
import ohd.hseb.monitor.river.settings.RiverMonitorMenuSettings;
import ohd.hseb.monitor.settings.FilterSettings;
import ohd.hseb.monitor.settings.ViewSettings;
import ohd.hseb.monitor.util.HydroappsDefaultDirs;
import ohd.hseb.officenotes.OfficeNotesDataManager;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.MemoryLogger;
import ohd.hseb.util.SessionLogger;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.vtecevent.VtecEventDataManager;

public class RiverMonitorAppManager
{
    private MessageSystemManager _msgSystemManager;

    private RiverMonitorDataManager _riverMonitorDataManager;

    private AppsDefaults _appsDefaults;

    public static final String DEFAULT_RIVER_SETTINGS_FILE = "DefaultRiverMonitorSettings.txt";
    public static final String SITE_RIVER_SETTINGS_FILE = "RiverMonitorSettings.txt";
    public static final String PE_CONFIG_FILE = "RiverMonitorPEConfig.txt";

    private SessionLogger _logger;
    private MonitorFrame _mainFrame;

    private DerivedColumnsFileManager _derivedColumnsFileManager;
    
    private String _appName = "RiverMonitor";
    
    public static String DERIVED_COLUMNS_FILE = "DerivedColumns.txt";

    public RiverMonitorAppManager(Database db, String missingRepresentation, String version, 
                                String versionDate, MonitorTimeSeriesLiteManager monitorTimeSeriesLiteManager)
    {
        _appsDefaults = new AppsDefaults();

        createLogger();

        ViewSettings viewSettings = new ViewSettings();
        FilterSettings filterSettings = new FilterSettings();
        RiverMonitorMenuSettings menuSettings = new RiverMonitorMenuSettings();

        createFrame(db, version);
     
        JSplitPane splitPane = new MonitorSplitPane(JSplitPane.HORIZONTAL_SPLIT, true);
       
        JPanel timeDisplayPanel = new JPanel();
        JToolBar toolBar = new JToolBar();

        OfficeNotesDataManager officeNotesDataMgr = new OfficeNotesDataManager(db,_logger, missingRepresentation);
        AlertAlarmDataManager alertAlarmDataMgr = new AlertAlarmDataManager(db, _logger, missingRepresentation);
        VtecEventDataManager vtecEventDataMgr = new VtecEventDataManager(db, _logger, missingRepresentation);

        String settingsDir = _appsDefaults.getToken("rivermon_config_dir", 
        	HydroappsDefaultDirs.RIVERMON_CONFIG_DIR);
      
        _derivedColumnsFileManager = new DerivedColumnsFileManager(settingsDir + "/" + DERIVED_COLUMNS_FILE, _logger);

        _riverMonitorDataManager = new RiverMonitorDataManager(db, _appsDefaults, missingRepresentation, 
            _logger, menuSettings, _derivedColumnsFileManager);

        _msgSystemManager = new MessageSystemManager();

        MemoryLogger.setLogger(_logger);
        
        new RiverMonitorRefreshManager(_msgSystemManager, _logger, _riverMonitorDataManager, splitPane);

        new RiverMonitorFilterManager(_msgSystemManager, _logger, _mainFrame, _riverMonitorDataManager,  splitPane, _appsDefaults, filterSettings);

        new RiverMonitorMenuManager(_msgSystemManager, _logger, _mainFrame, version, versionDate, _appsDefaults, 
            toolBar, _riverMonitorDataManager, officeNotesDataMgr, alertAlarmDataMgr, vtecEventDataMgr,
            menuSettings, timeDisplayPanel, _appName, monitorTimeSeriesLiteManager);

        new RiverMonitorViewManager(_mainFrame, _msgSystemManager, _logger, _riverMonitorDataManager, splitPane,
            viewSettings, _derivedColumnsFileManager, monitorTimeSeriesLiteManager, _appsDefaults);

        RiverMonitorSettingsManager settingsManager =  new RiverMonitorSettingsManager(_msgSystemManager, _appsDefaults, _logger, _mainFrame, viewSettings, 
            filterSettings, menuSettings);

        settingsManager.startRiverMonitor();

        Container frameContentPane = _mainFrame.getContentPane();
        _mainFrame.setLayout(new GridBagLayout());


        //col, row, width, height, weightx, weighty, fill       
        ComponentHelper.addFrameComponent(frameContentPane, 
            toolBar,          0,  0,  1,  1,  1,  0,  1);
        ComponentHelper.addFrameComponent(frameContentPane, 
            timeDisplayPanel,     1,  0,  1,  1,  1,  0,  1);
        ComponentHelper.addFrameComponent(frameContentPane, 
            splitPane, 0,  1,  2,  1,  1,  1,  1);


        _mainFrame.pack();
        _mainFrame.setVisible(true);
    }

    private void createLogger()
    {
        String logDir = _appsDefaults.getToken("rivermon_log_dir", 
        	HydroappsDefaultDirs.RIVERMON_LOG_DIR);
         String logFile = logDir + "/" + _appName;

        _logger = new SessionLogger(_appName, logFile, true, true, null);
        _logger.log(null);
        _logger.log(_appName + " Started");

    }

    private void createFrame(Database db, String version)
    {
        _mainFrame = new MonitorFrame();
        String title = _appName + "     DbName : " + db.getDatabaseName() + 
        "      Session : "+ _logger.getSessionId();  
        _mainFrame.setTitle(title);
    }

}
