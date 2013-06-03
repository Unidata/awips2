package ohd.hseb.monitor.precip.manager;

import java.awt.Component;
import java.awt.Container;
import java.awt.Graphics;
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
import ohd.hseb.monitor.precip.PrecipMonitorDataManager;
import ohd.hseb.monitor.precip.settings.PrecipMonitorMenuSettings;
import ohd.hseb.monitor.settings.FilterSettings;
import ohd.hseb.monitor.settings.ViewSettings;
import ohd.hseb.monitor.util.HydroappsDefaultDirs;
import ohd.hseb.officenotes.OfficeNotesDataManager;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.MemoryLogger;
import ohd.hseb.util.SessionLogger;
import ohd.hseb.util.gui.ComponentHelper;

public class PrecipMonitorAppManager
{
    private MessageSystemManager _msgSystemManager;

    private PrecipMonitorDataManager _precipMonitorDataManager;

    private AppsDefaults _appsDefaults;

    public static final String DEFAULT_PRECIP_SETTINGS_FILE = "DefaultPrecipMonitorSettings.txt";
    public static final String SITE_PRECIP_SETTINGS_FILE = "PrecipMonitorSettings.txt";

    private SessionLogger _logger;
    private MonitorFrame _mainFrame;
    
    private DerivedColumnsFileManager _derivedColumnsFileManager;
    
    private String _appName = "PrecipMonitor";

    public static String DERIVED_COLUMNS_FILE = "PrecipDerivedColumns.txt";

    public PrecipMonitorAppManager(Database db, String missingRepresentation, String version, String versionDate,
                                    MonitorTimeSeriesLiteManager monitorTimeSeriesLiteManager)
    {
        _appsDefaults = new AppsDefaults();

        createLogger();

        ViewSettings viewSettings = new ViewSettings();
        FilterSettings filterSettings = new FilterSettings();
        PrecipMonitorMenuSettings menuSettings = new PrecipMonitorMenuSettings();

        createFrame(db, version);

        JSplitPane splitPane = new MonitorSplitPane(JSplitPane.HORIZONTAL_SPLIT, true);
         
        JPanel timeDisplayPanel = new JPanel();
        JToolBar toolBar = new JToolBar();

        OfficeNotesDataManager officeNotesDataMgr = new OfficeNotesDataManager(db,_logger, missingRepresentation);
        AlertAlarmDataManager alertAlarmDataMgr = new AlertAlarmDataManager(db, _logger, missingRepresentation);

        String settingsDir = _appsDefaults.getToken("rivermon_config_dir", 
        	HydroappsDefaultDirs.RIVERMON_CONFIG_DIR);
        
        _derivedColumnsFileManager = new DerivedColumnsFileManager(settingsDir + "/" + DERIVED_COLUMNS_FILE, _logger);       
        
        MemoryLogger.setLogger(_logger);
        _precipMonitorDataManager = new PrecipMonitorDataManager(db, _appsDefaults, missingRepresentation, 
                                    _logger, menuSettings, _derivedColumnsFileManager);
        
        _msgSystemManager = new MessageSystemManager();

        
        //These objects remain in without a variable, because they register listeners to the _msgSystemManager
        new PrecipMonitorRefreshManager(_msgSystemManager, _logger, _precipMonitorDataManager, splitPane);

        new PrecipMonitorFilterManager(_msgSystemManager, _logger, _mainFrame, _precipMonitorDataManager,  splitPane, _appsDefaults, filterSettings);

        new PrecipMonitorMenuManager(_msgSystemManager, _logger, _mainFrame, version, versionDate, _appsDefaults, 
            toolBar, _precipMonitorDataManager, officeNotesDataMgr,alertAlarmDataMgr, menuSettings, 
            timeDisplayPanel, _appName, monitorTimeSeriesLiteManager);

        new PrecipMonitorViewManager(_mainFrame, _msgSystemManager, _logger, _precipMonitorDataManager, splitPane,
                                viewSettings, _derivedColumnsFileManager, monitorTimeSeriesLiteManager, _appsDefaults);
   
       PrecipMonitorSettingsManager settingsManager =  new PrecipMonitorSettingsManager(_msgSystemManager, _appsDefaults, _logger, _mainFrame, viewSettings, 
                                    filterSettings, menuSettings);

       settingsManager.startPrecipMonitor();
  
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

    private class MonitorJSplitPane extends JSplitPane
    {
       
        
        
        public MonitorJSplitPane (int newOrientation,
                                boolean newContinuousLayout)
        {
            super(newOrientation, newContinuousLayout);
        }
        
        public void setDividerLocation(int location)
        {
            int currentDividerLocation = getDividerLocation();
            int adjustedLocation = location;
            
            
            if (location < 30 )
            {
                int difference = currentDividerLocation - location;
                if (difference > 50)
                {
                    adjustedLocation = currentDividerLocation;
                }
            }
            super.setDividerLocation(adjustedLocation);
        }
        
        public void setRightComponent(Component comp)
        {
            String header = "MonitorJSplitPane.setRightComponent() ";
            
            int beforeDividerLocation = getDividerLocation();
            
          //  setDividerLocation(beforeDividerLocation);
            super.setRightComponent(comp);
            
            int afterDividerLocation = getDividerLocation();
            
            setDividerLocation(beforeDividerLocation);
            
            System.out.println(header + " before divider location = " + beforeDividerLocation);
            System.out.println(header + "  after divider location = " + afterDividerLocation);
            
        }
        
        public void setLeftComponent(Component comp)
        {
            String header = "MonitorJSplitPane.setLeftComponent() ";
            
            int beforeDividerLocation = getDividerLocation();
            setDividerLocation(beforeDividerLocation);
            
            super.setLeftComponent(comp);
            
            int afterDividerLocation = getDividerLocation();
            
            setDividerLocation(beforeDividerLocation);
            
            System.out.println(header + " before divider location = " + beforeDividerLocation);
            System.out.println(header + "  after divider location = " + afterDividerLocation);
            
         }
        
      
        public void paint(Graphics g)
        {
            String header = "MonitorJSplitPane.paint() ";
            super.paint(g);
            
            int dividerLocation = getDividerLocation();
            int lastDividerLocation = getLastDividerLocation();
            
            System.out.println(header + " divider location = " + dividerLocation);
            System.out.println(header + " last divider location = " + lastDividerLocation);
            
          // if (lastDividerLocation > dividerLocation)
          //  {
         //       setDividerLocation(lastDividerLocation);
          //  }
        }
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
