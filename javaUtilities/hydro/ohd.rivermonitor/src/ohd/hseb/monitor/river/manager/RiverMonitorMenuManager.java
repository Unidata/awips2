package ohd.hseb.monitor.river.manager;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import javax.swing.ButtonGroup;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JSeparator;
import javax.swing.JToolBar;

import ohd.hseb.alertalarm.AlertAlarmDataManager;
import ohd.hseb.alertalarm.AlertAlarmDialog;
import ohd.hseb.monitor.MonitorFrame;
import ohd.hseb.monitor.MonitorMessage;
import ohd.hseb.monitor.MonitorTimeSeriesLiteManager;
import ohd.hseb.monitor.MonitorToolBarManager;
import ohd.hseb.monitor.manager.MonitorMenuManager;
import ohd.hseb.monitor.manager.Receiver;
import ohd.hseb.monitor.messaging.MessageSystemManager;
import ohd.hseb.monitor.messaging.MessageType;
import ohd.hseb.monitor.river.RiverMonitorDataManager;
import ohd.hseb.monitor.river.RiverMonitorJTableRowData;
import ohd.hseb.officenotes.OfficeNotesDataManager;
import ohd.hseb.officenotes.OfficeNotesDialog;
import ohd.hseb.monitor.river.LookBackTimeDialog;
import ohd.hseb.monitor.river.settings.RiverMonitorMenuSettings;
import ohd.hseb.monitor.river.settings.RiverColumnDataSettings;
import ohd.hseb.monitor.util.HydroappsDefaultDirs;
import ohd.hseb.rivermonlocgroup.RiverMonGroupDialog;
import ohd.hseb.rivermonlocgroup.RiverMonLocationDialog;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.SessionLogger;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.vtecevent.VtecEventDataManager;
import ohd.hseb.vtecevent.VtecEventDialog;

public class RiverMonitorMenuManager extends MonitorMenuManager
{
    private JMenuBar _menuBar;
    private JMenu  _fileMenu, _displayMenu, _sortMenu, _configMenu, _detailsMenu, _helpMenu;
    private JMenu _fcstTsMenu;
    private JRadioButtonMenuItem _fcstTsFZMenuItem;
    private JRadioButtonMenuItem _fcstTsFFMenuItem;
    private JRadioButtonMenuItem _fcstTsIngestFilterMenuItem; 
    private ButtonGroup _fcstTsGroup;
    private JMenuItem _selectColumnsMenuItem;
    private JCheckBoxMenuItem _showMissingRiverMenuItem;
    private JMenuItem _saveColumnSettingsMenuItem;
    private JMenuItem _loadSettingsMenuItem;
    private JMenuItem _loadOfficeSettingsMenuItem;
    private JMenuItem _saveOfficeSettingsMenuItem;
    private JMenuItem _refreshColumnMenuItem;
    private JMenuItem _exportTableToTextMenuItem;
    private JMenuItem _precipMonitorMenuItem;
    private JMenuItem _exitApplicationMenuItem;
    private JMenuItem _officeNotesMenuItem;
    private JMenuItem _alertAlarmMenuItem;
    private JMenuItem _vtecEventMenuItem;
    private JMenuItem _siteSpecificMenuItem;
    private JMenuItem _timeSeriesLiteMenuItem;
    private JMenuItem _riverMonitorPEConfigMenuItem;
    private JMenuItem _riverMonGroupMenuItem;
    private JMenuItem _riverMonLocationMenuItem;
    private JMenuItem _derivedColumnsMenuItem;
    private JMenuItem _hsagrplocSortMenuItem;
    private JMenuItem _riverThreatSortMenuItem;
    private JMenuItem _precipThreatSortMenuItem;
    private JMenuItem _clearSortMenuItem;

    private JMenuItem _validTimeLookUpForAlertAlarmMenuItem;
    private JMenuItem _endTimeLookUpForVTECEventMenuItem;    
    private JMenuItem _ugcExpireTimeLookUpMenuItem;
    private JMenuItem _vtecEventProductTimeLookUpMenuItem;
    private JMenuItem _obsTimeLookUpForLatestObsValueMenuItem;
    private JMenuItem _fcstBasisTimeLookUpForMaxFcstValueMenuItem;
    private JMenuItem _precipThresholdForThreatColorMenuItem;

    private JMenuItem _aboutMenuItem;

    private RiverMonGroupDialog _riverMonGroupDialog = null;
    private RiverMonLocationDialog _riverMonLocationDialog;

    private RiverMonitorDataManager _rivermonitorDataManager;
    private RiverMonitorMenuSettings _menuSettings;

    private OfficeNotesDialog _officeNotesDialog;
    private OfficeNotesDataManager _officeNotesDataMgr;
    private AlertAlarmDialog _alertAlarmDialog;
    private AlertAlarmDataManager _alertAlarmDataMgr;
    private VtecEventDataManager _vtecEventDataMgr;
    private VtecEventDialog _vtecEventDialog;

    private String _settingsDir;

    private MonitorTimeSeriesLiteManager _monitorTimeSeriesLiteManager;

    private RiverMonitorJTableRowData _selectedRowData;

    private MonitorToolBarManager _toolBarManager;
    
    public RiverMonitorMenuManager(MessageSystemManager msgSystemManager, 
            SessionLogger logger, MonitorFrame mainFrame, String version, 
            String versionDate, AppsDefaults appsDefaults, JToolBar toolBar,
            RiverMonitorDataManager riverMonitorDataManager, OfficeNotesDataManager officeNotesDataMgr,
            AlertAlarmDataManager alertAlarmDataMgr, VtecEventDataManager vtecEventDataMgr,
            RiverMonitorMenuSettings menuSettings, JPanel timeDisplayPanel, String appName,
            MonitorTimeSeriesLiteManager monitorTimeSeriesLiteManager)
    {
        super(msgSystemManager, appsDefaults, logger);
        _appsDefaults = appsDefaults;
        _rivermonitorDataManager = riverMonitorDataManager;

        _menuSettings  = menuSettings;

        _officeNotesDataMgr = officeNotesDataMgr;
        _alertAlarmDataMgr = alertAlarmDataMgr;
        _vtecEventDataMgr = vtecEventDataMgr;
        _monitorTimeSeriesLiteManager = monitorTimeSeriesLiteManager;

        _settingsDir = _appsDefaults.getToken("rivermon_config_dir", 
        	HydroappsDefaultDirs.RIVERMON_CONFIG_DIR);

        int defaultRefreshInterval = 15;
        createRefreshComponents(defaultRefreshInterval, timeDisplayPanel);

        _toolBarManager = new MonitorToolBarManager(toolBar, _appsDefaults, _officeNotesDataMgr);
        _toolBarManager.getOfficeNotesButton().addActionListener(new OfficeNotesDialogListener());

        setAboutInfo(version, versionDate, appName);
        _mainFrame = mainFrame;

        setMessageSystemManager(msgSystemManager);
        _msgSystemManager.registerReceiver(new UpdateDisplayWithSettingsReceiver(), MessageType.UPDATE_DISPLAY_WITH_SETTINGS);
        _msgSystemManager.registerReceiver(new ViewSelectItemReceiver(), MessageType.VIEW_SELECT_ITEM);
        
        _msgSystemManager.registerReceiver(new UpdateDisplayReceiver(), MessageType.REFRESH_DISPLAY);


        createMenus();
        setFrameListener();
    }
    
    protected long getDataUpdateTime()
    {
        return _rivermonitorDataManager.getPdcUpdateTime();
    }
    
    protected long getDisplayedRecordCount()
    {
        return _rivermonitorDataManager.getDisplayedRecordCount();
    }

    public void setMenuSettingsRefreshInterval(int value)
    {
        _menuSettings.setRefreshInterval(value);
    }

    public void setRefreshTimeSpinnerValue()
    {
        _refreshTimeSpinner.setValue(_menuSettings.getRefreshInterval());
    }

    private void createMenus()
    {
        _menuBar = new JMenuBar();
        _mainFrame.setJMenuBar(_menuBar);

        _toolBarManager.getOfficeNotesButton().addActionListener(new OfficeNotesDialogListener());
        
        _fileMenu = new JMenu("File");
        _configMenu = new JMenu("Config");
        _displayMenu = new JMenu("Display");
        _sortMenu = new JMenu("Sort");
        _detailsMenu = new JMenu("Details");
        _helpMenu = new JMenu("Help");

        _fileMenu.setMnemonic('F');
        _configMenu.setMnemonic('C');
        _displayMenu.setMnemonic('D');
        _sortMenu.setMnemonic('S');
        _detailsMenu.setMnemonic('E');
        _helpMenu.setMnemonic('H');

        
        //File Menu 

        _refreshColumnMenuItem = new JMenuItem("Refresh");
        _refreshColumnMenuItem.setMnemonic('R');
        _fileMenu.add(_refreshColumnMenuItem);
        
        _fileMenu.add(new JSeparator());
        
        _saveColumnSettingsMenuItem = new JMenuItem("Save Custom  Settings ...");
        _saveColumnSettingsMenuItem.setMnemonic('S');
        _fileMenu.add(_saveColumnSettingsMenuItem);

        _saveOfficeSettingsMenuItem = new JMenuItem("Save Office Settings");
        _saveOfficeSettingsMenuItem.setMnemonic('F');
        _saveOfficeSettingsMenuItem.setToolTipText("Save office settings to PrecipMonitorSettings.txt");
        _fileMenu.add(_saveOfficeSettingsMenuItem);

        _loadSettingsMenuItem = new JMenuItem("Load Custom Settings ...");
        _loadSettingsMenuItem.setMnemonic('L');
        _fileMenu.add(_loadSettingsMenuItem);

        _loadOfficeSettingsMenuItem = new JMenuItem("Load Office Settings");
        _loadOfficeSettingsMenuItem.setMnemonic('O');
        _loadOfficeSettingsMenuItem.setToolTipText("Load office settings from PrecipMonitorSettings.txt");
        _fileMenu.add(_loadOfficeSettingsMenuItem);
        
        _fileMenu.add(new JSeparator());

        _exportTableToTextMenuItem = new JMenuItem("Export Data To Text File ...");
        _exportTableToTextMenuItem.setMnemonic('x');
        _fileMenu.add(_exportTableToTextMenuItem);
        
        _fileMenu.add(new JSeparator());
  
        _precipMonitorMenuItem = new JMenuItem("PrecipMonitor ...");
        _precipMonitorMenuItem.setMnemonic('P');
        _fileMenu.add(_precipMonitorMenuItem);
        
        _fileMenu.add(new JSeparator());


        _exitApplicationMenuItem = new JMenuItem("Exit");
        _exitApplicationMenuItem.setMnemonic('E');
        _fileMenu.add(_exitApplicationMenuItem);
        
        //Display Menu
        
      
        _selectColumnsMenuItem = new JMenuItem("Select Columns ...");
        _selectColumnsMenuItem.setMnemonic('S');
        _displayMenu.add(_selectColumnsMenuItem);
        
        _displayMenu.add(new JSeparator());
     
        _fcstTsMenu = new JMenu("Forecast Source");
        _fcstTsMenu.setMnemonic('F');
        _displayMenu.add(_fcstTsMenu);
        
        _validTimeLookUpForAlertAlarmMenuItem = new JMenuItem("Alert Alarm ValidTime ...");
        _validTimeLookUpForAlertAlarmMenuItem.setMnemonic('A');
        _displayMenu.add(_validTimeLookUpForAlertAlarmMenuItem);

        _endTimeLookUpForVTECEventMenuItem = new JMenuItem("VTEC Event EndTime ...");
        _endTimeLookUpForVTECEventMenuItem.setMnemonic('V');
        _displayMenu.add(_endTimeLookUpForVTECEventMenuItem);

        _ugcExpireTimeLookUpMenuItem = new JMenuItem("UGCExpire Time ...");
        _ugcExpireTimeLookUpMenuItem.setMnemonic('U');
        _displayMenu.add(_ugcExpireTimeLookUpMenuItem);
        
        _vtecEventProductTimeLookUpMenuItem = new JMenuItem("VTEC Event Product Time ...");
        _vtecEventProductTimeLookUpMenuItem.setMnemonic('P');
        _displayMenu.add(_vtecEventProductTimeLookUpMenuItem);

        _obsTimeLookUpForLatestObsValueMenuItem = new JMenuItem("Latest ObsTime ...");
        _obsTimeLookUpForLatestObsValueMenuItem.setMnemonic('O');
        _displayMenu.add(_obsTimeLookUpForLatestObsValueMenuItem);

        _fcstBasisTimeLookUpForMaxFcstValueMenuItem = new JMenuItem("Latest Fcst Basis Time ...");
        _fcstBasisTimeLookUpForMaxFcstValueMenuItem.setMnemonic('B');
        _displayMenu.add(_fcstBasisTimeLookUpForMaxFcstValueMenuItem);

        _precipThresholdForThreatColorMenuItem = new JMenuItem("Precip Threshold ...");
        _precipThresholdForThreatColorMenuItem.setMnemonic('P');
        _displayMenu.add(_precipThresholdForThreatColorMenuItem);
        
        
        _displayMenu.add(new JSeparator());
        
        _showMissingRiverMenuItem = new JCheckBoxMenuItem("Show Locations with Missing Obs/Fcst Data");
        _showMissingRiverMenuItem.setMnemonic('M');
        _displayMenu.add(_showMissingRiverMenuItem);

        //Config Menu
        
     
        _riverMonGroupMenuItem = new JMenuItem("Group Definitions ...");
        _riverMonGroupMenuItem.setMnemonic('G');
        _configMenu.add(_riverMonGroupMenuItem);

        _riverMonLocationMenuItem = new JMenuItem("Location Grouping/Ordering ...");
        _riverMonLocationMenuItem.setMnemonic('L');
        _configMenu.add(_riverMonLocationMenuItem);
        
        _configMenu.add(new JSeparator());
        

        _riverMonitorPEConfigMenuItem = new JMenuItem("River Data Type Override ...");
        _riverMonitorPEConfigMenuItem.setMnemonic('P');
        _configMenu.add(_riverMonitorPEConfigMenuItem);

        _configMenu.add(new JSeparator());
        
        _derivedColumnsMenuItem = new JMenuItem("Derived Columns ...");
        _derivedColumnsMenuItem.setMnemonic('D');
        _configMenu.add(_derivedColumnsMenuItem);
        
        

        
        //Sort Menu
        
        _clearSortMenuItem = new JMenuItem("Clear Sort");
        _clearSortMenuItem.setMnemonic('C');
        _clearSortMenuItem.setToolTipText("Clear All Sorts");
        _sortMenu.add(_clearSortMenuItem);
        
        _sortMenu.add(new JSeparator());
        
        _hsagrplocSortMenuItem = new JMenuItem("Sort by HSA | Group | Location Columns");
        _hsagrplocSortMenuItem.setMnemonic('O');
        _hsagrplocSortMenuItem.setToolTipText("Sort by HSA, group id, location id");
        _sortMenu.add(_hsagrplocSortMenuItem);
        
        _riverThreatSortMenuItem = new JMenuItem("River Threat Sort");
        _riverThreatSortMenuItem.setMnemonic('R');
        _riverThreatSortMenuItem.setToolTipText("Sort by Threat Column");
        _sortMenu.add(_riverThreatSortMenuItem);
        
        _precipThreatSortMenuItem = new JMenuItem("Precip Threat Sort");
        _precipThreatSortMenuItem.setMnemonic('P');
        _precipThreatSortMenuItem.setToolTipText("Sort by Precip Threat Column");
        _sortMenu.add(_precipThreatSortMenuItem);

       

        
        //Details Menu
        
        _officeNotesMenuItem = new JMenuItem("Office Notes ...");
        _officeNotesMenuItem.setMnemonic('O');
        _detailsMenu.add(_officeNotesMenuItem);

        _alertAlarmMenuItem = new JMenuItem("Alert Alarm ...");
        _alertAlarmMenuItem.setMnemonic('A');
        _detailsMenu.add(_alertAlarmMenuItem);

        _vtecEventMenuItem = new JMenuItem("VTEC Events ...");
        _vtecEventMenuItem.setMnemonic('V');
        _detailsMenu.add(_vtecEventMenuItem);
        
        _detailsMenu.add(new JSeparator());
        
        _siteSpecificMenuItem = new JMenuItem("SiteSpecific App...");
        _siteSpecificMenuItem.setMnemonic('S');
        _detailsMenu.add(_siteSpecificMenuItem);

        _timeSeriesLiteMenuItem = new JMenuItem("Time Series Lite...");
        _timeSeriesLiteMenuItem.setMnemonic('T');
        _detailsMenu.add(_timeSeriesLiteMenuItem);
 
        // Help Menu
        
        _aboutMenuItem = new JMenuItem("About ...");
        _aboutMenuItem.setMnemonic('A');
        _helpMenu.add(_aboutMenuItem);

     

        //sub menus
        _fcstTsGroup = new ButtonGroup();
        _fcstTsIngestFilterMenuItem = new JRadioButtonMenuItem("IngestFilter");
        _fcstTsIngestFilterMenuItem.setSelected(true);
        FcstTsMenuItemListener fcstTsListener = new FcstTsMenuItemListener();
        _fcstTsIngestFilterMenuItem.addActionListener(fcstTsListener);
        _fcstTsGroup.add(_fcstTsIngestFilterMenuItem);
        _fcstTsMenu.add(_fcstTsIngestFilterMenuItem);
        _fcstTsFFMenuItem = new JRadioButtonMenuItem("FF");
        _fcstTsFFMenuItem.addActionListener(fcstTsListener);
        _fcstTsGroup.add(_fcstTsFFMenuItem);
        _fcstTsMenu.add(_fcstTsFFMenuItem);
        _fcstTsFZMenuItem = new JRadioButtonMenuItem("FZ");
        _fcstTsFZMenuItem.addActionListener(fcstTsListener);
        _fcstTsGroup.add(_fcstTsFZMenuItem);
        _fcstTsMenu.add(_fcstTsFZMenuItem);
        
     

        setMenuListeners();

        _menuBar.add(_fileMenu);
        _menuBar.add(_displayMenu);
        _menuBar.add(_configMenu);
        _menuBar.add(_sortMenu);
        _menuBar.add(_detailsMenu);
        _menuBar.add(_helpMenu);
    }

    private void setMenuListeners()
    {
        ChangeColumnsDisplayedMenuListener changeMenuListener = new 
        ChangeColumnsDisplayedMenuListener();
        _selectColumnsMenuItem.addActionListener(changeMenuListener);

        ShowMissingRiverMenuListener showMissingPrecipMenuListener = new 
        ShowMissingRiverMenuListener();
        _showMissingRiverMenuItem.addItemListener(showMissingPrecipMenuListener);

        SaveSettingsListener saveMenuListener = new  
        SaveSettingsListener();
        _saveColumnSettingsMenuItem.addActionListener(saveMenuListener);

        LoadSettingsListener loadMenuListener = new LoadSettingsListener();
        _loadSettingsMenuItem.addActionListener(loadMenuListener);

        LoadOfficeSettingsListener loadOfficeSettingsListener = new LoadOfficeSettingsListener();
        _loadOfficeSettingsMenuItem.addActionListener(loadOfficeSettingsListener);

        SaveOfficeSettingsListener saveOfficeSettingsListener = new SaveOfficeSettingsListener();
        _saveOfficeSettingsMenuItem.addActionListener(saveOfficeSettingsListener);

        ExitApplicationListener exitApplicationListener = new ExitApplicationListener();
        _exitApplicationMenuItem.addActionListener(exitApplicationListener);

        RefreshMenuListener RefreshMenuListener = new
        RefreshMenuListener();
        _refreshColumnMenuItem.addActionListener(RefreshMenuListener);
        
        SaveTableListener saveTableListener = new SaveTableListener(); 
        _exportTableToTextMenuItem.addActionListener(saveTableListener);
        
        PrecipMonitorListener precipMonitorListener = new PrecipMonitorListener(); 
        _precipMonitorMenuItem.addActionListener(precipMonitorListener);
        
        AppSortListener appSortListener = new AppSortListener();
        _hsagrplocSortMenuItem.addActionListener(appSortListener);
        
        PrecipThreatSortListener precipThreatSortListener = new PrecipThreatSortListener();
        _precipThreatSortMenuItem.addActionListener(precipThreatSortListener);
        
        RiverThreatSortListener riverThreatSortListener = new RiverThreatSortListener();
        _riverThreatSortMenuItem.addActionListener(riverThreatSortListener);

        ClearSortListener clearSortListener = new ClearSortListener();
        _clearSortMenuItem.addActionListener(clearSortListener);

        AlertAlarmDialogListener alertAlarmDialogListener = new AlertAlarmDialogListener();
        _alertAlarmMenuItem.addActionListener(alertAlarmDialogListener);

        VtecEventDialogListener vtecEventDialogListener = new VtecEventDialogListener();
        _vtecEventMenuItem.addActionListener(vtecEventDialogListener);

        SiteSpecificListener siteSpecificListener = new SiteSpecificListener();
        _siteSpecificMenuItem.addActionListener(siteSpecificListener);

        OfficeNotesDialogListener officeNotesDialogListener = new OfficeNotesDialogListener();
        _officeNotesMenuItem.addActionListener(officeNotesDialogListener);
         
        TimeSeriesLiteMenuItemListener timeSeriesLiteMenuItemListener = new TimeSeriesLiteMenuItemListener();
        _timeSeriesLiteMenuItem.addActionListener(timeSeriesLiteMenuItemListener);

        RiverMonitorPEConfigEditorListener riverMonitorPEConfigEditorListener = new RiverMonitorPEConfigEditorListener();
        _riverMonitorPEConfigMenuItem.addActionListener(riverMonitorPEConfigEditorListener);

        RiverMonGroupDialogListener riverMonGroupDialogListener = new RiverMonGroupDialogListener();
        _riverMonGroupMenuItem.addActionListener(riverMonGroupDialogListener);

        RiverMonLocationDialogListener riverMonLocationDialogListener = new RiverMonLocationDialogListener();
        _riverMonLocationMenuItem.addActionListener(riverMonLocationDialogListener);

        AlertAlarmValidTimeListener alertAlarmValidTimeListener = new AlertAlarmValidTimeListener();
        _validTimeLookUpForAlertAlarmMenuItem.addActionListener(alertAlarmValidTimeListener);

        VtecEventEndTimeListener vtecEventEndTimeListener = new VtecEventEndTimeListener();
        _endTimeLookUpForVTECEventMenuItem.addActionListener(vtecEventEndTimeListener);

        UGCExpireTimeListener ugcExpireTimeListener = new UGCExpireTimeListener();
        _ugcExpireTimeLookUpMenuItem.addActionListener(ugcExpireTimeListener);

        VtecEventProductTimeListener vtecEventProductTimeListener = new VtecEventProductTimeListener();
        _vtecEventProductTimeLookUpMenuItem.addActionListener(vtecEventProductTimeListener);

        LatestObsTimeListener latestObsTimeListener = new LatestObsTimeListener();
        _obsTimeLookUpForLatestObsValueMenuItem.addActionListener(latestObsTimeListener);

        LatestFcstBasisTimeListener latestFcstBasisTimeListener = new LatestFcstBasisTimeListener();
        _fcstBasisTimeLookUpForMaxFcstValueMenuItem.addActionListener(latestFcstBasisTimeListener);

        PrecipThresholdListener precipThresholdListener = new PrecipThresholdListener(_menuSettings.getPrecipSettings());
        _precipThresholdForThreatColorMenuItem.addActionListener(precipThresholdListener); 

        DerivedColumnsEditorListener derivedColumnsEditorListener = new DerivedColumnsEditorListener();
        _derivedColumnsMenuItem.addActionListener(derivedColumnsEditorListener);

        AboutListener aboutListener = new AboutListener();
        _aboutMenuItem.addActionListener(aboutListener);

    }

    private class DerivedColumnsEditorListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
            String editorTitle = ("\"").concat("Derived Columns").concat("\"");
            String file = _settingsDir + "/" + RiverMonitorAppManager.DERIVED_COLUMNS_FILE; 
            launchTextEditor(editorTitle, file, "RiverMonitor Application");
        }
    }

    private void setFrameListener()
    {
        FrameListener frameListener = new FrameListener();
        _mainFrame.addWindowListener(frameListener);
    }

    public void closeApplication()
    {
        _logger.log("RiverMonitor Application Exiting....");
        _logger.log("====================================");
        _rivermonitorDataManager.disconnect();
        _mainFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        System.exit(0);
    }

    private void createOfficeNotesDialog()
    {
        List lidList = _rivermonitorDataManager.getLidList();
        String lids[] = new String[lidList.size()];
        for(int i=0; i < lidList.size(); i++)
        {
            lids[i] = lidList.get(i).toString();
        }
        Arrays.sort(lids);
        System.out.println("Create officenotes lids size:"+ lidList.size());
        Map lidDescDetailsMap = _rivermonitorDataManager.getLidDescDetails();
        if(_officeNotesDialog == null)
            _officeNotesDialog = new OfficeNotesDialog(_mainFrame, _officeNotesDataMgr, "RIVERMON", lids, lidDescDetailsMap, _logger);
    }

    public void loadSettings()
    {
        String header = "RiverMonitorMenuManager.loadSettings(): ";

        System.out.print(header);
        send(this, MessageType.LOAD_SETTINGS);
    }

    private void loadOfficeSettings()
    {
        String header = "RiverMonitorMenuManager.loadOfficeSettings(): ";

        System.out.print(header);
        send(this, MessageType.LOAD_OFFICE_SETTINGS);
    }

    public void viewSelectItem(MonitorMessage message)
    {
        String header = "RiverMonitorMenuManager.viewSelectItem(): ";
        System.out.println(header +" Invoked"+ "...Source:" + message.getMessageSource() + " Type:" + message.getMessageType());
        _selectedRowData = (RiverMonitorJTableRowData) message.getMessageData();
    }
    
    private class UpdateDisplayReceiver implements Receiver
    {
        public void receive(MonitorMessage message)
        {
            updateRecordCount();
        }
    }

    public void updateDisplayWithSettings(MonitorMessage message)
    {
        String header = "RiverMonitorMenuManager.updateDisplayWithSettings(): ";
        System.out.println(header +" Invoked"+ "...Source:" + message.getMessageSource() + " Type:" + message.getMessageType());
        refreshTimeDisplay();
        updateMenuWithNewSettings();
    }
    
    private void updateMenuWithNewSettings()
    {
        String fcstTypeSource = _menuSettings.getRiverSettings().getFcstTypeSource();
      
        if(fcstTypeSource != null)
        {
            if(fcstTypeSource.equals("IngestFilter"))
            {
                _fcstTsIngestFilterMenuItem.setSelected(true);
            }
            else if(fcstTypeSource.equals("FF"))
            {
                _fcstTsFFMenuItem.setSelected(true);
            }
            else 
            {
                _fcstTsFZMenuItem.setSelected(true);
            }
        }
        else
        {
            _fcstTsFZMenuItem.setActionCommand("IngestFilter");
        }
        
        if(_menuSettings.shouldShowMissingRiver())
            _showMissingRiverMenuItem.setSelected(true);
        else
            _showMissingRiverMenuItem.setSelected(false);
    }

    public void saveSettings()
    {
        String header = "RiverMonitorMenuManager.saveSettings(): ";

        System.out.print(header);
        send(this, MessageType.SAVE_SETTINGS);
    }

    private void saveOfficeSettings()
    {
        String header = "RiverMonitorMenuManager.saveOfficeSettings(): ";

        System.out.print(header);
        send(this, MessageType.SAVE_OFFICE_SETTINGS);
    }

    public void loadSettings(File fileHandler)
    {
        if(fileHandler != null)
        {
            String header = "RiverMonitorMenuManager.loadSettings(): ";

            System.out.print(header);
            send(this, MessageType.LOAD_SETTINGS);
        }
    }

    public void loadSettingsFromFile(File fileHandler)
    {
        String header = "RiverMonitorMenuManager.loadSettingsFromFile(): ";
        if(fileHandler != null)
        {
            _logger.log(header + "Read settings from " +  fileHandler.getAbsolutePath());
            loadSettings(fileHandler);
        }
    }

  
    private void changeColumns()
    {
        String header = "RiverMonitorMenuManager.changeColumns(): ";

        System.out.print(header);
        send(this, MessageType.CHANGE_COLUMNS);
    }

    private void saveTable()
    {
        String header = "RiverMonitorMenuManager.saveTable(): ";

        System.out.print(header);
        send(this, MessageType.CREATE_TEXT_FROM_VIEW); 
    }
    
    private void precipThreatSort()
    {
        String header = "RiverMonitorMenuManager.precipThreatSort(): ";

        System.out.print(header);
        send(this, MessageType.CLEAR_SORT);
        
        System.out.print(header);
        send(this, MessageType.PRECIP_THREAT_SORT);
    }
    
    private void riverThreatSort()
    {
        String header = "RiverMonitorMenuManager.riverThreatSort(): ";

        System.out.print(header);
        send(this, MessageType.CLEAR_SORT);
        
        System.out.print(header);
        send(this, MessageType.RIVER_THREAT_SORT);
    }
    
    private void appSort()
    {
        String header = "RiverMonitorMenuManager.appSort(): ";

        System.out.print(header);
        send(this, MessageType.CLEAR_SORT);
        
        System.out.print(header);
        send(this, MessageType.APP_SORT);
    }

    private void clearSort()
    {
        String header = "RiverMonitorMenuManager.clearSort(): ";
        System.out.print(header);
        send(this, MessageType.CLEAR_SORT);
    }

    private void createRiverMonGroupDialog()
    {
        String defaultHsa = _rivermonitorDataManager.getDefaultHsa();
        if(_riverMonGroupDialog == null)
            _riverMonGroupDialog = new RiverMonGroupDialog(_mainFrame,
                            _rivermonitorDataManager.getRiverMonLocGroupDataManager(), _logger, defaultHsa);
    }

    private void createRiverMonLocationDialog()
    {
        if(_riverMonLocationDialog == null)
            _riverMonLocationDialog = new RiverMonLocationDialog(_mainFrame, 
                            _rivermonitorDataManager.getRiverMonLocGroupDataManager(), _logger);
    }

    private void createAlertAlarmDialog()
    {
        if(_alertAlarmDialog == null)
            _alertAlarmDialog = new AlertAlarmDialog(_mainFrame, _alertAlarmDataMgr, _logger);
    }
    
    private void createVtecEventDialog()
    {
        if(_vtecEventDialog == null)
            _vtecEventDialog = new VtecEventDialog(_mainFrame, _vtecEventDataMgr, _logger);
    }
   
    private class ExitApplicationListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
            closeApplication();
        }
    }
    
    private class VtecEventDialogListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
            createVtecEventDialog();
            if(_selectedRowData != null)
            {
                _vtecEventDialog.showVtecEventDialog(_selectedRowData.getLid());
            }
            else
                _vtecEventDialog.showVtecEventDialog(null);
        }
    }

    private class AlertAlarmDialogListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
            createAlertAlarmDialog();
            if(_selectedRowData != null)
            {
                _alertAlarmDialog.showAlertAlarmDialog(_selectedRowData.getLid());
            }
            else
                _alertAlarmDialog.showAlertAlarmDialog(null);
        }
    }

    private class TimeSeriesLiteMenuItemListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
            if(_selectedRowData != null)
            {
                _monitorTimeSeriesLiteManager.displayTimeSeriesLite(_mainFrame, _selectedRowData);
            }
            else
            {
                String textMsg = "<HTML><BODY> Unable to display TimeSeriesLite <BR>"+
                "Please highlight a row... </BODY></HTML>";
                JOptionPane.showMessageDialog(_mainFrame, textMsg,"TimeSeriesLite", JOptionPane.PLAIN_MESSAGE);
            }
        }
    }

    private class SiteSpecificListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
            String whfsBinDir = _appsDefaults.getToken("whfs_bin_dir", 
            	HydroappsDefaultDirs.WHFS_BIN_DIR);
            String siteSpecificExe = whfsBinDir + "/run_SiteSpecific";

            Runtime rt = Runtime.getRuntime();

            String cmd;
            if(_selectedRowData != null)
                cmd = siteSpecificExe + " "+ _selectedRowData.getLid();
            else
                cmd = siteSpecificExe;
            
            try
            {
                _mainFrame.setWaitCursor();
                rt.exec(cmd);
                _logger.log("SiteSpecific App is launched [cmd:"+ cmd +"]");
                _mainFrame.setDefaultCursor();
            }
            catch(IOException ex)
            {
                _mainFrame.setDefaultCursor();
                String str = "Unable to launch SiteSpecific Appln:["+ cmd +"]" + "Exception:"+ ex;
                JOptionPane.showMessageDialog(_mainFrame, str,"RiverMonitor Application",JOptionPane.PLAIN_MESSAGE);
            }
        }
    }
    
    private class PrecipMonitorListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
           startMonitor("PrecipMonitor Application", "PRECIP");          
        }
    }
    
    private class SaveTableListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
           saveTable();          
        }
    }

    public class OLDShowMissingRiverMenuListener implements ItemListener
    {
        public void itemStateChanged(ItemEvent e)
        {
            boolean prevValue = _menuSettings.shouldShowMissingRiver();
            boolean newValue = false;
            if( e.getStateChange()  == ItemEvent.SELECTED)
            {
                newValue = true;
            }
            else if(e.getStateChange() == ItemEvent.DESELECTED)
            {
                newValue = false;
            }

            _menuSettings.setShowMissingRiver(newValue);

            if( prevValue != newValue)
            {
                menuChange();
            }
        }
    }
    
    public class ShowMissingRiverMenuListener implements ItemListener
    {
        public void itemStateChanged(ItemEvent e)
        {
            String header = "RiverMonitorMenuManager.ShowMissingPrecipMenuListener.itemStateChanged()";

            boolean prevValue = _menuSettings.shouldShowMissingRiver();
            boolean newValue = false;
            if( e.getStateChange()  == ItemEvent.SELECTED)
            {
                newValue = true;
            }

            _menuSettings.setShowMissingRiver(newValue);

            if( prevValue != newValue)
            {
                sendMissingFilterChanged();
            }
        }
    }
    
    public class ChangeColumnsDisplayedMenuListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
            changeColumns();
        }
    }

    private class ClearSortListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
            clearSort();
        }
    }

    private class PrecipThreatSortListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
            precipThreatSort();
        }
    }
    
    private class RiverThreatSortListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
            riverThreatSort();
        }
    }
    
    private class AppSortListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
            appSort();
        }
    }

    private class RefreshMenuListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
            refresh();
        }
    }

    private class AlertAlarmValidTimeListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
            int prevValue = _menuSettings.getRiverSettings().getAlertAlarmLookBack();

            new LookBackTimeDialog( _mainFrame, _menuSettings.getRiverSettings(),
                "AlertAlarm----ValidTime Filter", 
                new JLabel("<HTML><CENTER>Num Of Hrs To Lookback:</CENTER></HTML>"),
                RiverColumnDataSettings.VALID_TIME_TAG, 168, 1);

            if(prevValue != _menuSettings.getRiverSettings().getAlertAlarmLookBack())
            {
                menuChange();
            }
        }
    }

    private class VtecEventEndTimeListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
            int prevValue = _menuSettings.getRiverSettings().getVtecEventEndTimeApproach();

            new LookBackTimeDialog( _mainFrame, _menuSettings.getRiverSettings(),
                "VTEC Event----EndTime Filter", 
                new JLabel("<HTML><CENTER>Num Of Hrs To LookAhead:</CENTER></HTML>"),
                RiverColumnDataSettings.VTEC_EVENT_END_TIME_TAG, 24, 1);

            if(prevValue != _menuSettings.getRiverSettings().getVtecEventEndTimeApproach())
            {
                menuChange();
            }
        }
    }
 
    private class VtecEventProductTimeListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
            int prevValue = _menuSettings.getRiverSettings().getVtecEventProductTimeLookBack();

            new LookBackTimeDialog( _mainFrame, _menuSettings.getRiverSettings(),
                "VTEC Event----Product Time Filter", 
                new JLabel("<HTML><CENTER>Num Of Hrs To Lookback:</CENTER></HTML>"),
                RiverColumnDataSettings.VTEC_EVENT_PRODUCT_TIME_TAG, 9000, 1);

            if(prevValue != _menuSettings.getRiverSettings().getVtecEventProductTimeLookBack())
            {
                menuChange();
            }
        }
    }
    
    private class UGCExpireTimeListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
            int prevValue = _menuSettings.getRiverSettings().getUgcExpireTimeApproach();

            new LookBackTimeDialog( _mainFrame, _menuSettings.getRiverSettings(),
                "UGC ExpireTime Filter", 
                new JLabel("<HTML><CENTER>Num Of Hrs To LookAhead:</CENTER></HTML>"),
                RiverColumnDataSettings.UGC_EXPIRE_TIME_TAG, 24, 1);

            if(prevValue != _menuSettings.getRiverSettings().getUgcExpireTimeApproach())
            {
                menuChange();
            }
        }
    }

    private class LatestObsTimeListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
            int prevValue = _menuSettings.getRiverSettings().getLatestObsLookBack();

            new LookBackTimeDialog( _mainFrame, _menuSettings.getRiverSettings(),
                "Latest ObsTime Filter", 
                new JLabel("<HTML><CENTER>Num Of Hrs To Lookback:</CENTER></HTML>"),
                RiverColumnDataSettings.LATEST_OBS_TIME_TAG, 720, 1);

            if(prevValue != _menuSettings.getRiverSettings().getLatestObsLookBack())
            {
                menuChange();
            }
        }
    }

    private class LatestFcstBasisTimeListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
            int prevValue = _menuSettings.getRiverSettings().getLatestFcstBasisTimeLookBack();

            new LookBackTimeDialog( _mainFrame, _menuSettings.getRiverSettings(),
                "Latest Fcst BasisTime Filter", 
                new JLabel("<HTML><CENTER>Num Of Hrs To Lookback:</CENTER></HTML>"),
                RiverColumnDataSettings.LATEST_FCST_BASIS_TIME_TAG, 720, 1);

            if(prevValue != _menuSettings.getRiverSettings().getLatestFcstBasisTimeLookBack())
            {
                menuChange();
            }
        }
    }

    private class FcstTsMenuItemListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
            String prevValue = _menuSettings.getRiverSettings().getFcstTypeSource();
            String selectedItem = e.getActionCommand();
            _menuSettings.getRiverSettings().setFcstTypeSource(selectedItem);
            
            if( ! prevValue.equalsIgnoreCase(_menuSettings.getRiverSettings().getFcstTypeSource()))
            {
                menuChange();
            }
        }
    }
    
   

    private class RiverMonitorPEConfigEditorListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
            String editorTitle = ("\"").concat("PEConfig File").concat("\"");
            String file = _settingsDir + RiverMonitorAppManager.PE_CONFIG_FILE; 
            launchTextEditor(editorTitle, file, "RiverMonitor Application");
        }
    }

    private class RiverMonGroupDialogListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
            createRiverMonGroupDialog();
            boolean hasTableChanged = _riverMonGroupDialog.showRiverMonGroupDialog();
            if(hasTableChanged)
            {
                menuChange();
            }
        }
    }

    private class RiverMonLocationDialogListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
            createRiverMonLocationDialog();
            boolean hasTableChanged = _riverMonLocationDialog.showRiverMonLocationDialog();
            System.out.println("Rivermonlocation table changed:"+ hasTableChanged);
            if(hasTableChanged)
            {
                menuChange();
            }
        }
    }

    private class LoadOfficeSettingsListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
            loadOfficeSettings();
        }
    }

    private class SaveOfficeSettingsListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
            String message = "<HTML><BODY> This will overwrite standard office settings for RiverMonitor. <BR>" +
            "Are you sure ? <BR> <BODY> <HTML>";
            if(confirmSaveOfficeSettings(message))
                saveOfficeSettings();
        }
    }

    private class LoadSettingsListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
            loadSettings();
        }
    }

    private class FrameListener extends WindowAdapter
    {
        public void windowClosing(WindowEvent event)
        {
            closeApplication();
        }
    }

    private class OfficeNotesDialogListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
            createOfficeNotesDialog();
            if(_selectedRowData != null)
            {
                _officeNotesDialog.showOfficeNotesDialog(_selectedRowData.getLid());
            }
            else
                _officeNotesDialog.showOfficeNotesDialog(null);
            
            _toolBarManager.setOfficeNotesButtonIcon(_officeNotesDataMgr.getCountOfOfficeNotes());
        }
    }

    private class SaveSettingsListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
            saveSettings();
        }
    }

    private class UpdateDisplayWithSettingsReceiver implements Receiver
    {
        public void receive(MonitorMessage message)
        {
            setInitialRefreshTimeInfo();
            updateDisplayWithSettings(message);
        }
    }

    private class ViewSelectItemReceiver implements Receiver
    {
        public void receive(MonitorMessage message)
        {
            viewSelectItem(message);
        }
    }

}
