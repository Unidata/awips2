package ohd.hseb.monitor.precip.manager;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
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
import ohd.hseb.monitor.precip.PrecipMonitorDataManager;
import ohd.hseb.monitor.precip.PrecipMonitorJTableRowData;
import ohd.hseb.monitor.precip.settings.PrecipMonitorMenuSettings;
import ohd.hseb.monitor.util.HydroappsDefaultDirs;
import ohd.hseb.officenotes.OfficeNotesDataManager;
import ohd.hseb.officenotes.OfficeNotesDialog;
import ohd.hseb.rivermonlocgroup.RiverMonGroupDialog;
import ohd.hseb.rivermonlocgroup.RiverMonLocationDialog;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.SessionLogger;

public class PrecipMonitorMenuManager extends MonitorMenuManager
{
    private JMenuBar _menuBar;
    private JMenu  _fileMenu, _displayMenu, _sortMenu, _configMenu, _detailsMenu, _helpMenu;
    private JMenuItem _selectColumnsMenuItem;
    private JCheckBoxMenuItem _showMissingPrecipMenuItem;
    private JMenuItem _saveColumnSettingsMenuItem;
    private JMenuItem _loadSettingsMenuItem;
    private JMenuItem _loadOfficeSettingsMenuItem;
    private JMenuItem _saveOfficeSettingsMenuItem;
    private JMenuItem _refreshColumnMenuItem;
    private JMenuItem _exportTableToTextMenuItem;
    private JMenuItem _riverMonitorMenuItem;
    private JMenuItem _exitApplicationMenuItem;
    
    private JMenuItem _officeNotesMenuItem;
    private JMenuItem _alertAlarmMenuItem;
    private JMenuItem _timeSeriesLiteMenuItem;
    private JMenuItem _riverMonGroupMenuItem;
    private JMenuItem _riverMonLocationMenuItem;
    private JMenuItem _derivedColumnsMenuItem;
    private JMenuItem _hsagrplocSortMenuItem;
    private JMenuItem _precipThreatSortMenuItem;
    private JMenuItem _clearSortMenuItem;
    private JMenuItem _precipThresholdForThreatColorMenuItem;
    private JMenuItem _aboutMenuItem;
    
    private RiverMonGroupDialog _riverMonGroupDialog = null;
    private RiverMonLocationDialog _riverMonLocationDialog;
    
    private AlertAlarmDialog _alertAlarmDialog;
    private AlertAlarmDataManager _alertAlarmDataMgr;
    
    private PrecipMonitorDataManager _precipMonitorDataManager;
    private PrecipMonitorMenuSettings _menuSettings;

    private OfficeNotesDialog _officeNotesDialog;
    private OfficeNotesDataManager _officeNotesDataMgr;
    
    private String _settingsDir;
    
    private MonitorTimeSeriesLiteManager _monitorTimeSeriesLiteManager;
    
    private PrecipMonitorJTableRowData _selectedRowData;
    
    private MonitorToolBarManager _toolBarManager;
    
    public PrecipMonitorMenuManager(MessageSystemManager msgSystemManager, 
            SessionLogger logger, MonitorFrame mainFrame, String version, 
            String versionDate, AppsDefaults appsDefaults, JToolBar toolBar,
            PrecipMonitorDataManager precipMonitorDataManager, OfficeNotesDataManager officeNotesDataMgr,
            AlertAlarmDataManager alertAlarmDataMgr,
            PrecipMonitorMenuSettings menuSettings, JPanel timeDisplayPanel, String appName,
            MonitorTimeSeriesLiteManager monitorTimeSeriesLiteManager)
    {
        super(msgSystemManager, appsDefaults, logger);
        _appsDefaults = appsDefaults;
        
        _menuSettings = menuSettings;
        
        _precipMonitorDataManager = precipMonitorDataManager;
        _alertAlarmDataMgr = alertAlarmDataMgr;

        _officeNotesDataMgr = officeNotesDataMgr;
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
        return _precipMonitorDataManager.getPdcUpdateTime();
    }
    
    protected long getDisplayedRecordCount()
    {
        return _precipMonitorDataManager.getDisplayedRecordCount();   
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
        _refreshColumnMenuItem.setToolTipText("Update screen with latest available data. Same as Update Now");
        _fileMenu.add(_refreshColumnMenuItem); 
        
        _fileMenu.add(new JSeparator());

        _saveColumnSettingsMenuItem = new JMenuItem("Save Custom Settings ...");
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
        
        _riverMonitorMenuItem = new JMenuItem("RiverMonitor ...");
        _riverMonitorMenuItem.setMnemonic('M');
        _fileMenu.add(_riverMonitorMenuItem);
        
        _fileMenu.add(new JSeparator());
        
        
        _exitApplicationMenuItem = new JMenuItem("Exit");
        _exitApplicationMenuItem.setMnemonic('E');
        _fileMenu.add(_exitApplicationMenuItem);
        
        //Display Menu
        _selectColumnsMenuItem = new JMenuItem("Select Columns ...");
        _selectColumnsMenuItem.setMnemonic('S');
        _displayMenu.add(_selectColumnsMenuItem);
        
        _displayMenu.add(new JSeparator());
        
        _precipThresholdForThreatColorMenuItem = new JMenuItem("Precip Threshold ...");
        _precipThresholdForThreatColorMenuItem.setMnemonic('P');
        _displayMenu.add(_precipThresholdForThreatColorMenuItem);
        
        _showMissingPrecipMenuItem = new JCheckBoxMenuItem("Show Locations with Missing Precip Data");
        _showMissingPrecipMenuItem.setMnemonic('M');
        _displayMenu.add(_showMissingPrecipMenuItem);

        //Config Menu
        _riverMonGroupMenuItem = new JMenuItem("Group Definitions ...");
        _riverMonGroupMenuItem.setMnemonic('G');
        _configMenu.add(_riverMonGroupMenuItem);

        _riverMonLocationMenuItem = new JMenuItem("Location Grouping/Ordering ...");
        _riverMonLocationMenuItem.setMnemonic('L');
        _configMenu.add(_riverMonLocationMenuItem);
        
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
        
        _precipThreatSortMenuItem = new JMenuItem("PrecipThreat Sort");
        _precipThreatSortMenuItem.setMnemonic('P');
        _precipThreatSortMenuItem.setToolTipText("Sort by PrecipThreat");
        _sortMenu.add(_precipThreatSortMenuItem);

      
        
        //Details Memu
        _officeNotesMenuItem = new JMenuItem("Office Notes ...");
        _officeNotesMenuItem.setMnemonic('O');
        _detailsMenu.add(_officeNotesMenuItem);
        
        _alertAlarmMenuItem = new JMenuItem("Alert Alarm ...");
        _alertAlarmMenuItem.setMnemonic('A');
        _detailsMenu.add(_alertAlarmMenuItem);

        
        _detailsMenu.add(new JSeparator());

        
        _timeSeriesLiteMenuItem = new JMenuItem("Time Series Lite...");
        _timeSeriesLiteMenuItem.setMnemonic('T');
        _detailsMenu.add(_timeSeriesLiteMenuItem);

   
        //Help Menu
        _aboutMenuItem = new JMenuItem("About ...");
        _aboutMenuItem.setMnemonic('A');
        _helpMenu.add(_aboutMenuItem);
  
    
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

        ShowMissingPrecipMenuListener showMissingPrecipMenuListener = new 
        ShowMissingPrecipMenuListener();
        _showMissingPrecipMenuItem.addItemListener(showMissingPrecipMenuListener);
        
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
        
        RiverMonitorListener riverMonitorListener = new RiverMonitorListener(); 
        _riverMonitorMenuItem.addActionListener(riverMonitorListener);
        
        AppSortListener appSortListener = new AppSortListener();
        _hsagrplocSortMenuItem.addActionListener(appSortListener);
        
        PrecipThreatSortListener precipThreatSortListener = new PrecipThreatSortListener();
        _precipThreatSortMenuItem.addActionListener(precipThreatSortListener);

        ClearSortListener clearSortListener = new ClearSortListener();
        _clearSortMenuItem.addActionListener(clearSortListener);
        
        AlertAlarmDialogListener alertAlarmDialogListener = new AlertAlarmDialogListener();
        _alertAlarmMenuItem.addActionListener(alertAlarmDialogListener);

        OfficeNotesDialogListener officeNotesDialogListener = new OfficeNotesDialogListener();
        _officeNotesMenuItem.addActionListener(officeNotesDialogListener);

        TimeSeriesLiteMenuItemListener timeSeriesLiteMenuItemListener = new TimeSeriesLiteMenuItemListener();
        _timeSeriesLiteMenuItem.addActionListener(timeSeriesLiteMenuItemListener);

        RiverMonGroupDialogListener riverMonGroupDialogListener = new RiverMonGroupDialogListener();
        _riverMonGroupMenuItem.addActionListener(riverMonGroupDialogListener);

        RiverMonLocationDialogListener riverMonLocationDialogListener = new RiverMonLocationDialogListener();
        _riverMonLocationMenuItem.addActionListener(riverMonLocationDialogListener);

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
            String file = _settingsDir + "/" + PrecipMonitorAppManager.DERIVED_COLUMNS_FILE; 
            launchTextEditor(editorTitle, file, "PrecipMonitor Application");
        }
    }

    private void setFrameListener()
    {
        FrameListener frameListener = new FrameListener();
        _mainFrame.addWindowListener(frameListener);
    }

    public void closeApplication()
    {
        _logger.log("PrecipMonitor Application Exiting....");
        _logger.log("====================================");
        _precipMonitorDataManager.disconnect();
        _mainFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        System.exit(0);
    }

    private void createOfficeNotesDialog()
    {
        List lidList = _precipMonitorDataManager.getLidList();
        String lids[] = new String[lidList.size()];
        for(int i=0; i < lidList.size(); i++)
        {
            lids[i] = lidList.get(i).toString();
        }
        Arrays.sort(lids);
        System.out.println("Create officenotes lids size:"+ lidList.size());
        Map lidDescDetailsMap = _precipMonitorDataManager.getLidDescDetails();
        if(_officeNotesDialog == null)
            _officeNotesDialog = new OfficeNotesDialog(_mainFrame, _officeNotesDataMgr, "RIVERMON", lids, lidDescDetailsMap, _logger);
    }
    
    public void loadSettings()
    {
        String header = "PrecipMonitorMenuManager.loadSettings(): ";

        System.out.print(header);
        send(this, MessageType.LOAD_SETTINGS);
    }

    private void loadOfficeSettings()
    {
        String header = "PrecipMonitorMenuManager.loadOfficeSettings(): ";

        System.out.print(header);
        send(this, MessageType.LOAD_OFFICE_SETTINGS);
    }

    public void viewSelectItem(MonitorMessage message)
    {
        String header = "PrecipMonitorMenuManager.viewSelectItem(): ";
        System.out.println(header +" Invoked"+ "...Source:" + message.getMessageSource() + " Type:" + message.getMessageType());
        _selectedRowData = (PrecipMonitorJTableRowData) message.getMessageData();
    }
    
    public void updateDisplayWithSettings(MonitorMessage message)
    {
        String header = "PrecipMonitorMenuManager.updateDisplayWithSettings(): ";
        System.out.println(header +" Invoked"+ "...Source:" + message.getMessageSource() + " Type:" + message.getMessageType());
        refreshTimeDisplay();
        updateMenuWithNewSettings();
    }

    private void updateMenuWithNewSettings()
    {
        if(_menuSettings.shouldShowMissingPrecip())
            _showMissingPrecipMenuItem.setSelected(true);
        else
            _showMissingPrecipMenuItem.setSelected(false);
    }

    public void saveSettings()
    {
        String header = "PrecipMonitorMenuManager.saveSettings(): ";

        System.out.print(header);
        send(this, MessageType.SAVE_SETTINGS);
    }

    private void saveOfficeSettings()
    {
        String header = "PrecipMonitorMenuManager.saveOfficeSettings(): ";

        System.out.print(header);
        send(this, MessageType.SAVE_OFFICE_SETTINGS);
    }

    public void loadSettings(File fileHandler)
    {
        if(fileHandler != null)
        {
            String header = "PrecipMonitorMenuManager.loadSettings(): ";

            System.out.print(header);
            send(this, MessageType.LOAD_SETTINGS);
        }
    }

    public void loadSettingsFromFile(File fileHandler)
    {
        String header = "PrecipMonitorMenuManager.loadSettingsFromFile(): ";
        if(fileHandler != null)
        {
            _logger.log(header + "Read settings from " +  fileHandler.getAbsolutePath());
            loadSettings(fileHandler);
        }
    }

    private void saveTable()
    {
        String header = "PrecipMonitorMenuManager.saveTable(): ";

        System.out.print(header);
        send(this, MessageType.CREATE_TEXT_FROM_VIEW); 
    }
   
    private void changeColumns()
    {
        String header = "PrecipMonitorMenuManager.changeColumns(): ";

        System.out.print(header);
        send(this, MessageType.CHANGE_COLUMNS);
    }

    private void precipThreatSort()
    {
        String header = "PrecipMonitorMenuManager.precipThreatSort(): ";

        System.out.print(header);
        send(this, MessageType.CLEAR_SORT);
        
        System.out.print(header);
        send(this, MessageType.PRECIP_THREAT_SORT);
    }
    
    private void appSort()
    {
        String header = "PrecipMonitorMenuManager.appSort(): ";

        System.out.print(header);
        send(this, MessageType.CLEAR_SORT);
        
        System.out.print(header);
        send(this, MessageType.APP_SORT);
    }

    private void clearSort()
    {
        String header = "PrecipMonitorMenuManager.clearSort(): ";
        System.out.print(header);
        send(this, MessageType.CLEAR_SORT);
    }
    
    private void createAlertAlarmDialog()
    {
        if(_alertAlarmDialog == null)
            _alertAlarmDialog = new AlertAlarmDialog(_mainFrame, _alertAlarmDataMgr, _logger);
    }
    
   
    private void createRiverMonGroupDialog()
    {
        String defaultHsa = _precipMonitorDataManager.getDefaultHsa();
        if(_riverMonGroupDialog == null)
            _riverMonGroupDialog = new RiverMonGroupDialog(_mainFrame, 
                                    _precipMonitorDataManager.getRiverMonLocGroupDataManager(), _logger, defaultHsa);
    }

    private void createRiverMonLocationDialog()
    {
        if(_riverMonLocationDialog == null)
            _riverMonLocationDialog = new RiverMonLocationDialog(_mainFrame,
                                        _precipMonitorDataManager.getRiverMonLocGroupDataManager(), _logger);
    }

    private class ExitApplicationListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
            closeApplication();
        }
    }

    public class ShowMissingPrecipMenuListener implements ItemListener
    {
        public void itemStateChanged(ItemEvent e)
        {
            String header = "PrecipMonitorMenuManager.ShowMissingPrecipMenuListener.itemStateChanged()";
            
            boolean prevValue = _menuSettings.shouldShowMissingPrecip();
            boolean newValue = false;
            if( e.getStateChange()  == ItemEvent.SELECTED)
            {
                newValue = true;
            }
          
            _menuSettings.setShowMissingPrecip(newValue);

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
    
    private class AppSortListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
            appSort();
        }
    }

    private class RiverMonitorListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
           startMonitor("RiverMonitor Application", "RIVER");          
        }
    }
    
    private class SaveTableListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
           saveTable();          
        }
    }

    private class RefreshMenuListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
            refresh();
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
                _monitorTimeSeriesLiteManager.displayTimeSeriesLite(_mainFrame, _selectedRowData, 1);
            }
            else
            {
                String textMsg = "<HTML><BODY> Unable to display TimeSeriesLite <BR>"+
                "Please highlight a row... </BODY></HTML>";
                JOptionPane.showMessageDialog(_mainFrame, textMsg,"TimeSeriesLite", JOptionPane.PLAIN_MESSAGE);
            }
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
            String message = "<HTML><BODY> This will overwrite standard office settings for PrecipMonitor. <BR>" +
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
    
    private class UpdateDisplayReceiver implements Receiver
    {
        public void receive(MonitorMessage message)
        {
            updateRecordCount();
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
