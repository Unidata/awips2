package ohd.hseb.monitor.river.manager;

import java.io.File;

import javax.swing.JFileChooser;

import ohd.hseb.monitor.MonitorFrame;
import ohd.hseb.monitor.MonitorMessage;
import ohd.hseb.monitor.manager.BaseManager;
import ohd.hseb.monitor.manager.Receiver;
import ohd.hseb.monitor.messaging.MessageSystemManager;
import ohd.hseb.monitor.messaging.MessageType;
import ohd.hseb.monitor.river.settings.RiverMonitorMenuSettings;
import ohd.hseb.monitor.river.settings.RiverMonitorSettingsFileManager;
import ohd.hseb.monitor.settings.FilterSettings;
import ohd.hseb.monitor.settings.ViewSettings;
import ohd.hseb.monitor.util.HydroappsDefaultDirs;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.SessionLogger;

public class RiverMonitorSettingsManager extends BaseManager
{
    private String _settingsDir;
    private AppsDefaults _appsDefaults;
    private SessionLogger _logger;

    private JFileChooser _fileChooser;

    private MonitorFrame _mainFrame;
    private ViewSettings _viewSettings;
    private FilterSettings _filterSettings;
    private RiverMonitorMenuSettings _menuSettings;

    private RiverMonitorSettingsFileManager _riverMonitorSettingsFileManger;
    
    private String _officeSettingsFile ;
    private String _defaultSettingsFile;

    public RiverMonitorSettingsManager(MessageSystemManager msgSystemManager, AppsDefaults appsDefaults, 
            SessionLogger logger, MonitorFrame mainFrame, ViewSettings viewSettings, 
            FilterSettings filterSettings, RiverMonitorMenuSettings menuSettings)
    {
        _appsDefaults = appsDefaults;
        _logger = logger;
        _viewSettings = viewSettings;
        _filterSettings = filterSettings;
        _menuSettings = menuSettings;
        
        _settingsDir = _appsDefaults.getToken("rivermon_config_dir", 
        	HydroappsDefaultDirs.RIVERMON_CONFIG_DIR);

        _riverMonitorSettingsFileManger = new RiverMonitorSettingsFileManager(_logger, _viewSettings, _filterSettings, _menuSettings);

        _mainFrame = mainFrame;
        
        _officeSettingsFile = _settingsDir.concat("/").concat(RiverMonitorAppManager.SITE_RIVER_SETTINGS_FILE);
        _defaultSettingsFile = _settingsDir.concat("/").concat(RiverMonitorAppManager.DEFAULT_RIVER_SETTINGS_FILE);

        setMessageSystemManager(msgSystemManager);

        _msgSystemManager.registerReceiver(new LoadSettingsReceiver(), MessageType.LOAD_SETTINGS);
        _msgSystemManager.registerReceiver(new LoadOfficeSettingsReceiver(), MessageType.LOAD_OFFICE_SETTINGS);
        _msgSystemManager.registerReceiver(new SaveSettingsReceiver(), MessageType.SAVE_SETTINGS);
        _msgSystemManager.registerReceiver(new SaveOfficeSettingsReceiver(), MessageType.SAVE_OFFICE_SETTINGS);
    }

    private void saveSettings(MonitorMessage message)
    {
        String header = "RiverMonitorSettingsManager.saveSettings(): ";
        System.out.println(header +" Invoked"+ "...Source:" + message.getMessageSource() + " Type:" + message.getMessageType());

        File fileHandler = null;
        _fileChooser = new JFileChooser();

        _fileChooser.setCurrentDirectory(new File(_settingsDir));
        _fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

        int result = _fileChooser.showSaveDialog(_mainFrame);
        if(result == JFileChooser.CANCEL_OPTION)
            fileHandler = null;
        else
            fileHandler = _fileChooser.getSelectedFile();

        System.out.print(header);
        saveSettingsToFile(fileHandler);
    }

    private void saveOfficeSettings(MonitorMessage message)
    {
        String header = "RiverMonitorSettingsManager.saveOfficeSettings(): ";
        System.out.println(header +" Invoked"+ "...Source:" + message.getMessageSource() + " Type:" + message.getMessageType());

        File fileHandler = new File(_officeSettingsFile);

        System.out.print(header);
        saveSettingsToFile(fileHandler);
    }

    private void saveSettingsToFile(File fileHandler) 
    {
        send(this, MessageType.UPDATE_SETTINGS);

        if(fileHandler != null)
        {
            _riverMonitorSettingsFileManger.saveSettingsToFile(fileHandler);
        }
    }

    private void loadSettings(MonitorMessage message)
    {
        String header = "RiverMonitorSettingsManager.loadSettings(): ";
        System.out.println(header +" Invoked"+ "...Source:" + message.getMessageSource() + " Type:" + message.getMessageType());

        if(message.getMessageSource() != this)
        {
            File fileHandler = null;
            _fileChooser = new JFileChooser();
            _fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

            _fileChooser.setCurrentDirectory(new File(_settingsDir));
            int result = _fileChooser.showOpenDialog(_mainFrame);

            if(result == JFileChooser.CANCEL_OPTION)
                fileHandler = null;
            else
                fileHandler = _fileChooser.getSelectedFile();

            loadSettings(fileHandler);
        }
    }
    
    public void startRiverMonitor()
    {
        String header = "RiverMonitorSettings.startRiverMonitor(): ";
        File fileHandler = new File(_officeSettingsFile);
        if(fileHandler == null || (! fileHandler.exists() ))
        {
            fileHandler = new File(_defaultSettingsFile);
        }
     
        _logger.log(header + "Trying to load from file :" + fileHandler.getAbsolutePath());
        loadSettings(fileHandler);
    }
    
    private void loadSettings(File fileHandler)
    {
        String header = "RiverMonitorSettingsManager.loadSettings(): ";
        if(fileHandler != null)
        {
            _riverMonitorSettingsFileManger.setSettingsFromFile(fileHandler);
      
            System.out.print(header);
            send(this, MessageType.RELOAD_DATA_WITH_NEW_SETTINGS);
        }
    }

    private void loadOfficeSettings(MonitorMessage message)
    {
        String header = "RiverMonitorSettingsManager.loadOfficeSettings(): ";
        System.out.println(header +" Invoked"+ "...Source:" + message.getMessageSource() + " Type:" + message.getMessageType());

        File fileHandler = new File(_officeSettingsFile);

        loadSettings(fileHandler);
    }

    private class SaveOfficeSettingsReceiver implements Receiver
    {
        public void receive(MonitorMessage message)
        {
            saveOfficeSettings(message);
        }
    }

    private class LoadOfficeSettingsReceiver implements Receiver
    {
        public void receive(MonitorMessage message)
        {
            loadOfficeSettings(message);
        }
    }

    private class LoadSettingsReceiver implements Receiver
    {
        public void receive(MonitorMessage message)
        {
            loadSettings(message);
        }
    }

    private class SaveSettingsReceiver implements Receiver
    {
        public void receive(MonitorMessage message)
        {
            saveSettings(message);
        }
    }


}

