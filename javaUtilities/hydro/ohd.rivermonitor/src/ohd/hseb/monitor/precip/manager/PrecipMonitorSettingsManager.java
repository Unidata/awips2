package ohd.hseb.monitor.precip.manager;

import java.io.File;

import javax.swing.JFileChooser;

import ohd.hseb.monitor.MonitorFrame;
import ohd.hseb.monitor.MonitorMessage;
import ohd.hseb.monitor.manager.BaseManager;
import ohd.hseb.monitor.manager.Receiver;
import ohd.hseb.monitor.messaging.MessageSystemManager;
import ohd.hseb.monitor.messaging.MessageType;
import ohd.hseb.monitor.precip.settings.PrecipMonitorMenuSettings;
import ohd.hseb.monitor.precip.settings.PrecipMonitorSettingsFileManager;
import ohd.hseb.monitor.settings.FilterSettings;
import ohd.hseb.monitor.settings.ViewSettings;
import ohd.hseb.monitor.util.HydroappsDefaultDirs;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.SessionLogger;

public class PrecipMonitorSettingsManager extends BaseManager
{
    private String _settingsDir;
    private AppsDefaults _appsDefaults;
    private SessionLogger _logger;

    private JFileChooser _fileChooser;

    private MonitorFrame _mainFrame;
    private ViewSettings _viewSettings;
    private FilterSettings _filterSettings;
    private PrecipMonitorMenuSettings _menuSettings;

    private PrecipMonitorSettingsFileManager _precipMonitorSettingsFileManger;
    
    private String _officeSettingsFile ;
    private String _defaultSettingsFile;

    public PrecipMonitorSettingsManager(MessageSystemManager msgSystemManager, AppsDefaults appsDefaults, 
            SessionLogger logger, MonitorFrame mainFrame, ViewSettings viewSettings, 
            FilterSettings filterSettings, PrecipMonitorMenuSettings menuSettings)
    {
        _appsDefaults = appsDefaults;
        _logger = logger;
        _viewSettings = viewSettings;
        _filterSettings = filterSettings;
        _menuSettings = menuSettings;
        
        _settingsDir = _appsDefaults.getToken("rivermon_config_dir", 
        	HydroappsDefaultDirs.RIVERMON_CONFIG_DIR);

        _precipMonitorSettingsFileManger = new PrecipMonitorSettingsFileManager(_logger, _viewSettings, _filterSettings, _menuSettings);

        _mainFrame = mainFrame;
        
        _officeSettingsFile = _settingsDir.concat("/").concat(PrecipMonitorAppManager.SITE_PRECIP_SETTINGS_FILE);
        _defaultSettingsFile = _settingsDir.concat("/").concat(PrecipMonitorAppManager.DEFAULT_PRECIP_SETTINGS_FILE);

        setMessageSystemManager(msgSystemManager);

        _msgSystemManager.registerReceiver(new LoadSettingsReceiver(), MessageType.LOAD_SETTINGS);
        _msgSystemManager.registerReceiver(new LoadOfficeSettingsReceiver(), MessageType.LOAD_OFFICE_SETTINGS);
        _msgSystemManager.registerReceiver(new SaveSettingsReceiver(), MessageType.SAVE_SETTINGS);
        _msgSystemManager.registerReceiver(new SaveOfficeSettingsReceiver(), MessageType.SAVE_OFFICE_SETTINGS);
    }

    private void saveSettings(MonitorMessage message)
    {
        String header = "PrecipMonitorSettingsManager.saveSettings(): ";
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
        String header = "PrecipMonitorSettingsManager.saveOfficeSettings(): ";
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
            _precipMonitorSettingsFileManger.saveSettingsToFile(fileHandler);
        }
    }

    private void loadSettings(MonitorMessage message)
    {
        String header = "PrecipMonitorSettingsManager.loadSettings(): ";
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
    
    public void startPrecipMonitor()
    {
        String header = "PrecipMonitorSettings.startPrecipMonitor(): ";
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
        String header = "PrecipMonitorSettingsManager.loadSettings(): ";
        if(fileHandler != null)
        {
            _precipMonitorSettingsFileManger.setSettingsFromFile(fileHandler);
      
            System.out.print(header);
            send(this, MessageType.RELOAD_DATA_WITH_NEW_SETTINGS);
        }
    }

    private void loadOfficeSettings(MonitorMessage message)
    {
        String header = "PrecipMonitorSettingsManager.loadOfficeSettings(): ";
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

