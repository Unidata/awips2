package ohd.hseb.monitor.manager;

import static ohd.hseb.util.TimeHelper.MILLIS_PER_MINUTE;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

import javax.swing.JButton;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;
import javax.swing.Timer;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.monitor.MonitorFrame;
import ohd.hseb.monitor.messaging.MessageSystemManager;
import ohd.hseb.monitor.messaging.MessageType;
import ohd.hseb.monitor.precip.PrecipThresholdDialog;
import ohd.hseb.monitor.precip.settings.PrecipColumnDataSettings;
import ohd.hseb.monitor.util.HydroappsDefaultDirs;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.SessionLogger;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.DialogHelper;

public abstract class MonitorMenuManager extends BaseManager
{
    private String _version;
    private String _appName;
    private String _date;

    protected MonitorFrame _mainFrame;
    private JButton _refreshNowButton;
    private JLabel _currentTimeLabel;
    private JLabel _nextRefreshTimeLabel;
    protected JSpinner _refreshTimeSpinner;
    protected JPanel _timeDisplayPanel;
    protected JLabel _dataTimeLabel;
    protected JLabel _rowCountLabel;

    private Timer _refreshDisplayTimer = null;

    protected AppsDefaults _appsDefaults;
    protected SessionLogger _logger;
    
    public MonitorMenuManager(MessageSystemManager msgSystemManager, AppsDefaults appsDefaults, 
            SessionLogger logger)
    {
        setMessageSystemManager(msgSystemManager);
        _appsDefaults = appsDefaults;
        _logger = logger;
    }

    protected void setAboutInfo(String version, String date, String appName)
    {
        _version = version;
        _date = date;
        _appName = appName;
    }

    public class AboutListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
            String aboutText = "  Application : "+ _appName + "\n" +
            "  Version       : " + _version + "\n" +
            "  Date             : " + _date + "\n" +
            "  Developed by: \n"+
            "                  National Weather Service\n" +
            "                  Office of Hydrologic Development\n" +
            "                  Hydrology Laboratory";
            DialogHelper.displayMessageDialog(_mainFrame, aboutText, _appName);
        }
    }

    protected boolean confirmSaveOfficeSettings(String message)
    {
           boolean result = false;
           int answer = JOptionPane.showConfirmDialog(_mainFrame, message,"Save Office Settings",JOptionPane.YES_NO_CANCEL_OPTION);
           switch(answer)
           {
               case JOptionPane.YES_OPTION:
                   result = true;
                   break;
               case JOptionPane.NO_OPTION:
               case JOptionPane.CANCEL_OPTION:
               case JOptionPane.CLOSED_OPTION:
                   result = false;
                   break;
           }
           return result ;
    }
    
    protected void startMonitor(String appName, String monitorName)
    {
        Runtime rt = Runtime.getRuntime();
        
        String binDir = _appsDefaults.getToken("whfs_bin_dir", 
        	HydroappsDefaultDirs.WHFS_BIN_DIR);
        String cmd[] = new String[2];
        cmd[0]= binDir + "/start_rivermonitor";

        if(monitorName.equalsIgnoreCase("PRECIP"))
        {
            cmd[1] = "Precip";
        }
        else //RIVER
        {
            cmd[1] = "River";
        }

        try
        {
            _mainFrame.setWaitCursor();
            rt.exec(cmd);
            _logger.log("Launch " + monitorName + " Monitor [cmd:"+ cmd[0]+" "+cmd[1] +"]");
            _mainFrame.setDefaultCursor();
        }
        catch(IOException ex)
        {
            _mainFrame.setDefaultCursor();
            String str = "Unable to launch " + monitorName + " Monitor:[cmd"+  cmd[0]+" "+cmd[1] + "] Exception:"+ ex;
            JOptionPane.showMessageDialog(_mainFrame, str,appName, JOptionPane.PLAIN_MESSAGE);
        }
    }

    protected void launchTextEditor(String editorTitle, String file, String appName)
    {
        String editorDir = _appsDefaults.getToken("whfs_local_bin_dir", 
        	HydroappsDefaultDirs.WHFS_LOCAL_BIN_DIR);
        String editor = _appsDefaults.getToken("whfs_editor", "whfs_editor");

        if(editorDir == null)
            editorDir =".";
        if(editor == null)
            editor = "whfs_editor";
        String script = editorDir.concat("/").concat(editor);

        Runtime rt = Runtime.getRuntime();
        String cmd[] = new String[3];
        cmd[0] = script;
        cmd[1] = editorTitle;
        cmd[2] = file;
        try
        {
            _mainFrame.setWaitCursor();
            rt.exec(cmd);
            _logger.log("Launch Text Editor [cmd:"+ cmd[0]+" "+cmd[1]+" "+cmd[2]+"]");
            _mainFrame.setDefaultCursor();
        }
        catch(IOException ex)
        {
            _mainFrame.setDefaultCursor();
            String str = "Unable to launch text editor:[cmd"+  cmd[0]+" "+cmd[1]+" "+cmd[2]+ "] Exception:"+ ex;
            JOptionPane.showMessageDialog(_mainFrame, str,appName, JOptionPane.PLAIN_MESSAGE);
        }

    }

    protected void createRefreshComponents(int refreshInterval, JPanel timeDisplayPanel)
    {
        _timeDisplayPanel = timeDisplayPanel;

        JLabel refreshLabel = new JLabel("Update(in mins)");
        int maxValueForRefreshMins = 30;
        int minValueForRefreshMins = 1;
        int incrValueForRefreshMins = 1;
        int initialValueForRefreshMins = refreshInterval;

        SpinnerModel spinnerModel = new SpinnerNumberModel(initialValueForRefreshMins,
            minValueForRefreshMins,
            maxValueForRefreshMins,
            incrValueForRefreshMins);
        _refreshTimeSpinner = new JSpinner(spinnerModel);
        _refreshTimeSpinner.setPreferredSize(new Dimension(35,20));
        // Disable keyboard edits in the spinner
        JFormattedTextField textField =
            ((JSpinner.DefaultEditor)_refreshTimeSpinner.getEditor()).getTextField();
        textField.setEditable(false);
        _refreshTimeSpinner.addChangeListener(new RefreshTimeSpinBoxListener());

        _timeDisplayPanel.setLayout(new GridBagLayout());
        _currentTimeLabel = new JLabel();
        _currentTimeLabel.setBackground(Color.RED);

        _nextRefreshTimeLabel = new JLabel();
        _nextRefreshTimeLabel.setBackground(Color.GREEN);
        JPanel nextRefreshTimePanel = new JPanel();
        nextRefreshTimePanel.add(_nextRefreshTimeLabel);

        JPanel refreshPanel = new JPanel();
        _refreshNowButton = new JButton("Update Now");
        _refreshNowButton.setPreferredSize(new Dimension(110,20));
        RefreshButtonListener refreshDisplayTimerListener = new RefreshButtonListener();
        _refreshNowButton.addActionListener(refreshDisplayTimerListener);
        refreshPanel.add(refreshLabel);
        refreshPanel.add(_refreshTimeSpinner);
        refreshPanel.add(_refreshNowButton);
        
        _dataTimeLabel = new JLabel();
        _dataTimeLabel.setText("DEFAULT TEXT");
        
        _rowCountLabel = new JLabel();
   
        JPanel dummyPanel1 = new JPanel();
        JPanel dummyPanel1_25 = new JPanel();
        JPanel dummyPanel15 = new JPanel();
        JPanel dummyPanel2 = new JPanel();
        JPanel dummyPanel3 = new JPanel();
        dummyPanel1.setPreferredSize(new Dimension(50,20));
        dummyPanel1_25.setPreferredSize(new Dimension(50,20)); 
        dummyPanel15.setPreferredSize(new Dimension(50,20)); 
        dummyPanel2.setPreferredSize(new Dimension(60,20));
        dummyPanel3.setPreferredSize(new Dimension(50,20));

        //col, row, width, height, weightx, weighty, fill
        ComponentHelper.addFrameComponent(_timeDisplayPanel, 
            dummyPanel1,          0,  0,  1,  1,  0,  0,  1);
        ComponentHelper.addFrameComponent(_timeDisplayPanel, 
                _rowCountLabel,   1,  0,  1,  1,  0,  0,  1);  
        ComponentHelper.addFrameComponent(_timeDisplayPanel, 
                dummyPanel1_25,   2,  0,  1,  1,  0,  0,  1);
        ComponentHelper.addFrameComponent(_timeDisplayPanel, 
               _dataTimeLabel,    3,  0,  1,  1,  0,  0,  1);  
        ComponentHelper.addFrameComponent(_timeDisplayPanel, 
                dummyPanel15,     4,  0,  1,  1,  0,  0,  1);
        ComponentHelper.addFrameComponent(_timeDisplayPanel, 
            _currentTimeLabel,    5,  0,  1,  1,  0,  0,  1);
        ComponentHelper.addFrameComponent(_timeDisplayPanel, 
            dummyPanel2,          6,  0,  1,  1,  0,  0,  1);
        ComponentHelper.addFrameComponent(_timeDisplayPanel, 
            _nextRefreshTimeLabel,7,  0,  1,  1,  0,  0,  1);
        ComponentHelper.addFrameComponent(_timeDisplayPanel, 
            dummyPanel3,          8,  0,  1,  1,  0,  0,  1);
        ComponentHelper.addFrameComponent(_timeDisplayPanel, 
            refreshPanel,         9,  0,  1,  1,  0,  0,  1);
        
        
     }
    
    public void setInitialRefreshTimeInfo()
    {
        setRefreshTimeSpinnerValue();

         
        String curDateTimeString = DbTimeHelper.getDateTimeStringFromLongTime(System.currentTimeMillis());
        _currentTimeLabel.setText(" Last Update:  "+ curDateTimeString);

        Integer value = Integer.parseInt(_refreshTimeSpinner.getValue().toString());
        int refreshMilliSecs = (int) (value * MILLIS_PER_MINUTE);
        System.out.println("Interval :"+ value);

        String nextDateTimeString = DbTimeHelper.getDateTimeStringFromLongTime(System.currentTimeMillis() + refreshMilliSecs);
        String timeString[] = nextDateTimeString.split(" ");
        _nextRefreshTimeLabel.setText(" Next Update:  "+timeString[1]);
        
        //time of the PDC file read
        _dataTimeLabel.setText("Precip Data Time: N/A");
            
        updateRecordCount();
        
    }
    
    protected void updateRecordCount()
    {
        long recordCount = getDisplayedRecordCount();
        _rowCountLabel.setText(recordCount + " Locations");
    }

    private void changeTimeInformationDisplayed(long refreshMilliSecs, boolean changeLastUpdateTime)
    {
        long currentTime = System.currentTimeMillis();
        
        if(changeLastUpdateTime)
        {
            String curDateTimeString = DbTimeHelper.getTimeToSecondsStringFromLongTime(currentTime);
            _currentTimeLabel.setText(" Last Update:  "+ curDateTimeString);
        }
        String nextDateTimeString = DbTimeHelper.getTimeToSecondsStringFromLongTime(currentTime + refreshMilliSecs);
      //  String timeString[] = nextDateTimeString.split(" ");
        _nextRefreshTimeLabel.setText(" Next Update:  " + nextDateTimeString);
        
        //time of the PDC file read
        
        long dataUpdateTime = getDataUpdateTime();
        _dataTimeLabel.setText("Precip Data Time: " + DbTimeHelper.getTimeToSecondsStringFromLongTime(dataUpdateTime));
        
        
        //updateRecordCount();
   
    }
    
    protected abstract long getDataUpdateTime();
    protected abstract long getDisplayedRecordCount();

    public abstract void setMenuSettingsRefreshInterval(int value);

    // Refresh time spin box listener
    private class RefreshTimeSpinBoxListener implements ChangeListener
    {
        public void stateChanged(ChangeEvent e) 
        {
            Integer value = Integer.parseInt(_refreshTimeSpinner.getValue().toString());
            setMenuSettingsRefreshInterval(value);
            int refreshMilliSecs = (int) (value * MILLIS_PER_MINUTE); 
            changeTimeInformationDisplayed(refreshMilliSecs, false);
            startTimer(refreshMilliSecs);
        }
    }

    // Is attached to the update now button
    private class RefreshButtonListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
            refresh();  
        }
    }

    protected void startTimer(int delayMilliSecs)
    {
        if(_refreshDisplayTimer == null)
        {
            RefreshButtonListener refreshDisplayTimerListener = new RefreshButtonListener();
            _refreshDisplayTimer = new Timer(delayMilliSecs, refreshDisplayTimerListener);
        }
        else
        {
            _refreshDisplayTimer.stop();
            //This timer doesn't repeat, hence it always uses setInitialDelay and not setDelay
            //which is used only if the timer is set to repeat
            _refreshDisplayTimer.setInitialDelay(delayMilliSecs); 
        }
        _refreshDisplayTimer.start();
    }

    public abstract void setRefreshTimeSpinnerValue();

    protected void refreshTimeDisplay()
    {
        Object obj = _refreshTimeSpinner.getValue();
        int refreshMilliSecs =(int) (Integer.parseInt(obj.toString()) * MILLIS_PER_MINUTE); 
        startTimer(refreshMilliSecs);

        setRefreshTimeSpinnerValue();

        changeTimeInformationDisplayed(refreshMilliSecs, true);
        startTimer(refreshMilliSecs);
    }
    
    protected void refresh()
    {
        String header = "MonitorMenuManager.refresh(): ";
       
        _mainFrame.setWaitCursor();
        
        System.out.print(header);
        send(this, MessageType.RELOAD_DATA);
        refreshTimeDisplay();
        
        _mainFrame.setDefaultCursor();
    }
    
    protected void sendMissingFilterChanged()
    {
        String header = "MonitorMenuManager.sendMissingFilterChanged(): ";

        System.out.print(header);
        send(this, MessageType.REFRESH_DISPLAY);
        
    }
    
    protected void menuChange()
    {
        String header = "MonitorMenuManager.menuChange(): ";

        System.out.print(header);
        send(this, MessageType.RELOAD_DATA);
        refreshTimeDisplay();
    }

    protected class PrecipThresholdListener implements ActionListener
    {
        private PrecipColumnDataSettings _currentMenuSettings;
       
        
        public PrecipThresholdListener(PrecipColumnDataSettings currentMenuSettings)
        {
            String header = "PrecipThresholdListener.PrecipThresholdListener()";
            _currentMenuSettings = currentMenuSettings;
            System.out.println(header +"Current Settings: "  + _currentMenuSettings);
        }

        public void actionPerformed(ActionEvent e)
        {
            PrecipThresholdDialog precipThresholdDialog = new PrecipThresholdDialog(_mainFrame, _currentMenuSettings, _logger);
            
            String header = "PrecipThresholdListener.actionPerformed()";
          //  PrecipColumnDataSettings currentMenuSettings = _menuSettings.getPrecipSettings();
          
            System.out.println(header +"Before show Current Settings: "  + _currentMenuSettings);
            
            PrecipColumnDataSettings savedPrecipSettings = precipThresholdDialog.showPrecipThresholdDialog();
            if(savedPrecipSettings != null)
            {
                _currentMenuSettings.setPrecipSettings(savedPrecipSettings);
                System.out.println("Im here menu had changed");
                menuChange();
            }
            else
            {
                System.out.println("Im here menu had not changed");
            }
            System.out.println(header +"After show Current Settings: "  + _currentMenuSettings);
        }
    }

}
