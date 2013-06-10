package ohd.hseb.monitor.precip.settings;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import ohd.hseb.monitor.settings.FilterSettings;
import ohd.hseb.monitor.settings.MonitorSettingsFileManager;
import ohd.hseb.monitor.settings.ViewSettings;
import ohd.hseb.util.SessionLogger;

public class PrecipMonitorSettingsFileManager extends MonitorSettingsFileManager
{
    private PrecipMonitorMenuSettings _menuSettings;
    
    public PrecipMonitorSettingsFileManager(SessionLogger logger, ViewSettings viewSettings,
                                    FilterSettings filterSettings, PrecipMonitorMenuSettings menuSettings)
    {
        super(logger, "#PrecipMonitorSettings File", viewSettings, filterSettings);
        _menuSettings = menuSettings;
        _logger = logger;
        createDisplayTokenSet();
        createMenuTokenSet();
    }

    public Set createMenuTokenSet()
    {
        _menuTokenSet = new HashSet<String>();

        _menuTokenSet.add(PrecipColumnDataSettings.DIFF_ALERT_TAG);
        _menuTokenSet.add(PrecipColumnDataSettings.DIFF_CAUTION_TAG);
        _menuTokenSet.add(PrecipColumnDataSettings.PRECIP_ALERT_TAG);
        _menuTokenSet.add(PrecipColumnDataSettings.PRECIP_CAUTION_TAG);
        _menuTokenSet.add(PrecipColumnDataSettings.RATIO_ALERT_TAG);
        _menuTokenSet.add(PrecipColumnDataSettings.RATIO_CAUTION_TAG);
        _menuTokenSet.add(PrecipMonitorMenuSettings.REFRESH_INTERVAL_TAG);
        _menuTokenSet.add(PrecipMonitorMenuSettings.SHOW_MISSING_PRECIP_TAG);

        return _displayTokenSet;
    }

    public boolean setSettingsFromFile(File fileHandler)
    {
        String header = "PrecipMonitorSettingsFileManager.setSettingsFromFile(): ";
        boolean isReadCompleted = true;
        
        resetSettings();
        
        if(isValidSettingsFile(_logger, fileHandler, _settingsFileBeginTag ))
        {
            BufferedReader in = null;
            String line;
            try
            {
                in = new BufferedReader(new FileReader(fileHandler));

                while(( line = in.readLine()) != null)
                {
                    line = line.trim();
                    processLine(line);
                }

            }
            catch(IOException e)
            {
                printFileException(_logger, e,header);       
                isReadCompleted = false ;
            }
        }
        else
        {
            isReadCompleted = false;
        }

        setViewAndFilterSettings();
        return isReadCompleted;
    }
    
    private void resetSettings()
    {
        _tableDisplaySettingsList = null;
        _expandPathTokenValuesList = null;
        _pathTokenValuesList = null;
        
        setViewAndFilterSettings();
        _menuSettings.resetSettings();
    }


    public void saveSettingsToFile(File fileHandler)
    {
        createSettingsFile(fileHandler);

        BufferedWriter out = null;
        String header = "PrecipMonitorSettingsFileManager.saveSettingsToFile(): ";
        try
        {
            out = new BufferedWriter(new FileWriter(fileHandler));
        }
        catch(IOException exception)
        {
            _logger.log(header+"ERROR = " + exception.getMessage());
            exception.printStackTrace(_logger.getPrintWriter());     
        }

        saveDisplaySettings(out);

        saveMenuSettingsToFile(out);
    } 

    public void processMenuToken(String tokenName, String tokenValue)
    {
        if(tokenName.compareTo(PrecipMonitorMenuSettings.REFRESH_INTERVAL_TAG) == 0)
        {
            _menuSettings.setRefreshInterval(new Integer(tokenValue));
            System.out.println("Setting in menu refresh  interval:" + tokenValue);
        }
        if(tokenName.compareTo(PrecipMonitorMenuSettings.SHOW_MISSING_PRECIP_TAG) == 0)
        {
            _menuSettings.setShowMissingPrecip(new Boolean(tokenValue));
            System.out.println("Setting Show Missing Precip Flag:" + tokenValue);
        }
        else
        {
            String spiltStr[] = tokenValue.split("\\|");
            String hr1, hr3, hr6;

            if(spiltStr != null)
            {
                if(spiltStr.length == 3)  // we have 3 values 1hr | 3hr | 6hr correctly
                {
                    hr1 = null; hr3 = null; hr6 = null;

                    hr1 = spiltStr[0].trim();
                    hr3 = spiltStr[1].trim();
                    hr6 = spiltStr[2].trim();

                    if(tokenName.compareTo(PrecipColumnDataSettings.PRECIP_ALERT_TAG) == 0)
                    {
                        _menuSettings.getPrecipSettings().setPrecipAlert1Hr(new Double(hr1));
                        _menuSettings.getPrecipSettings().setPrecipAlert3Hr(new Double(hr3));
                        _menuSettings.getPrecipSettings().setPrecipAlert6Hr(new Double(hr6));
                    }
                    else if(tokenName.compareTo(PrecipColumnDataSettings.PRECIP_CAUTION_TAG) == 0)
                    {
                        _menuSettings.getPrecipSettings().setPrecipCaution1Hr(new Double(hr1));
                        _menuSettings.getPrecipSettings().setPrecipCaution3Hr(new Double(hr3));
                        _menuSettings.getPrecipSettings().setPrecipCaution6Hr(new Double(hr6));
                    }
                    else if(tokenName.compareTo(PrecipColumnDataSettings.RATIO_ALERT_TAG) == 0)
                    {
                        _menuSettings.getPrecipSettings().setRatioAlert1Hr(new Integer(hr1));
                        _menuSettings.getPrecipSettings().setRatioAlert3Hr(new Integer(hr3));
                        _menuSettings.getPrecipSettings().setRatioAlert6Hr(new Integer(hr6));
                    }
                    else if(tokenName.compareTo(PrecipColumnDataSettings.RATIO_CAUTION_TAG) == 0)
                    {
                        _menuSettings.getPrecipSettings().setRatioCaution1Hr(new Integer(hr1));
                        _menuSettings.getPrecipSettings().setRatioCaution3Hr(new Integer(hr3));
                        _menuSettings.getPrecipSettings().setRatioCaution6Hr(new Integer(hr6));
                    }
                    else if(tokenName.compareTo(PrecipColumnDataSettings.DIFF_ALERT_TAG) == 0)
                    {
                        _menuSettings.getPrecipSettings().setDiffAlert1Hr(new Double(hr1));
                        _menuSettings.getPrecipSettings().setDiffAlert3Hr(new Double(hr3));
                        _menuSettings.getPrecipSettings().setDiffAlert6Hr(new Double(hr6));
                    }
                    else if(tokenName.compareTo(PrecipColumnDataSettings.DIFF_CAUTION_TAG) == 0)
                    {
                        _menuSettings.getPrecipSettings().setDiffCaution1Hr(new Double(hr1));
                        _menuSettings.getPrecipSettings().setDiffCaution3Hr(new Double(hr3));
                        _menuSettings.getPrecipSettings().setDiffCaution6Hr(new Double(hr6));
                    }
                }
            }
        }
    }

    public void saveMenuSettingsToFile(BufferedWriter writer)
    {
        String header = "PrecipMonitorSettingsFileManager.saveMenuSettingsToFile(): ";
        try
        {
            if(_menuSettings != null)
            {
                writer.newLine();
                writer.write(PrecipMonitorMenuSettings.REFRESH_INTERVAL_TAG + " : " + _menuSettings.getRefreshInterval());
                writer.newLine();

                writer.newLine();
                writer.write(PrecipMonitorMenuSettings.SHOW_MISSING_PRECIP_TAG + " : " + _menuSettings.shouldShowMissingPrecip());
                writer.newLine();
                
                writer.newLine();
                writer.write(PrecipColumnDataSettings.PRECIP_SETTINGS_BEGIN_TAG);
                writer.newLine();
                writer.write(PrecipColumnDataSettings.PRECIP_ALERT_TAG + " : " + _menuSettings.getPrecipSettings().getPrecipAlert1Hr() +" | " + 
                    _menuSettings.getPrecipSettings().getPrecipAlert3Hr() +" | " +
                    _menuSettings.getPrecipSettings().getPrecipAlert6Hr()  );
                writer.newLine();
                writer.write(PrecipColumnDataSettings.RATIO_ALERT_TAG + " : " + _menuSettings.getPrecipSettings().getRatioAlert1Hr() +" | " + 
                    _menuSettings.getPrecipSettings().getRatioAlert3Hr() +" | " +
                    _menuSettings.getPrecipSettings().getRatioAlert6Hr() );
                writer.newLine();
                writer.write(PrecipColumnDataSettings.DIFF_ALERT_TAG + " : " + _menuSettings.getPrecipSettings().getDiffAlert1Hr() +" | " + 
                    _menuSettings.getPrecipSettings().getDiffAlert3Hr() +" | " +
                    _menuSettings.getPrecipSettings().getDiffAlert6Hr()  );
                writer.newLine();

                writer.write(PrecipColumnDataSettings.PRECIP_CAUTION_TAG + " : " + _menuSettings.getPrecipSettings().getPrecipCaution1Hr() +" | " + 
                    _menuSettings.getPrecipSettings().getPrecipCaution3Hr() +" | " +
                    _menuSettings.getPrecipSettings().getPrecipCaution6Hr() );
                writer.newLine();
                writer.write(PrecipColumnDataSettings.RATIO_CAUTION_TAG + " : " + _menuSettings.getPrecipSettings().getRatioCaution1Hr() +" | " + 
                    _menuSettings.getPrecipSettings().getRatioCaution3Hr() +" | " +
                    _menuSettings.getPrecipSettings().getRatioCaution6Hr()  );
                writer.newLine();
                writer.write(PrecipColumnDataSettings.DIFF_CAUTION_TAG + " : " + _menuSettings.getPrecipSettings().getDiffCaution1Hr() +" | " + 
                    _menuSettings.getPrecipSettings().getDiffCaution3Hr() +" | " +
                    _menuSettings.getPrecipSettings().getDiffCaution6Hr()  );
                writer.newLine();
                writer.write(PrecipColumnDataSettings.PRECIP_SETTINGS_END_TAG);

                writer.close();
            }
        }
        catch(IOException exception)
        {
            printFileException(_logger, exception,header);            
        }

    }



}
