package ohd.hseb.monitor.river.settings;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import ohd.hseb.monitor.precip.settings.PrecipColumnDataSettings;
import ohd.hseb.monitor.settings.FilterSettings;
import ohd.hseb.monitor.settings.MonitorSettingsFileManager;
import ohd.hseb.monitor.settings.ViewSettings;
import ohd.hseb.util.SessionLogger;

public class RiverMonitorSettingsFileManager extends MonitorSettingsFileManager
{
    private RiverMonitorMenuSettings _menuSettings;
    private Map<String, Object> _filterTokenValuesMap;

    public RiverMonitorSettingsFileManager(SessionLogger logger, ViewSettings viewSettings,
            FilterSettings filterSettings, RiverMonitorMenuSettings menuSettings)
    {
        super(logger, "#RiverMonitorSettings File", viewSettings, filterSettings);
        _menuSettings = menuSettings;
        _logger = logger;
        createDisplayTokenSet();
        createMenuTokenSet();
    }

    public Set createMenuTokenSet()
    {
        _menuTokenSet = new HashSet<String>();

        _menuTokenSet.add(RiverColumnDataSettings.FCST_TS_TAG);
        _menuTokenSet.add(RiverColumnDataSettings.LATEST_FCST_BASIS_TIME_TAG);
        _menuTokenSet.add(RiverColumnDataSettings.LATEST_OBS_TIME_TAG);
        _menuTokenSet.add(RiverColumnDataSettings.UGC_EXPIRE_TIME_TAG);
        _menuTokenSet.add(RiverColumnDataSettings.VALID_TIME_TAG);
        _menuTokenSet.add(RiverColumnDataSettings.VTEC_EVENT_END_TIME_TAG);
        _menuTokenSet.add(RiverColumnDataSettings.VTEC_EVENT_PRODUCT_TIME_TAG);
        _menuTokenSet.add(RiverMonitorMenuSettings.REFRESH_INTERVAL_TAG);
        _menuTokenSet.add(RiverMonitorMenuSettings.SHOW_MISSING_RIVER_TAG);
        _menuTokenSet.add(PrecipColumnDataSettings.DIFF_ALERT_TAG);
        _menuTokenSet.add(PrecipColumnDataSettings.DIFF_CAUTION_TAG);
        _menuTokenSet.add(PrecipColumnDataSettings.PRECIP_ALERT_TAG);
        _menuTokenSet.add(PrecipColumnDataSettings.PRECIP_CAUTION_TAG);
        _menuTokenSet.add(PrecipColumnDataSettings.RATIO_ALERT_TAG);
        _menuTokenSet.add(PrecipColumnDataSettings.RATIO_CAUTION_TAG);

        return _displayTokenSet;
    }

    public boolean setSettingsFromFile(File fileHandler)
    {
        String header = "RiverMonitorSettingsFileManager.setSettingsFromFile(): ";
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
        setRiverSettings();
        return isReadCompleted;
    }

    private void resetSettings()
    {
        _tableDisplaySettingsList = null;
        _expandPathTokenValuesList = null;
        _pathTokenValuesList = null;
        _filterTokenValuesMap = null;

        setViewAndFilterSettings();
        _menuSettings.resetSettings();
    }

    private void setRiverSettings()
    {

        if(_filterTokenValuesMap != null)
        {
            Object object = _filterTokenValuesMap.get(RiverColumnDataSettings.FCST_TS_TAG);
            if(object != null)
            {
                String fcstTypeSource = _filterTokenValuesMap.get(RiverColumnDataSettings.FCST_TS_TAG).toString();
                _menuSettings.getRiverSettings().setFcstTypeSource(fcstTypeSource);
            }

            object = _filterTokenValuesMap.get(RiverColumnDataSettings.VALID_TIME_TAG);
            if(object != null)
            {
                Integer alerAlarmLoookback = new Integer(object.toString());
                _menuSettings.getRiverSettings().setAlertAlarmLookBack(alerAlarmLoookback);
            }

            object = _filterTokenValuesMap.get(RiverColumnDataSettings.LATEST_OBS_TIME_TAG);
            if(object != null)
            {
                Integer latestObsLookback = new Integer(_filterTokenValuesMap.get(RiverColumnDataSettings.LATEST_OBS_TIME_TAG).toString());
                _menuSettings.getRiverSettings().setLatestObsLookBack(latestObsLookback);
            }

            object = _filterTokenValuesMap.get(RiverColumnDataSettings.LATEST_FCST_BASIS_TIME_TAG);
            if(object != null)
            {
                Integer latestFcstBasisTime = new Integer(_filterTokenValuesMap.get(RiverColumnDataSettings.LATEST_FCST_BASIS_TIME_TAG).toString());
                _menuSettings.getRiverSettings().setLatestFcstBasisTimeLookBack(latestFcstBasisTime);
            }

            object = _filterTokenValuesMap.get(RiverColumnDataSettings.VTEC_EVENT_PRODUCT_TIME_TAG);
            if(object != null)
            {
                Integer vtecEventProductTimeLookback = new Integer(_filterTokenValuesMap.get(RiverColumnDataSettings.VTEC_EVENT_PRODUCT_TIME_TAG).toString());
                _menuSettings.getRiverSettings().setVtecEventProductTimeLookBack(vtecEventProductTimeLookback);
            }

            object = _filterTokenValuesMap.get(RiverColumnDataSettings.VTEC_EVENT_END_TIME_TAG);
            if(object != null)
            {
                Integer vtecEventEndTimeApproach = new Integer(_filterTokenValuesMap.get(RiverColumnDataSettings.VTEC_EVENT_END_TIME_TAG).toString());
                _menuSettings.getRiverSettings().setVtecEventEndTimeApproach(vtecEventEndTimeApproach);
            }

            object = _filterTokenValuesMap.get(RiverColumnDataSettings.UGC_EXPIRE_TIME_TAG);
            if(object != null)
            {
                Integer ugcExpireTimeApproach = new Integer(_filterTokenValuesMap.get(RiverColumnDataSettings.UGC_EXPIRE_TIME_TAG).toString());
                _menuSettings.getRiverSettings().setUgcExpireTimeApproach(ugcExpireTimeApproach);
            }
        }
    }

    public void saveSettingsToFile(File fileHandler)
    {
        createSettingsFile(fileHandler);

        BufferedWriter out = null;
        String header = "RiverMonitorSettingsFileManager.saveSettingsToFile(): ";
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

    private void processPrecipToken(String tokenName, String tokenValue)
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

    public void processMenuToken(String tokenName, String tokenValue)
    {
        if(tokenName.compareTo(RiverMonitorMenuSettings.REFRESH_INTERVAL_TAG) == 0)
        {
            _menuSettings.setRefreshInterval(new Integer(tokenValue));
        }
        if(tokenName.compareTo(RiverMonitorMenuSettings.SHOW_MISSING_RIVER_TAG) == 0)
        {
            _menuSettings.setShowMissingRiver(new Boolean(tokenValue));
        }
        else if( (tokenName.equals(PrecipColumnDataSettings.DIFF_ALERT_TAG) || 
                tokenName.equals(PrecipColumnDataSettings.DIFF_CAUTION_TAG) ||
                tokenName.equals(PrecipColumnDataSettings.PRECIP_ALERT_TAG) ||
                tokenName.equals(PrecipColumnDataSettings.PRECIP_CAUTION_TAG) ||
                tokenName.equals(PrecipColumnDataSettings.RATIO_ALERT_TAG) ||
                tokenName.equals(PrecipColumnDataSettings.RATIO_CAUTION_TAG) ))
        {
            processPrecipToken(tokenName, tokenValue);
        }
        else
        {
            if(_filterTokenValuesMap == null)
            {
                _filterTokenValuesMap = new HashMap<String, Object>();
            }

            if(tokenValue.length() > 0 && tokenValue.charAt(0) != '\n')
            {
                _filterTokenValuesMap.put(tokenName, tokenValue);
            }
        }
    }

    public void saveMenuSettingsToFile(BufferedWriter writer)
    {
        String header = "SettingsFileManager.saveMenuSettingsToFile(): ";
        try
        {
            if(_menuSettings != null)
            {
                writer.newLine();
                writer.write(RiverMonitorMenuSettings.REFRESH_INTERVAL_TAG + " : " + _menuSettings.getRefreshInterval());
                writer.newLine();
                
                writer.newLine();
                writer.write(RiverMonitorMenuSettings.SHOW_MISSING_RIVER_TAG + " : " + _menuSettings.shouldShowMissingRiver());
                writer.newLine();

                writer.newLine();
                writer.write(RiverColumnDataSettings.RIVER_SETTINGS_BEGIN_TAG);
                writer.newLine();
                writer.write(RiverColumnDataSettings.FCST_TS_TAG + " : " + _menuSettings.getRiverSettings().getFcstTypeSource());
                writer.newLine();
                writer.write(RiverColumnDataSettings.LATEST_FCST_BASIS_TIME_TAG + " : " + _menuSettings.getRiverSettings().getLatestFcstBasisTimeLookBack());
                writer.newLine();
                writer.write(RiverColumnDataSettings.LATEST_OBS_TIME_TAG + " : " + _menuSettings.getRiverSettings().getLatestObsLookBack());
                writer.newLine();
                writer.write(RiverColumnDataSettings.VALID_TIME_TAG + " : " + _menuSettings.getRiverSettings().getAlertAlarmLookBack());
                writer.newLine();
                writer.write(RiverColumnDataSettings.VTEC_EVENT_PRODUCT_TIME_TAG + " : " + _menuSettings.getRiverSettings().getVtecEventProductTimeLookBack());
                writer.newLine();
                writer.write(RiverColumnDataSettings.VTEC_EVENT_END_TIME_TAG + " : " + _menuSettings.getRiverSettings().getVtecEventEndTimeApproach());
                writer.newLine();
                writer.write(RiverColumnDataSettings.UGC_EXPIRE_TIME_TAG + " : " + _menuSettings.getRiverSettings().getUgcExpireTimeApproach());
                writer.newLine();
                writer.write(RiverColumnDataSettings.RIVER_SETTINGS_END_TAG);
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