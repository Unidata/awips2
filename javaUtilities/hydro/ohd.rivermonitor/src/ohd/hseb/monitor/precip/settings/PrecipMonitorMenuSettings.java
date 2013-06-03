package ohd.hseb.monitor.precip.settings;

import ohd.hseb.util.BooleanHolder;

public class PrecipMonitorMenuSettings
{
    private int _refreshInterval;
    
    public static final String REFRESH_INTERVAL_TAG = "RefreshInterval";
    
    public static final String SHOW_MISSING_PRECIP_TAG = "ShowMissingPrecip";

    public static int DEFAULT_REFRESH_INTERVAL = 15;
    
    private BooleanHolder _showMissingPrecipBooleanHolder = new BooleanHolder(true);

    private PrecipColumnDataSettings _precipSettings;
    
    public PrecipMonitorMenuSettings()
    {
        _refreshInterval = DEFAULT_REFRESH_INTERVAL;
        setShowMissingPrecip(false);
        _precipSettings = new PrecipColumnDataSettings();
    }
    
    public void resetSettings()
    {
        _refreshInterval = DEFAULT_REFRESH_INTERVAL;
        setShowMissingPrecip(false);
        _precipSettings.resetSettings();
    }
    
    public PrecipMonitorMenuSettings(int refreshInterval, boolean showMissingPrecip)
    {
        _refreshInterval = refreshInterval;
        setShowMissingPrecip(showMissingPrecip);
        _precipSettings = new PrecipColumnDataSettings();
    }
    
    public void setPrecipSettings(PrecipColumnDataSettings precipSettings)
    {
        _precipSettings = precipSettings;
    }
    
    public PrecipColumnDataSettings getPrecipSettings()
    {
        return _precipSettings;
    }
    
    public boolean shouldShowMissingPrecip()
    {
        return  _showMissingPrecipBooleanHolder.getValue();
    }

    public void setShowMissingPrecip(boolean showMissingPrecip)
    {
        _showMissingPrecipBooleanHolder.setValue(showMissingPrecip);
    }

    public void setRefreshInterval(int refreshInterval)
    {
        _refreshInterval = refreshInterval;
    }
    
    public int getRefreshInterval()
    {
        return _refreshInterval;
    }

    private void setShowMissingPrecipBooleanHolder(BooleanHolder showMissingPrecipBooleanHolder)
    {
        _showMissingPrecipBooleanHolder = showMissingPrecipBooleanHolder;
    }

    public BooleanHolder getShowMissingPrecipBooleanHolder()
    {
        return _showMissingPrecipBooleanHolder;
    }
    
    
}
