package ohd.hseb.monitor.river.settings;

import ohd.hseb.monitor.precip.settings.PrecipColumnDataSettings;
import ohd.hseb.util.BooleanHolder;

public class RiverMonitorMenuSettings
{
  private int _refreshInterval;
    
    public static final String REFRESH_INTERVAL_TAG = "RefreshInterval";
    
    public static final String SHOW_MISSING_RIVER_TAG = "ShowMissingRiver";

    public static int DEFAULT_REFRESH_INTERVAL = 15;

    private PrecipColumnDataSettings _precipSettings;
    private RiverColumnDataSettings _riverSettings;
    
  //  public boolean _showMissingRiver;
    
    private BooleanHolder _showMissingDataBooleanHolder = new BooleanHolder(true);

    
    public RiverMonitorMenuSettings()
    {
        _refreshInterval = DEFAULT_REFRESH_INTERVAL;
        setShowMissingRiver(false);
        _precipSettings = new PrecipColumnDataSettings();
        _riverSettings = new RiverColumnDataSettings();
    }
    
    public void resetSettings()
    {
        _refreshInterval = DEFAULT_REFRESH_INTERVAL;
        setShowMissingRiver(false);
        _precipSettings.resetSettings();
        _riverSettings.resetSettings();
    }
    
    public RiverMonitorMenuSettings(int refreshInterval, boolean showMissingPrecip)
    {
        _refreshInterval = refreshInterval;
        setShowMissingRiver(showMissingPrecip);
        _precipSettings = new PrecipColumnDataSettings();
        _riverSettings = new RiverColumnDataSettings();
    }
    
    public void setPrecipSettings(PrecipColumnDataSettings precipSettings)
    {
        _precipSettings = precipSettings;
    }
    
    public PrecipColumnDataSettings getPrecipSettings()
    {
        return _precipSettings;
    }
    
    public void setRiverSettings(RiverColumnDataSettings riverSettings)
    {
        _riverSettings = riverSettings;
    }
    
    public RiverColumnDataSettings getRiverSettings()
    {
        return _riverSettings;
    }
    
    public void setRefreshInterval(int refreshInterval)
    {
        _refreshInterval = refreshInterval;
    }
    
    public int getRefreshInterval()
    {
        return _refreshInterval;
    }
    
    public boolean shouldShowMissingRiver()
    {
        return  getShowMissingDataBooleanHolder().getValue();
    }

    public void setShowMissingRiver(boolean showMissingRiver)
    {
        _showMissingDataBooleanHolder.setValue(showMissingRiver);
    }
    
    private void setShowMissingDataBooleanHolder(BooleanHolder showMissingPrecipBooleanHolder)
    {
        _showMissingDataBooleanHolder = showMissingPrecipBooleanHolder;
    }

    public BooleanHolder getShowMissingDataBooleanHolder()
    {
        return _showMissingDataBooleanHolder;
    }
    
    
}
