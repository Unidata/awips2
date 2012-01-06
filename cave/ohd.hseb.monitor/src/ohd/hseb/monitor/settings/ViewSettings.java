package ohd.hseb.monitor.settings;

public class ViewSettings
{
    public static String COLUMN_TAG = "COLUMN";
    public static String SORT_TAG = "SORT";
    
    private String _columnDisplaySettings;
    
    public ViewSettings()
    {
        
    }
    
    public String getColumnDisplaySettings() 
    {
        return _columnDisplaySettings;
    }

    public void setColumnDisplaySettings(String columnDisplaySettings) 
    {
        _columnDisplaySettings = columnDisplaySettings;
    }

}
