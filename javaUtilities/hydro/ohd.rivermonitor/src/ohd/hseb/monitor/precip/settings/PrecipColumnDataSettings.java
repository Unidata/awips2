package ohd.hseb.monitor.precip.settings;

public class PrecipColumnDataSettings
{
    private double _precipAlert1Hr ;
    private double _precipAlert3Hr ;
    private double _precipAlert6Hr ;

    private double _precipCaution1Hr ;
    private double _precipCaution3Hr ;
    private double _precipCaution6Hr ;

    private int _ratioAlert1Hr ;
    private int _ratioAlert3Hr ;
    private int _ratioAlert6Hr ;

    private int _ratioCaution1Hr ;
    private int _ratioCaution3Hr ;
    private int _ratioCaution6Hr ;

    private double _diffAlert1Hr ;
    private double _diffAlert3Hr ;
    private double _diffAlert6Hr ;

    private double _diffCaution1Hr ;
    private double _diffCaution3Hr ;
    private double _diffCaution6Hr ;
    
    public static final String PRECIP_SETTINGS_BEGIN_TAG = "#Precip Settings Begin";
    public static final String PRECIP_SETTINGS_END_TAG = "#Precip Settings End";
    public static final String PRECIP_ALERT_TAG = "Precip Alert 1Hr | 3Hr | 6Hr";
    public static final String PRECIP_CAUTION_TAG = "Precip Caution 1Hr | 3Hr | 6Hr";
    public static final String RATIO_ALERT_TAG = "Ratio Alert 1Hr | 3Hr | 6Hr";
    public static final String RATIO_CAUTION_TAG = "Ratio Caution 1Hr | 3Hr | 6Hr";
    public static final String DIFF_ALERT_TAG = "Diff Alert 1Hr | 3Hr | 6Hr";
    public static final String DIFF_CAUTION_TAG = "Diff Caution 1Hr | 3Hr | 6Hr";
    
    public static double DEFAULT_PRECIP_THRESHOLD_6HR = 1;
    public static double DEFAULT_PRECIP_THRESHOLD_3HR = 1.25;
    public static double DEFAULT_PRECIP_THRESHOLD_1HR = 1.15;

    public static double DEFAULT_PRECIP_MIN_THRESHOLD_6HR = 0;
    public static double DEFAULT_PRECIP_MIN_THRESHOLD_3HR = 0;
    public static double DEFAULT_PRECIP_MIN_THRESHOLD_1HR = 0;

    public static double DEFAULT_PRECIP_MAX_THRESHOLD_6HR = 5;
    public static double DEFAULT_PRECIP_MAX_THRESHOLD_3HR = 5;
    public static double DEFAULT_PRECIP_MAX_THRESHOLD_1HR = 5;

    public static double DEFAULT_DIFF_THRESHOLD_6HR = -0.25;
    public static double DEFAULT_DIFF_THRESHOLD_3HR = -0.3;
    public static double DEFAULT_DIFF_THRESHOLD_1HR = -0.35;

    public static double DEFAULT_DIFF_MIN_THRESHOLD_6HR = -5;
    public static double DEFAULT_DIFF_MIN_THRESHOLD_3HR = -5;
    public static double DEFAULT_DIFF_MIN_THRESHOLD_1HR = -5;

    public static double DEFAULT_DIFF_MAX_THRESHOLD_6HR = 5;
    public static double DEFAULT_DIFF_MAX_THRESHOLD_3HR = 5;
    public static double DEFAULT_DIFF_MAX_THRESHOLD_1HR = 5;

    public static int DEFAULT_RATIO_THRESHOLD_6HR = 90;
    public static int DEFAULT_RATIO_THRESHOLD_3HR = 90;
    public static int DEFAULT_RATIO_THRESHOLD_1HR = 90;

    public static int DEFAULT_RATIO_MIN_THRESHOLD_6HR = 0;
    public static int DEFAULT_RATIO_MIN_THRESHOLD_3HR = 0;
    public static int DEFAULT_RATIO_MIN_THRESHOLD_1HR = 0;

    public static int DEFAULT_RATIO_MAX_THRESHOLD_6HR = 400;
    public static int DEFAULT_RATIO_MAX_THRESHOLD_3HR = 400;
    public static int DEFAULT_RATIO_MAX_THRESHOLD_1HR = 400;
    
    public PrecipColumnDataSettings()
    {
        resetSettings();
    }

    public PrecipColumnDataSettings(double precipAlert1Hr, double precipAlert3Hr, double precipAlert6Hr,
            double precipCaution1Hr, double precipCaution3Hr, double precipCaution6Hr,
            int ratioAlert1Hr, int ratioAlert3Hr ,int ratioAlert6Hr,
            int ratioCaution1Hr, int ratioCaution3Hr, int ratioCaution6Hr,
            double diffAlert1Hr, double diffAlert3Hr, double diffAlert6Hr, 
            double diffCaution1Hr ,double diffCaution3Hr, double diffCaution6Hr)
    {
        _precipAlert1Hr = precipAlert1Hr;
        _precipAlert3Hr = precipAlert3Hr;
        _precipAlert6Hr = precipAlert6Hr;

        _precipCaution1Hr = precipCaution1Hr;
        _precipCaution3Hr = precipCaution3Hr;
        _precipCaution6Hr = precipCaution6Hr;

        _ratioAlert1Hr = ratioAlert1Hr;
        _ratioAlert3Hr = ratioAlert3Hr;
        _ratioAlert6Hr = ratioAlert6Hr;

        _ratioCaution1Hr = ratioCaution1Hr;
        _ratioCaution3Hr = ratioCaution3Hr;
        _ratioCaution6Hr = ratioCaution6Hr;

        _diffAlert1Hr = diffAlert1Hr;
        _diffAlert3Hr = diffAlert3Hr;
        _diffAlert6Hr = diffAlert6Hr;

        _diffCaution1Hr = diffCaution1Hr;
        _diffCaution3Hr = diffCaution3Hr;
        _diffCaution6Hr = diffCaution6Hr;
        
    }
    
    public void setPrecipSettings(double precipAlert1Hr, double precipAlert3Hr, double precipAlert6Hr,
            double precipCaution1Hr, double precipCaution3Hr, double precipCaution6Hr,
            int ratioAlert1Hr, int ratioAlert3Hr ,int ratioAlert6Hr,
            int ratioCaution1Hr, int ratioCaution3Hr, int ratioCaution6Hr,
            double diffAlert1Hr, double diffAlert3Hr, double diffAlert6Hr, 
            double diffCaution1Hr ,double diffCaution3Hr, double diffCaution6Hr)
    {
        _precipAlert1Hr = precipAlert1Hr;
        _precipAlert3Hr = precipAlert3Hr;
        _precipAlert6Hr = precipAlert6Hr;

        _precipCaution1Hr = precipCaution1Hr;
        _precipCaution3Hr = precipCaution3Hr;
        _precipCaution6Hr = precipCaution6Hr;

        _ratioAlert1Hr = ratioAlert1Hr;
        _ratioAlert3Hr = ratioAlert3Hr;
        _ratioAlert6Hr = ratioAlert6Hr;

        _ratioCaution1Hr = ratioCaution1Hr;
        _ratioCaution3Hr = ratioCaution3Hr;
        _ratioCaution6Hr = ratioCaution6Hr;

        _diffAlert1Hr = diffAlert1Hr;
        _diffAlert3Hr = diffAlert3Hr;
        _diffAlert6Hr = diffAlert6Hr;

        _diffCaution1Hr = diffCaution1Hr;
        _diffCaution3Hr = diffCaution3Hr;
        _diffCaution6Hr = diffCaution6Hr;
        
    }
    
    public void setPrecipSettings(PrecipColumnDataSettings precipSettings)
    {
         setPrecipSettings(precipSettings.getPrecipAlert1Hr(), precipSettings.getPrecipAlert3Hr(), precipSettings.getPrecipAlert6Hr(),
             precipSettings.getPrecipCaution1Hr(), precipSettings.getPrecipCaution3Hr(), precipSettings.getPrecipCaution6Hr(),
             precipSettings.getRatioAlert1Hr(), precipSettings.getRatioAlert3Hr (),precipSettings.getRatioAlert6Hr(),
             precipSettings.getRatioCaution1Hr(), precipSettings.getRatioCaution3Hr(), precipSettings.getRatioCaution6Hr(),
             precipSettings.getDiffAlert1Hr(), precipSettings.getDiffAlert3Hr(), precipSettings.getDiffAlert6Hr(), 
             precipSettings.getDiffCaution1Hr (),precipSettings.getDiffCaution3Hr(), precipSettings.getDiffCaution6Hr());
    }
    
    public void resetSettings()
    {
        _precipAlert1Hr = DEFAULT_PRECIP_THRESHOLD_1HR;
        _precipAlert3Hr = DEFAULT_PRECIP_THRESHOLD_3HR;
        _precipAlert6Hr = DEFAULT_PRECIP_THRESHOLD_6HR;

        _precipCaution1Hr = DEFAULT_PRECIP_THRESHOLD_1HR;
        _precipCaution3Hr = DEFAULT_PRECIP_THRESHOLD_3HR;
        _precipCaution6Hr = DEFAULT_PRECIP_THRESHOLD_6HR;

        _ratioAlert1Hr = DEFAULT_RATIO_THRESHOLD_1HR;
        _ratioAlert3Hr = DEFAULT_RATIO_THRESHOLD_3HR;
        _ratioAlert6Hr = DEFAULT_RATIO_THRESHOLD_6HR;

        _ratioCaution1Hr = DEFAULT_RATIO_THRESHOLD_1HR;
        _ratioCaution3Hr = DEFAULT_RATIO_THRESHOLD_3HR;
        _ratioCaution6Hr = DEFAULT_RATIO_THRESHOLD_6HR;

        _diffAlert1Hr = DEFAULT_DIFF_THRESHOLD_1HR;
        _diffAlert3Hr = DEFAULT_DIFF_THRESHOLD_3HR;
        _diffAlert6Hr = DEFAULT_DIFF_THRESHOLD_6HR;

        _diffCaution1Hr = DEFAULT_DIFF_THRESHOLD_1HR;
        _diffCaution3Hr = DEFAULT_DIFF_THRESHOLD_3HR;
        _diffCaution6Hr = DEFAULT_DIFF_THRESHOLD_6HR;
    }

    public double getDiffAlert1Hr()
    {
        return _diffAlert1Hr;
    }
    public void setDiffAlert1Hr(double diffAlert1Hr)
    {
        _diffAlert1Hr = diffAlert1Hr;
    }
    public double getDiffAlert3Hr()
    {
        return _diffAlert3Hr;
    }
    public void setDiffAlert3Hr(double diffAlert3Hr)
    {
        _diffAlert3Hr = diffAlert3Hr;
    }
    public double getDiffAlert6Hr()
    {
        return _diffAlert6Hr;
    }
    public void setDiffAlert6Hr(double diffAlert6Hr)
    {
        _diffAlert6Hr = diffAlert6Hr;
    }
    public double getDiffCaution1Hr()
    {
        return _diffCaution1Hr;
    }
    public void setDiffCaution1Hr(double diffCaution1Hr)
    {
        _diffCaution1Hr = diffCaution1Hr;
    }
    public double getDiffCaution3Hr()
    {
        return _diffCaution3Hr;
    }
    public void setDiffCaution3Hr(double diffCaution3Hr)
    {
        _diffCaution3Hr = diffCaution3Hr;
    }
    public double getDiffCaution6Hr()
    {
        return _diffCaution6Hr;
    }
    public void setDiffCaution6Hr(double diffCaution6Hr)
    {
        _diffCaution6Hr = diffCaution6Hr;
    }
    public double getPrecipAlert1Hr()
    {
        return _precipAlert1Hr;
    }
    public void setPrecipAlert1Hr(double precipAlert1Hr)
    {
        _precipAlert1Hr = precipAlert1Hr;
    }
    public double getPrecipAlert3Hr()
    {
        return _precipAlert3Hr;
    }
    public void setPrecipAlert3Hr(double precipAlert3Hr)
    {
        _precipAlert3Hr = precipAlert3Hr;
    }
    public double getPrecipAlert6Hr()
    {
        return _precipAlert6Hr;
    }
    public void setPrecipAlert6Hr(double precipAlert6Hr)
    {
        _precipAlert6Hr = precipAlert6Hr;
    }
    public double getPrecipCaution1Hr()
    {
        return _precipCaution1Hr;
    }
    public void setPrecipCaution1Hr(double precipCaution1Hr)
    {
        _precipCaution1Hr = precipCaution1Hr;
    }
    public double getPrecipCaution3Hr()
    {
        return _precipCaution3Hr;
    }
    public void setPrecipCaution3Hr(double precipCaution3Hr)
    {
        _precipCaution3Hr = precipCaution3Hr;
    }
    public double getPrecipCaution6Hr()
    {
        return _precipCaution6Hr;
    }
    public void setPrecipCaution6Hr(double precipCaution6Hr)
    {
        _precipCaution6Hr = precipCaution6Hr;
    }
    public int getRatioAlert1Hr()
    {
        return _ratioAlert1Hr;
    }
    public void setRatioAlert1Hr(int ratioAlert1Hr)
    {
        _ratioAlert1Hr = ratioAlert1Hr;
    }
    public int getRatioAlert3Hr()
    {
        return _ratioAlert3Hr;
    }
    public void setRatioAlert3Hr(int ratioAlert3Hr)
    {
        _ratioAlert3Hr = ratioAlert3Hr;
    }
    public int getRatioAlert6Hr()
    {
        return _ratioAlert6Hr;
    }
    public void setRatioAlert6Hr(int ratioAlert6Hr)
    {
        _ratioAlert6Hr = ratioAlert6Hr;
    }
    public int getRatioCaution1Hr()
    {
        return _ratioCaution1Hr;
    }
    public void setRatioCaution1Hr(int ratioCaution1Hr)
    {
        _ratioCaution1Hr = ratioCaution1Hr;
    }
    public int getRatioCaution3Hr()
    {
        return _ratioCaution3Hr;
    }
    public void setRatioCaution3Hr(int ratioCaution3Hr)
    {
        _ratioCaution3Hr = ratioCaution3Hr;
    }
    public int getRatioCaution6Hr()
    {
        return _ratioCaution6Hr;
    }
    public void setRatioCaution6Hr(int ratioCaution6Hr)
    {
        _ratioCaution6Hr = ratioCaution6Hr;
    }

    public PrecipColumnDataSettings getMenuSettings()
    {
        return this;
    }

    public String toString()
    {
        String result = _precipAlert1Hr + "|" +
        _precipAlert3Hr + "|" +
        _precipAlert6Hr + "|" +
        _precipCaution1Hr + "|" +
        _precipCaution3Hr + "|" +
        _precipCaution6Hr + "|" +
        _ratioAlert1Hr + "|" +
        _ratioAlert3Hr + "|" +
        _ratioAlert6Hr + "|" +
        _ratioCaution1Hr + "|" +
        _ratioCaution3Hr + "|" +
        _ratioCaution6Hr + "|" +
        _diffAlert1Hr + "|" +
        _diffAlert3Hr + "|" +
        _diffAlert6Hr + "|" +
        _diffCaution1Hr + "|" +
        _diffCaution3Hr + "|" +
        _diffCaution6Hr ;
        
        return result;
    }
}
