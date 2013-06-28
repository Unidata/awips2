package ohd.hseb.monitor.river.settings;

public class RiverColumnDataSettings
{
    private String _fcstTypeSource;
    private int _alertalarmLookBack;
    private int _latestObsLookBack;
    private int _latestFcstBasisTimeLookBack;
    private int _vtecEventEndTimeApproach;
    private int _ugcExpireTimeApproach;
    private int _vtecEventProductTimeLookBack;

    public static String DEFAULT_FCST_TYPESOURCE = "IngestFilter" ;
    public static int DEFAULT_ALERTALARM_LOOKBACK = 8;
    public static int DEFAULT_LATEST_OBS_LOOKBACK = 12 ;
    public static int DEFAULT_LATEST_FCST_BASISTIME_LOOKBACK = 24;
    public static int DEFAULT_VTEC_ENDTIME_APPROACH = 4;
    public static int DEFAULT_UGC_EXPIRETIME_APPROACH = 4;
    public static int DEFAULT_VTEC_EVENT_PRODUCT_TIME_LOOKBACK = 168;

    public static final String RIVER_SETTINGS_BEGIN_TAG = "#River Settings Begin";
    public static final String RIVER_SETTINGS_END_TAG = "#River Settings End";
    public static String FCST_TS_TAG = "FCST_TS"; 
    public static String VALID_TIME_TAG = "Valid_Time";
    public static String VTEC_EVENT_END_TIME_TAG = "VTECEventEnd_Time";
    public static String UGC_EXPIRE_TIME_TAG = "UGCExpire_Time";
    public static String VTEC_EVENT_PRODUCT_TIME_TAG = "VTECEventProduct_Time";
    public static String LATEST_FCST_BASIS_TIME_TAG = "LatestFcst_BasisTime";
    public static String LATEST_OBS_TIME_TAG = "LatestObs_Time";

    public RiverColumnDataSettings()
    {
       resetSettings();
    }
    
    public void resetSettings()
    {
        _fcstTypeSource = DEFAULT_FCST_TYPESOURCE ;
        _alertalarmLookBack = DEFAULT_ALERTALARM_LOOKBACK;
        _latestObsLookBack = DEFAULT_LATEST_OBS_LOOKBACK;
        _latestFcstBasisTimeLookBack = DEFAULT_LATEST_FCST_BASISTIME_LOOKBACK;
        _vtecEventEndTimeApproach = DEFAULT_VTEC_ENDTIME_APPROACH;
        _ugcExpireTimeApproach = DEFAULT_UGC_EXPIRETIME_APPROACH; 
        _vtecEventProductTimeLookBack = DEFAULT_VTEC_EVENT_PRODUCT_TIME_LOOKBACK;
    }

    public String getFcstTypeSource()
    {
        return _fcstTypeSource;
    }
    public void setFcstTypeSource(String fcstTypeSource)
    {
        _fcstTypeSource = fcstTypeSource;
    }
    public int getAlertAlarmLookBack()
    {
        return _alertalarmLookBack;
    }
    public void setAlertAlarmLookBack(int alertalarmLookBack)
    {
        _alertalarmLookBack = alertalarmLookBack;
    }
    public int getLatestObsLookBack()
    {
        return _latestObsLookBack;
    }
    public void setLatestObsLookBack(int latestObsLookBack)
    {
        _latestObsLookBack = latestObsLookBack;
    }
    public int getLatestFcstBasisTimeLookBack()
    {
        return _latestFcstBasisTimeLookBack;
    }
    public void setLatestFcstBasisTimeLookBack(int fcstBasisTimeLookBack)
    {
        _latestFcstBasisTimeLookBack = fcstBasisTimeLookBack;
    }
    public int getVtecEventEndTimeApproach()
    {
        return _vtecEventEndTimeApproach;
    }
    public void setVtecEventEndTimeApproach(int vtecEventEndTimeApproach)
    {
        _vtecEventEndTimeApproach = vtecEventEndTimeApproach;
    }
    public int getUgcExpireTimeApproach()
    {
        return _ugcExpireTimeApproach;
    }
    public void setUgcExpireTimeApproach(int ugcExpireTimeApproach)
    {
        _ugcExpireTimeApproach = ugcExpireTimeApproach;
    }

    public int getVtecEventProductTimeLookBack()
    {
        return _vtecEventProductTimeLookBack;
    }

    public void setVtecEventProductTimeLookBack(int vtecEventProductTimeLookBack)
    {
        _vtecEventProductTimeLookBack = vtecEventProductTimeLookBack;
    }

}
