package gov.noaa.nws.ncep.viz.rsc.ncgrid.dgdriv;

public class GridDBConstants {
    public static final String GRID_TBL_NAME = "grid";

    public static final String PLUGIN_NAME = "pluginName";

    public static final String INFO = "info";
    
    public static final String ID = "id";

    public static final String INFO_ID = "info.id";

    public static final String MODEL_NAME_QUERY = "info.datasetId";

    public static final String EVENT_NAME_QUERY = "info.secondaryId";

    public static final String ENSEMBLE_ID_QUERY = "info.ensembleId";

    public static final String NAVIGATION_QUERY = "info.location";

    public static final String PARAMETER_QUERY = "info.parameter.abbreviation";

    public static final String LEVEL_ID_QUERY = "info.level.masterLevel.name";

    public static final String LEVEL_ONE_QUERY = "info.level.levelonevalue";

    public static final String LEVEL_TWO_QUERY = "info.level.leveltwovalue";
    
    public static final String DATA_TIME_QUERY = "dataTime";
    
    public static final String REF_TIME_QUERY = DATA_TIME_QUERY+".refTime";
    
    public static final String FORECAST_TIME_QUERY = DATA_TIME_QUERY +".fcstTime";
    
    public static final String DATA_URI_QUERY = "dataURI";
}
