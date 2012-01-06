package gov.noaa.nws.ncep.viz.localization.impl;


public class LocalizationConstants {
    public static final int DEFAULT = 0;

    public static final int BASE = 1;

    public static final int DESK = 1 << 2;

    public static final int SITE = 1 << 3;

    public static final int USER = 1 << 4;

    public static final int USER_SITE = 1 << 5;

    public static final int USER_SITE_DESK = 1 << 6;

    public static final int LOCAL = 1 << 7;

    public static final int EXACT_MATCH = 1;

    public static final int PREFIX_MATCH = 2;

    public static final int POSTFIX_MATCH = 3;

    public static final int CONTAIN_MATCH = 4;

    public static final String EXACT_MATCH_STRING_VALUE = "exact";

    public static final String PREFIX_MATCH_STRING_VALUE = "prefix";

    public static final String POSTFIX_MATCH_STRING_VALUE = "postfix";

    public static final String CONTAIN_MATCH_STRING_VALUE = "contain";

    public static final String LOCAL_DIR = ".";

    public static final String NCEP_DIR = "ncep";

    public static final String BASE_DIR = "base";

    public static final String SITE_DIR = "site";

    public static final String DESK_DIR = "desk";

    public static final String USER_DIR = "user";

    public static final String LOCALIZATION_BASE_LEVEL = "base";

    public static final String LOCALIZATION_SITE_LEVEL = "site";

    public static final String LOCALIZATION_DESK_LEVEL = "desk";

    public static final String LOCALIZATION_USER_LEVEL = "user";

    public static final String DEFAULT_ROOT_DIR_PORTION_FOR_NCEP_BASE = "ncep";

    public static final String DEFAULT_ROOT_DIR_PORTION_FOR_NCEP_SITE_OR_USER = "etc";

    public static final String ACTIVE_DESK = "-ACTIVE_DESK";

    public static final String ACTIVE_SITE = "-ACTIVE_SITE";

    public static final String COLOR_MAP_PATH = "luts";
}
