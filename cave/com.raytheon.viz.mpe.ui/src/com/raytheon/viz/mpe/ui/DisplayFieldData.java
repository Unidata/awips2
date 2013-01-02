package com.raytheon.viz.mpe.ui;

/**
 * DisplayFieldData enum
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 9, 2011            rgeorge     Initial creation
 * 
 * </pre>
 * 
 * @author rgeorge
 * @version 1.0
 */
public enum DisplayFieldData {

    rMosaic("rfcwide_rmosaic_dir", "Radar-Derived Precip"), //
    avgrMosaic("rfcwide_avg_rmosaic_dir", "Average Radar-Derived Precip"), //
    maxrMosaic("rfcwide_max_rmosaic_dir", "Max Radar-Derived Precip"), //
    bMosaic("rfcwide_bmosaic_dir",
            "Mean Field Bias Corrected Radar-Derived Precip"), //
    lMosaic("rfcwide_lmosaic_dir",
            "Local Bias Corrected Radar-Derived Precip (in)"), //
    gageOnly("rfcwide_gageonly_dir", "Gage Only Analysis (in)"), //
    satPre("rfcwide_satpre_dir", "Satellite-Derived Precip (in)"), //
    lsatPre("rfcwide_lsatpre_dir",
            "Local Bias Corrected Satellite-Derived Precip"), //
    mMosaic("rfcwide_mmosaic_dir", "Multisensor Precip"), //
    mlMosaic("rfcwide_mlmosaic_dir", "Local Bias Multisensor Precip"), //
    p3lMosaic("rfcwide_p3lmosaic_dir",
            "P3 Local Bias Corrected Radar-Derived Precip (in)"), //
    Xmrg("rfcwide_xmrg_dir", "Best Estimate QPE (in)"), //
    multiHour("", "%d hr Saved Precip Estimate For %s Ending %s (in)"), //
    Locspan("rfcwide_locspan_dir", "memory span index (local bias)", 0), //
    Locbias("rfcwide_locbias_dir", "Local Bias Values", 0), //
    localField1("mpe_localfield1_dir", "Local Field #1", 0), //
    localField2("mpe_localfield2_dir", "Local Field #2", 0), //
    localField3("mpe_localfield3_dir", "Local Field #3", 0), //
    Height("rfcwide_height_dir", "Height of Radar Coverage (ft) ", 0), //
    Index("rfcwide_index_dir", "Radar Coverage Map (in)", 0), //
    Prism("mpe_prism_dir", "Monthly Normal Precipitation (in)", 0), //
    maxtempPrism("mpe_prism_dir", "Monthly Normal Max Temperature (F)", 0), //
    mintempPrism("mpe_prism_dir", "Monthly Normal Min Temperature (F)", 0), //
    rfcMosaic("gaq_xmrg_1hr_dir", "RFC Best Estimate Mosaic"), //
    sgMosaic("mpe_sgmosaic_dir", "Satellite Gage Mosaic (in)"), //
    srMosaic("mpe_srmosaic_dir", "Satellite Radar Mosaic (in)"), //
    srgMosaic("mpe_srgmosaic_dir", "Satellite Gage Radar Mosaic (in)"), //
    rfcbMosaic("mpe_rfcbmosaic_dir",
            "RFC Bias Corrected Radar-Derived Precip (in)"), //
    rfcmMosaic("mpe_rfcmmosaic_dir", "RFC Multisensor Precip (in)"), //
    qmosaic("mpe_qmosaic_dir", "Q2 Radar Mosaic"), lqmosaic("mpe_lqmosaic_dir",
            "Q2 Local Bias Mosaic"), mlqmosaic("mpe_mlqmosaic_dir",
            "Q2 MultiSensor Mosaic"), subValue("", ""), //
    missing("", ""), gageTriangles("rfcwide_p3lmosaic_dir", "Gage Triangles"), savelevel2(
            "", "Save Level 2 Data"), qc_precipitation("",
            "QC Precipitation..."), qc_temperatures("", "QC Temperatures.."), qc_freezinglevel(
            "", "QC Freezing Level..."); //

    private String dirToken;

    String cv_use;

    private int cv_duration;

    private String displayString;

    private DisplayFieldData(String dirToken, String displayString) {
        this(dirToken, displayString, 1, null);
    }

    private DisplayFieldData(String dirToken, String displayString,
            int cv_duration) {
        this(dirToken, displayString, cv_duration, null);
    }

    private DisplayFieldData(String dirToken, String displayString,
            int cv_duration, String cv_use) {
        this.dirToken = dirToken;
        this.displayString = displayString;
        this.cv_duration = cv_duration;
        this.cv_use = cv_use;
    }

    public String getDirToken() {
        return dirToken;
    }

    /**
     * @return the cv_use
     */
    public String getCv_use() {
        if (cv_use == null) {
            cv_use = name().toUpperCase();
        }
        return cv_use;
    }

    public int getCv_duration() {
        return cv_duration;
    }

    @Override
    public String toString() {
        return displayString;
    }

    /**
     * A case-insensitive version of {@link DisplayFieldData#valueOf(String)}
     * 
     * @param displayFieldData
     * @return
     */
    public static DisplayFieldData fromString(String displayFieldData) {
        for (DisplayFieldData fieldData : DisplayFieldData.values()) {
            if (fieldData.name().equalsIgnoreCase(displayFieldData)) {
                return fieldData;
            }
        }
        return null;
    }
}
