/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe;

import java.nio.file.Path;
import com.raytheon.uf.common.mpe.gribit2.XmrgToGribConstants;
import com.raytheon.uf.edex.plugin.mpe.HrapGridFactor;
import com.raytheon.uf.common.mpe.constants.MpeConstants;
import com.raytheon.uf.edex.plugin.mpe.XmrgDateNameFormat;
import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsBooleanField;
import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsCustomField;
import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsDoubleField;
import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsIntegerField;
import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsPathField;
import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsStringField;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.FieldgenConstants;

/**
 * POJO containing configuration information utilized by HPE Field Generator.
 * TODO: ideally this class (or some derivative thereof) will be reusable for
 * MPE Field Generator as well (for now, it will just remain associated solely
 * with HPE FieldGen).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 23, 2016 5631       bkowal      Initial creation
 * Aug 31, 2016 5631       bkowal      Added {@link #geoBinPath} and {@link #geoAsciiPath}.
 * Sep 01, 2016 4628       bkowal      Centralized common Apps Defaults property
 *                                     constants.
 * Sep 13, 2016 5631       bkowal      Added {@link #gageLocationPath}.
 * Sep 19, 2016 5631       bkowal      Added {@link #beamHeightPath} and {@link #prismPath}.
 * Sep 27, 2016 5631       bkowal      Added missing getters/setters.
 * Oct 05, 2016 5631       bkowal      Added {@link #rfcBiasLag}.
 * Oct 18, 2016 5631       bkowal      Added {@link #autoGraphicScale}.
 * 
 * </pre>
 * 
 * @author bkowal
 */
public class HPEFieldGenConfig {

    // in minutes
    private static final int DEFAULT_DPA_WINDOW = 10;

    // in minutes
    private static final int DEFAULT_DHR_WINDOW = 10;

    // in minutes
    private static final int DEFAULT_DSP_WINDOW = 10;

    // in minutes
    private static final int DEFAULT_DSP_DURATION = 60;

    private Boolean saveGIF = Boolean.FALSE;

    private Boolean saveJPEG = Boolean.FALSE;

    private Boolean saveGRIB = Boolean.FALSE;

    private Boolean saveNETCDF = Boolean.FALSE;

    @AppsDefaultsIntegerField(property = HPEFieldgenConstants.AppsDefaults.HPE_TIMELAG, defaultValue = 0)
    private int timeLag;

    @AppsDefaultsStringField(property = HPEFieldgenConstants.AppsDefaults.RFCW_RFCNAME, nullDefaultValue = true)
    private String rfcName;

    /**
     * Number of minutes around top of hour in which to search for a dpa
     * top-of-hour product.
     */
    private int dpaWindow = DEFAULT_DPA_WINDOW;

    /**
     * Number of minutes around top of hour in which to search for a dhr
     * top-of-hour product.
     */
    @AppsDefaultsIntegerField(property = HPEFieldgenConstants.AppsDefaults.DHR_WINDOW, defaultValue = DEFAULT_DHR_WINDOW)
    private int dhrWindow;

    /**
     * Number of minutes around top of hour in which to search for a dsp
     * top-of-hour product.
     */
    @AppsDefaultsIntegerField(property = HPEFieldgenConstants.AppsDefaults.DSP_WINDOW, defaultValue = DEFAULT_DSP_WINDOW)
    private int dspWindow;

    /**
     * Duration minutes for dsp product.
     */
    @AppsDefaultsIntegerField(property = HPEFieldgenConstants.AppsDefaults.DSP_DURATION, defaultValue = DEFAULT_DSP_DURATION)
    private int dspDuration;

    /**
     * indicates whether a full or quarter hrap grid should be used.
     */
    @AppsDefaultsCustomField(property = XmrgToGribConstants.AppsDefaults.HPE_HRAP_GRID_FACTOR, converter = HrapGridFactorConverter.class, defaultValue = HrapGridFactor.LOOKUP_NUM_4)
    private HrapGridFactor hrapGridFactor;

    /**
     * flag indicating whether or not zeros should be filtered out of gage data.
     */
    @AppsDefaultsBooleanField(property = HPEFieldgenConstants.AppsDefaults.HPE_DEL_GAGE_ZEROS, defaultValue = false)
    private boolean delGageZeros;

    /**
     * indicates how the date should be formatted within a xmrg file name.
     */
    @AppsDefaultsCustomField(property = HPEFieldgenConstants.AppsDefaults.ST3_DATE_FORM, converter = XmrgDateNameConverter.class, defaultValue = XmrgDateNameFormat.VALUE_MDY)
    private XmrgDateNameFormat dateFormat;

    /**
     * flag indicating whether or not Gage QC should be ran.
     */
    @AppsDefaultsBooleanField(property = HPEFieldgenConstants.AppsDefaults.HPE_GAGE_QC, defaultValue = false)
    private boolean runGageQC;

    /**
     * flag indicating whether or not loc bias should be rerun.
     */
    @AppsDefaultsBooleanField(property = HPEFieldgenConstants.AppsDefaults.HPE_LOCBIAS_1HR_RERUN, defaultValue = false)
    private boolean rerunLocBias1Hr;

    /**
     * value of the FXA_LOCAL_SITE environment variable. This field is REQUIRED
     * (absence = application termination) and has no defaults.
     */
    @AppsDefaultsStringField(property = MpeConstants.AppsDefaults.FXA_LOCAL_SITE, required = true)
    private String fxaLocalSite;

    /**
     * flag indicating whether or not the misbin file should be loaded.
     */
    @AppsDefaultsBooleanField(property = HPEFieldgenConstants.AppsDefaults.HPE_LOAD_MISBIN, defaultValue = false)
    private boolean loadMisbin;

    /**
     * flag indicating whether or not local bias should be used when building
     * EBMosaic and BDHRMosaic products. When local bias is not used, mean field
     * bias will be used.
     */
    @AppsDefaultsBooleanField(property = HPEFieldgenConstants.AppsDefaults.HPE_USE_LOCBIAS, defaultValue = false)
    private boolean useLocBias;

    /**
     * flag indicating whether or not nowcast should be ran.
     */
    @AppsDefaultsBooleanField(property = HPEFieldgenConstants.AppsDefaults.HPE_RUN_NOWCAST, defaultValue = false)
    private boolean runNowcast;

    /**
     * The QPE best estimate type. Used to determine which set of polygons
     * should be applied to the Best Estimate QPE product. This field is
     * REQUIRED (absence = application termination) and has no defaults.
     */
    @AppsDefaultsCustomField(property = HPEFieldgenConstants.AppsDefaults.HPE_QPE_FIELDTYPE, required = true, converter = RadarMosaicConverter.class)
    private HPERadarMosaic bestMosaic;

    @AppsDefaultsCustomField(property = HPEFieldgenConstants.AppsDefaults.HPE_BASE_RADAR_MOSAIC, converter = RadarMosaicConverter.class, defaultValue = "ERMOSAIC")
    private HPERadarMosaic baseMosaic;

    @AppsDefaultsBooleanField(property = HPEFieldgenConstants.AppsDefaults.HPE_DUALPOL_ON, defaultValue = true)
    private boolean dualPolOn;

    @AppsDefaultsStringField(property = HPEFieldgenConstants.AppsDefaults.HPE_BIAS_SOURCE, defaultValue = "RFC")
    private String defaultBiasSource;

    @AppsDefaultsIntegerField(property = HPEFieldgenConstants.AppsDefaults.HPE_RFC_BIAS_LAG, defaultValue = 2)
    private int rfcBiasLag;

    // in minutes
    @AppsDefaultsIntegerField(property = HPEFieldgenConstants.AppsDefaults.HPE_RFC_BIAS_FLAG, defaultValue = 2)
    private int rfcBiasFlag;

    @AppsDefaultsPathField(property = HPEFieldgenConstants.AppsDefaults.GEO_ST3_BIN)
    private Path geoBinPath;

    @AppsDefaultsPathField(property = HPEFieldgenConstants.AppsDefaults.GEO_ST3_ASCII)
    private Path geoAsciiPath;

    @AppsDefaultsPathField(property = HPEFieldgenConstants.AppsDefaults.RFCWIDE_GAGELOC_DIR)
    private Path gageLocationPath;

    @AppsDefaultsPathField(property = FieldgenConstants.AppsDefaults.RFCWIDE_BEAMHEIGHT_DIR)
    private Path beamHeightPath;

    @AppsDefaultsPathField(property = HPEFieldgenConstants.AppsDefaults.RFCWIDE_PRISM_DIR)
    private Path prismPath;

    @AppsDefaultsDoubleField(property = HPEFieldgenConstants.AppsDefaults.ST3_AUTO_GRAPHIC_SCALE, defaultValue = 4.0)
    private double autoGraphicScale;

    public HPEFieldGenConfig() {
    }

    public Boolean getSaveGIF() {
        return saveGIF;
    }

    public void setSaveGIF(Boolean saveGIF) {
        this.saveGIF = saveGIF;
    }

    public Boolean getSaveJPEG() {
        return saveJPEG;
    }

    public void setSaveJPEG(Boolean saveJPEG) {
        this.saveJPEG = saveJPEG;
    }

    public Boolean getSaveGRIB() {
        return saveGRIB;
    }

    public void setSaveGRIB(Boolean saveGRIB) {
        this.saveGRIB = saveGRIB;
    }

    public Boolean getSaveNETCDF() {
        return saveNETCDF;
    }

    public void setSaveNETCDF(Boolean saveNETCDF) {
        this.saveNETCDF = saveNETCDF;
    }

    public int getTimeLag() {
        return timeLag;
    }

    public void setTimeLag(int timeLag) {
        this.timeLag = timeLag;
    }

    public String getRfcName() {
        return rfcName;
    }

    public void setRfcName(String rfcName) {
        this.rfcName = rfcName;
    }

    public int getDpaWindow() {
        return dpaWindow;
    }

    public void setDpaWindow(int dpaWindow) {
        this.dpaWindow = dpaWindow;
    }

    public int getDhrWindow() {
        return dhrWindow;
    }

    public void setDhrWindow(int dhrWindow) {
        this.dhrWindow = dhrWindow;
    }

    public int getDspWindow() {
        return dspWindow;
    }

    public void setDspWindow(int dspWindow) {
        this.dspWindow = dspWindow;
    }

    public int getDspDuration() {
        return dspDuration;
    }

    public void setDspDuration(int dspDuration) {
        this.dspDuration = dspDuration;
    }

    public HrapGridFactor getHrapGridFactor() {
        return hrapGridFactor;
    }

    public void setHrapGridFactor(HrapGridFactor hrapGridFactor) {
        this.hrapGridFactor = hrapGridFactor;
    }

    public boolean isDelGageZeros() {
        return delGageZeros;
    }

    public void setDelGageZeros(boolean delGageZeros) {
        this.delGageZeros = delGageZeros;
    }

    public XmrgDateNameFormat getDateFormat() {
        return dateFormat;
    }

    public void setDateFormat(XmrgDateNameFormat dateFormat) {
        this.dateFormat = dateFormat;
    }

    public boolean isRunGageQC() {
        return runGageQC;
    }

    public void setRunGageQC(boolean runGageQC) {
        this.runGageQC = runGageQC;
    }

    public boolean isRerunLocBias1Hr() {
        return rerunLocBias1Hr;
    }

    public void setRerunLocBias1Hr(boolean rerunLocBias1Hr) {
        this.rerunLocBias1Hr = rerunLocBias1Hr;
    }

    public String getFxaLocalSite() {
        return fxaLocalSite;
    }

    public void setFxaLocalSite(String fxaLocalSite) {
        this.fxaLocalSite = fxaLocalSite;
    }

    public boolean isLoadMisbin() {
        return loadMisbin;
    }

    public void setLoadMisbin(boolean loadMisbin) {
        this.loadMisbin = loadMisbin;
    }

    public boolean isUseLocBias() {
        return useLocBias;
    }

    public void setUseLocBias(boolean useLocBias) {
        this.useLocBias = useLocBias;
    }

    public boolean isRunNowcast() {
        return runNowcast;
    }

    public void setRunNowcast(boolean runNowcast) {
        this.runNowcast = runNowcast;
    }

    public HPERadarMosaic getBestMosaic() {
        return bestMosaic;
    }

    public void setBestMosaic(HPERadarMosaic bestMosaic) {
        this.bestMosaic = bestMosaic;
    }

    public HPERadarMosaic getBaseMosaic() {
        return baseMosaic;
    }

    public void setBaseMosaic(HPERadarMosaic baseMosaic) {
        this.baseMosaic = baseMosaic;
    }

    public Path getGeoBinPath() {
        return geoBinPath;
    }

    public void setGeoBinPath(Path geoBinPath) {
        this.geoBinPath = geoBinPath;
    }

    public Path getGeoAsciiPath() {
        return geoAsciiPath;
    }

    public void setGeoAsciiPath(Path geoAsciiPath) {
        this.geoAsciiPath = geoAsciiPath;
    }

    public Path getGageLocationPath() {
        return gageLocationPath;
    }

    public void setGageLocationPath(Path gageLocationPath) {
        this.gageLocationPath = gageLocationPath;
    }

    public Path getBeamHeightPath() {
        return beamHeightPath;
    }

    public void setBeamHeightPath(Path beamHeightPath) {
        this.beamHeightPath = beamHeightPath;
    }

    public Path getPrismPath() {
        return prismPath;
    }

    public void setPrismPath(Path prismPath) {
        this.prismPath = prismPath;
    }

    public boolean isDualPolOn() {
        return dualPolOn;
    }

    public void setDualPolOn(boolean dualPolOn) {
        this.dualPolOn = dualPolOn;
    }

    public String getDefaultBiasSource() {
        return defaultBiasSource;
    }

    public void setDefaultBiasSource(String defaultBiasSource) {
        this.defaultBiasSource = defaultBiasSource;
    }

    public int getRfcBiasLag() {
        return rfcBiasLag;
    }

    public void setRfcBiasLag(int rfcBiasLag) {
        this.rfcBiasLag = rfcBiasLag;
    }

    public int getRfcBiasFlag() {
        return rfcBiasFlag;
    }

    public void setRfcBiasFlag(int rfcBiasFlag) {
        this.rfcBiasFlag = rfcBiasFlag;
    }

    public double getAutoGraphicScale() {
        return autoGraphicScale;
    }

    public void setAutoGraphicScale(double autoGraphicScale) {
        this.autoGraphicScale = autoGraphicScale;
    }
}