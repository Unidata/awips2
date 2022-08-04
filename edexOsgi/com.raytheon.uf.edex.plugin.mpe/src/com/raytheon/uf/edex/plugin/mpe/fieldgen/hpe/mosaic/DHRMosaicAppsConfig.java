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
package com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.mosaic;

import java.nio.file.Path;

import com.raytheon.uf.common.ohd.AppsDefaultsDirKeys;
import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsCustomField;
import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsPathField;
import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsStringField;
import com.raytheon.uf.edex.plugin.mpe.apps.SaveFlagConverter;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.HPEFieldgenConstants;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.IXmrgAlternateFormatFlags;

/**
 * POJO containing property values read from Apps Defaults that are used by the
 * HPE Field Gen DHRMosaic Generator.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 21, 2016 5631       bkowal      Initial creation
 * Oct 11, 2016 5631       bkowal      Implement {@link IXmrgAlternateFormatFlags}.
 *
 * </pre>
 *
 * @author bkowal
 */

public class DHRMosaicAppsConfig implements IXmrgAlternateFormatFlags {

    @AppsDefaultsPathField(property = AppsDefaultsDirKeys.HPE_DHRMOSAIC_DIR)
    private Path hpeDHRMosaicPath;

    @AppsDefaultsCustomField(property = HPEFieldgenConstants.AppsDefaults.HPE_SAVE_DHRHEIGHT, converter = SaveFlagConverter.class)
    private Boolean saveHeight;

    @AppsDefaultsPathField(property = HPEFieldgenConstants.AppsDefaults.HPE_DHRHEIGHT_DIR, required = false)
    private Path heightPath;

    @AppsDefaultsCustomField(property = HPEFieldgenConstants.AppsDefaults.HPE_SAVE_DHRINDEX, converter = SaveFlagConverter.class)
    private Boolean saveDHRIndex;

    @AppsDefaultsPathField(property = HPEFieldgenConstants.AppsDefaults.HPE_DHRINDEX_DIR, required = false)
    private Path dhrIndexPath;

    @AppsDefaultsCustomField(property = HPEFieldgenConstants.AppsDefaults.DHRMOSAIC_SAVE_NETCDF, converter = SaveFlagConverter.class)
    private Boolean saveNetCDF;

    @AppsDefaultsPathField(property = HPEFieldgenConstants.AppsDefaults.DHRMOSAIC_NETCDF_DIR, required = false)
    private Path netCDFPath;

    @AppsDefaultsStringField(property = HPEFieldgenConstants.AppsDefaults.DHRMOSAIC_NETCDF_ID, nullDefaultValue = true)
    private String netCDFId;

    @AppsDefaultsCustomField(property = HPEFieldgenConstants.AppsDefaults.DHRMOSAIC_SAVE_GIF, converter = SaveFlagConverter.class)
    private Boolean saveGIF;

    @AppsDefaultsPathField(property = HPEFieldgenConstants.AppsDefaults.DHRMOSAIC_GIF_DIR, required = false)
    private Path gifPath;

    @AppsDefaultsStringField(property = HPEFieldgenConstants.AppsDefaults.DHRMOSAIC_GIF_ID, nullDefaultValue = true)
    private String gifId;

    @AppsDefaultsCustomField(property = HPEFieldgenConstants.AppsDefaults.DHRMOSAIC_SAVE_JPEG, converter = SaveFlagConverter.class)
    private Boolean saveJPEG;

    @AppsDefaultsCustomField(property = HPEFieldgenConstants.AppsDefaults.DHRMOSAIC_SAVE_GRIB, converter = SaveFlagConverter.class)
    private Boolean saveGRIB;

    @AppsDefaultsPathField(property = HPEFieldgenConstants.AppsDefaults.MPE_MISBIN_DIR, required = false)
    private Path mpeMisbinPath;

    @AppsDefaultsPathField(property = HPEFieldgenConstants.AppsDefaults.DPR_GRID_DIR)
    private Path dprGridPath;

    @AppsDefaultsPathField(property = HPEFieldgenConstants.AppsDefaults.DHR_GRID_DIR)
    private Path dhrGridPath;

    public DHRMosaicAppsConfig() {
    }

    public Path getHpeDHRMosaicPath() {
        return hpeDHRMosaicPath;
    }

    public void setHpeDHRMosaicPath(Path hpeDHRMosaicPath) {
        this.hpeDHRMosaicPath = hpeDHRMosaicPath;
    }

    public Boolean getSaveHeight() {
        return saveHeight;
    }

    public void setSaveHeight(Boolean saveHeight) {
        this.saveHeight = saveHeight;
    }

    public Path getHeightPath() {
        return heightPath;
    }

    public void setHeightPath(Path heightPath) {
        this.heightPath = heightPath;
    }

    public Boolean getSaveDHRIndex() {
        return saveDHRIndex;
    }

    public void setSaveDHRIndex(Boolean saveDHRIndex) {
        this.saveDHRIndex = saveDHRIndex;
    }

    public Path getDhrIndexPath() {
        return dhrIndexPath;
    }

    public void setDhrIndexPath(Path dhrIndexPath) {
        this.dhrIndexPath = dhrIndexPath;
    }

    @Override
    public Boolean getSaveNetCDF() {
        return saveNetCDF;
    }

    public void setSaveNetCDF(Boolean saveNetCDF) {
        this.saveNetCDF = saveNetCDF;
    }

    @Override
    public Path getNetCDFPath() {
        return netCDFPath;
    }

    public void setNetCDFPath(Path netCDFPath) {
        this.netCDFPath = netCDFPath;
    }

    @Override
    public String getNetCDFId() {
        return netCDFId;
    }

    public void setNetCDFId(String netCDFId) {
        this.netCDFId = netCDFId;
    }

    @Override
    public Boolean getSaveGIF() {
        return saveGIF;
    }

    public void setSaveGIF(Boolean saveGIF) {
        this.saveGIF = saveGIF;
    }

    @Override
    public Path getGifPath() {
        return gifPath;
    }

    public void setGifPath(Path gifPath) {
        this.gifPath = gifPath;
    }

    @Override
    public String getGifId() {
        return gifId;
    }

    public void setGifId(String gifId) {
        this.gifId = gifId;
    }

    @Override
    public Boolean getSaveJPEG() {
        return saveJPEG;
    }

    public void setSaveJPEG(Boolean saveJPEG) {
        this.saveJPEG = saveJPEG;
    }

    @Override
    public Boolean getSaveGRIB() {
        return saveGRIB;
    }

    public void setSaveGRIB(Boolean saveGRIB) {
        this.saveGRIB = saveGRIB;
    }

    public Path getMpeMisbinPath() {
        return mpeMisbinPath;
    }

    public void setMpeMisbinPath(Path mpeMisbinPath) {
        this.mpeMisbinPath = mpeMisbinPath;
    }

    public Path getDprGridPath() {
        return dprGridPath;
    }

    public void setDprGridPath(Path dprGridPath) {
        this.dprGridPath = dprGridPath;
    }

    public Path getDhrGridPath() {
        return dhrGridPath;
    }

    public void setDhrGridPath(Path dhrGridPath) {
        this.dhrGridPath = dhrGridPath;
    }
}