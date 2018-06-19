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

import java.awt.Rectangle;
import java.util.Calendar;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.shef.tables.Rwbiasstat;
import com.raytheon.uf.common.dataplugin.shef.tables.Rwparams;

/**
 * POJO used to accumulate all of the configuration information required for a
 * single run of HPE Field Gen.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 13, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class HPERunConfiguration {

    /*
     * Required property values read from Apps Defaults.
     */
    private HPEFieldGenConfig config;

    private Calendar runDateTime;

    private int runHours;

    private Set<HPERadarMosaic> productsToGenerate;

    private boolean calculateMeanFieldBias;

    private boolean dhrMeanFieldBias;

    private boolean retrievePrismData;

    private Rectangle geoGridData;

    private GeoData geoFileData;

    private Rwparams rwparams;

    private Rwbiasstat rwbiasstat;

    /*
     * The following fields are manipulated during the output generation phase
     * of HPE Field Gen.
     */

    private boolean satelliteAvailable;

    private boolean buildNeighborList;

    public HPERunConfiguration() {
    }

    public HPEFieldGenConfig getConfig() {
        return config;
    }

    public void setConfig(HPEFieldGenConfig config) {
        this.config = config;
    }

    public Calendar getRunDateTime() {
        return runDateTime;
    }

    public void setRunDateTime(Calendar runDateTime) {
        this.runDateTime = runDateTime;
    }

    public int getRunHours() {
        return runHours;
    }

    public void setRunHours(int runHours) {
        this.runHours = runHours;
    }

    public Set<HPERadarMosaic> getProductsToGenerate() {
        return productsToGenerate;
    }

    public void setProductsToGenerate(Set<HPERadarMosaic> productsToGenerate) {
        this.productsToGenerate = productsToGenerate;
    }

    public boolean isCalculateMeanFieldBias() {
        return calculateMeanFieldBias;
    }

    public void setCalculateMeanFieldBias(boolean calculateMeanFieldBias) {
        this.calculateMeanFieldBias = calculateMeanFieldBias;
    }

    public boolean isDhrMeanFieldBias() {
        return dhrMeanFieldBias;
    }

    public void setDhrMeanFieldBias(boolean dhrMeanFieldBias) {
        this.dhrMeanFieldBias = dhrMeanFieldBias;
    }

    public boolean isRetrievePrismData() {
        return retrievePrismData;
    }

    public void setRetrievePrismData(boolean retrievePrismData) {
        this.retrievePrismData = retrievePrismData;
    }

    public Rectangle getGeoGridData() {
        return geoGridData;
    }

    public void setGeoGridData(Rectangle geoGridData) {
        this.geoGridData = geoGridData;
    }

    public GeoData getGeoFileData() {
        return geoFileData;
    }

    public void setGeoFileData(GeoData geoFileData) {
        this.geoFileData = geoFileData;
    }

    public Rwparams getRwparams() {
        return rwparams;
    }

    public void setRwparams(Rwparams rwparams) {
        this.rwparams = rwparams;
    }

    public Rwbiasstat getRwbiasstat() {
        return rwbiasstat;
    }

    public void setRwbiasstat(Rwbiasstat rwbiasstat) {
        this.rwbiasstat = rwbiasstat;
    }

    public boolean isSatelliteAvailable() {
        return satelliteAvailable;
    }

    public void setSatelliteAvailable(boolean satelliteAvailable) {
        this.satelliteAvailable = satelliteAvailable;
    }

    public boolean isBuildNeighborList() {
        return buildNeighborList;
    }

    public void setBuildNeighborList(boolean buildNeighborList) {
        this.buildNeighborList = buildNeighborList;
    }
}