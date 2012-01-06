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
package com.raytheon.viz.radar.ui;

import com.raytheon.viz.radar.rsc.image.RadarSRMResource.SRMSource;
import com.raytheon.viz.radar.ui.RadarDisplayManager.TrackTypes;

/**
 * Singleton controls for the values needed by radar and SCAN
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class RadarDisplayControls {
    private int stiNumStorms;

    private TrackTypes stiTrackType = TrackTypes.PAST_AND_FORECAST;

    private int hiPOHLow;

    private int hiPOHHigh;

    private int hiPOSHLow;

    private int hiPOSHHigh;

    private boolean tvsShowElevated;

    private boolean dmdMdTvsShowExtrapolated;

    private int dmdMinFeatureStrength;

    private boolean dmdShowOverlapping;

    private TrackTypes dmdTrackType = TrackTypes.PAST_AND_FORECAST;

    private SRMSource srmSource;

    /** SRM Direction in degrees clockwise from North */
    private int srmDir;

    /** Storm Relative Motion speed in knots? */
    private int srmSpeed;

    private String stormHexOrCircle;

    private boolean showAll;

    public RadarDisplayControls() {
    }

    /**
     * @return the stiNumStorms
     */
    public int getStiNumStorms() {
        return stiNumStorms;
    }

    /**
     * @param stiNumStorms
     *            the stiNumStorms to set
     */
    public void setStiNumStorms(int stiNumStorms) {
        if (this.stiNumStorms != stiNumStorms) {
            this.stiNumStorms = stiNumStorms;
            RadarDisplayManager.getInstance().displayConfigUpdated();
        }
    }

    /**
     * @return the stiTrackType
     */
    public TrackTypes getStiTrackType() {
        return stiTrackType;
    }

    /**
     * @param stiTrackType
     *            the stiTrackType to set
     */
    public void setStiTrackType(TrackTypes stiTrackType) {
        if (this.stiTrackType != stiTrackType) {
            this.stiTrackType = stiTrackType;
            RadarDisplayManager.getInstance().displayConfigUpdated();
        }
    }

    /**
     * @return the tvsShowElevated
     */
    public boolean isTvsShowElevated() {
        return tvsShowElevated;
    }

    /**
     * @param tvsShowElevated
     *            the tvsShowElevated to set
     */
    public void setTvsShowElevated(boolean tvsShowElevated) {
        if (tvsShowElevated != this.tvsShowElevated) {
            this.tvsShowElevated = tvsShowElevated;
            RadarDisplayManager.getInstance().displayConfigUpdated();
        }
    }

    /**
     * @return the dmdMdTvsShowExtrapolated
     */
    public boolean isDmdMdTvsShowExtrapolated() {
        return dmdMdTvsShowExtrapolated;
    }

    /**
     * @param dmdMdTvsShowExtrapolated
     *            the dmdMdTvsShowExtrapolated to set
     */
    public void setDmdMdTvsShowExtrapolated(boolean dmdMdTvsShowExtrapolated) {
        if (this.dmdMdTvsShowExtrapolated != dmdMdTvsShowExtrapolated) {
            this.dmdMdTvsShowExtrapolated = dmdMdTvsShowExtrapolated;
            RadarDisplayManager.getInstance().displayConfigUpdated();
        }
    }

    /**
     * @return the dmdMinFeatureStrength
     */
    public int getDmdMinFeatureStrength() {
        return dmdMinFeatureStrength;
    }

    /**
     * @param dmdMinFeatureStrength
     *            the dmdMinFeatureStrength to set
     */
    public void setDmdMinFeatureStrength(int dmdMinFeatureStrength) {
        if (dmdMinFeatureStrength != this.dmdMinFeatureStrength) {
            this.dmdMinFeatureStrength = dmdMinFeatureStrength;
            RadarDisplayManager.getInstance().displayConfigUpdated();
        }
    }

    /**
     * @return the dmdShowOverlapping
     */
    public boolean isDmdShowOverlapping() {
        return dmdShowOverlapping;
    }

    /**
     * @param dmdShowOverlapping
     *            the dmdShowOverlapping to set
     */
    public void setDmdShowOverlapping(boolean dmdShowOverlapping) {
        if (this.dmdShowOverlapping != dmdShowOverlapping) {
            this.dmdShowOverlapping = dmdShowOverlapping;
            RadarDisplayManager.getInstance().displayConfigUpdated();
        }
    }

    /**
     * @return the dmdTrackType
     */
    public TrackTypes getDmdTrackType() {
        return dmdTrackType;
    }

    /**
     * @param dmdTrackType
     *            the dmdTrackType to set
     */
    public void setDmdTrackType(TrackTypes dmdTrackType) {
        if (this.dmdTrackType != dmdTrackType) {
            this.dmdTrackType = dmdTrackType;
            RadarDisplayManager.getInstance().displayConfigUpdated();
        }
    }

    /**
     * @return the srmSource
     */
    public SRMSource getSrmSource() {
        return srmSource;
    }

    /**
     * @param srmSource
     *            the srmSource to set
     */
    public void setSrmSource(SRMSource srmSource) {
        if (this.srmSource != srmSource) {
            this.srmSource = srmSource;
            RadarDisplayManager.getInstance().displayConfigUpdated();
        }
    }

    /**
     * @return the srmDir
     */
    public int getSrmDir() {
        return srmDir;
    }

    /**
     * @param srmDir
     *            the srmDir to set
     */
    public void setSrmDir(int srmDir) {
        if (this.srmDir != srmDir) {
            this.srmDir = srmDir;
            RadarDisplayManager.getInstance().displayConfigUpdated();
        }
    }

    /**
     * @return the srmSpeed
     */
    public int getSrmSpeed() {
        return srmSpeed;
    }

    /**
     * @param srmSpeed
     *            the srmSpeed to set
     */
    public void setSrmSpeed(int srmSpeed) {
        if (srmSpeed != this.srmSpeed) {
            this.srmSpeed = srmSpeed;
            RadarDisplayManager.getInstance().displayConfigUpdated();
        }
    }

    /**
     * @return the hiPOHLow
     */
    public int getHiPOHLow() {
        return hiPOHLow;
    }

    /**
     * @param hiPOHLow
     *            the hiPOHLow to set
     */
    public void setHiPOHLow(int hiPOHLow) {
        if (this.hiPOHLow != hiPOHLow) {
            this.hiPOHLow = hiPOHLow;
            RadarDisplayManager.getInstance().displayConfigUpdated();
        }
    }

    /**
     * @return the hiPOHHigh
     */
    public int getHiPOHHigh() {
        return hiPOHHigh;
    }

    /**
     * @param hiPOHHigh
     *            the hiPOHHigh to set
     */
    public void setHiPOHHigh(int hiPOHHigh) {
        if (this.hiPOHHigh != hiPOHHigh) {
            this.hiPOHHigh = hiPOHHigh;
            RadarDisplayManager.getInstance().displayConfigUpdated();
        }
    }

    /**
     * @return the hiPOSHLow
     */
    public int getHiPOSHLow() {
        return hiPOSHLow;
    }

    /**
     * @param hiPOSHLow
     *            the hiPOSHLow to set
     */
    public void setHiPOSHLow(int hiPOSHLow) {
        if (this.hiPOSHLow != hiPOSHLow) {
            this.hiPOSHLow = hiPOSHLow;
            RadarDisplayManager.getInstance().displayConfigUpdated();
        }
    }

    /**
     * @return the hiPOSHHigh
     */
    public int getHiPOSHHigh() {
        return hiPOSHHigh;
    }

    /**
     * @param hiPOSHHigh
     *            the hiPOSHHigh to set
     */
    public void setHiPOSHHigh(int hiPOSHHigh) {
        if (this.hiPOSHHigh != hiPOSHHigh) {
            this.hiPOSHHigh = hiPOSHHigh;
            RadarDisplayManager.getInstance().displayConfigUpdated();
        }
    }

    /**
     * @return the stormHexOrCircle
     */
    public String getStormHexOrCircle() {
        return stormHexOrCircle;
    }

    /**
     * @param stormHexOrCircle
     *            the stormHexOrCircle to set
     */
    public void setStormHexOrCircle(String stormHexOrCircle) {
        if (!stormHexOrCircle.equals(this.stormHexOrCircle)) {
            this.stormHexOrCircle = stormHexOrCircle;
            RadarDisplayManager.getInstance().displayConfigUpdated();
        }
    }

    /**
     * @return the showAll
     */
    public boolean isShowAll() {
        return showAll;
    }

    /**
     * @param showAll
     *            the showAll to set
     */
    public void setShowAll(boolean showAll) {
        if (this.showAll != showAll) {
            this.showAll = showAll;
            RadarDisplayManager.getInstance().displayConfigUpdated();
        }
    }
}
