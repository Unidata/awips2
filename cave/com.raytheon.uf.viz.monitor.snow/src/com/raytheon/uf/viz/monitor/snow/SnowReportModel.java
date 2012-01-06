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
package com.raytheon.uf.viz.monitor.snow;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.viz.monitor.data.ObReportWorstThreat;
import com.raytheon.uf.viz.monitor.data.ObReportWorstTime;
import com.raytheon.uf.viz.monitor.data.ObReportWorstValue;
import com.raytheon.uf.viz.monitor.data.ObTime;
import com.raytheon.uf.viz.monitor.listeners.IGuardianListener;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;

/**
 * SnowReportModel class is a singleton class that contains the report model for
 * this decision assistance tool.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 13, 2009 1999       grichard    Initial creation.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class SnowReportModel {

    /**
     * The static singleton instance.
     */
    private static SnowReportModel instance;

    /**
     * Indicates whether to clear all existing observations (if true) or honor
     * the age-out scheme (if false) protected boolean redoFullThreatInterval;
     */
    private boolean redoFullThreatInterval;

    /**
     * Indicates whether the forecast office wants the Fog Monitor's SAFESEAS
     * fog threat levels considered when determining the overall marine threat
     * level
     */
    private boolean usingFogThreatLevel;

    /** The "green" list */
    private ObTime greenList = new ObTime(ObConst.ThreatLevel.GREEN);

    /** The "yellow" list */
    private ObTime yellowList = new ObTime(ObConst.ThreatLevel.YELLOW);

    /** The "red" list */
    private ObTime redList = new ObTime(ObConst.ThreatLevel.RED);

    /** The worst value report */
    private ObReportWorstValue worstValueReport = new ObReportWorstValue()
            .init();

    /** The worst value time report */
    private ObReportWorstTime worstValueTimeReport = new ObReportWorstTime()
            .init();

    /** The worst threat report */
    private ObReportWorstThreat worstThreatReport = new ObReportWorstThreat()
            .init();

    /** The worst threat time report */
    private ObReportWorstTime worstThreatTimeReport = new ObReportWorstTime()
            .init();

    /**
     * Observing Station Location
     */
    private final Map<String, com.vividsolutions.jts.geom.Point> obStationLocation = new HashMap<String, com.vividsolutions.jts.geom.Point>();

    /**
     * Observing Station Geometry
     */
    private final Map<String, PreparedGeometry> obStationGeometry = new HashMap<String, PreparedGeometry>();

    /**
     * Display (stand-in) for Guardian type alert visualization.
     */
    private IGuardianListener snowDisplay;

    /**
     * Singleton constructor.
     * 
     * @return the observation station model.
     */
    public static synchronized SnowReportModel getInstance() {
        if (instance == null) {
            instance = new SnowReportModel();
        }

        return instance;
    }

    /**
     * Private constructor: Use getInstance().
     */
    private SnowReportModel() {
    }

    public boolean isRedoFullThreatInterval() {
        return redoFullThreatInterval;
    }

    public void setRedoFullThreatInterval(boolean redoFullThreatInterval) {
        this.redoFullThreatInterval = redoFullThreatInterval;
    }

    public ObTime getGreenList() {
        return greenList;
    }

    public void setGreenList(ObTime greenList) {
        this.greenList = greenList;
    }

    public ObTime getYellowList() {
        return yellowList;
    }

    public void setYellowList(ObTime yellowList) {
        this.yellowList = yellowList;
    }

    public ObTime getRedList() {
        return redList;
    }

    public void setRedList(ObTime redList) {
        this.redList = redList;
    }

    public Map<String, com.vividsolutions.jts.geom.Point> getObStationLocation() {
        return obStationLocation;
    }

    public Map<String, PreparedGeometry> getObStationGeometry() {
        return obStationGeometry;
    }

    public ObReportWorstValue getWorstValueReport() {
        return worstValueReport;
    }

    public void setWorstValueReport(ObReportWorstValue worstValueReport) {
        this.worstValueReport = worstValueReport;
    }

    public ObReportWorstTime getWorstValueTimeReport() {
        return worstValueTimeReport;
    }

    public void setWorstValueTimeReport(ObReportWorstTime worstValueTimeReport) {
        this.worstValueTimeReport = worstValueTimeReport;
    }

    public ObReportWorstThreat getWorstThreatReport() {
        return worstThreatReport;
    }

    public void setWorstThreatReport(ObReportWorstThreat worstThreatReport) {
        this.worstThreatReport = worstThreatReport;
    }

    public ObReportWorstTime getWorstThreatTimeReport() {
        return worstThreatTimeReport;
    }

    public void setWorstThreatTimeReport(ObReportWorstTime worstThreatTimeReport) {
        this.worstThreatTimeReport = worstThreatTimeReport;
    }

    public boolean isUsingFogThreatLevel() {
        return usingFogThreatLevel;
    }

    public void setUsingFogThreatLevel(boolean usingFogThreatLevel) {
        this.usingFogThreatLevel = usingFogThreatLevel;
    }

    public IGuardianListener getSnowDisplay() {
        return snowDisplay;
    }

    public void setSnowDisplay(IGuardianListener snowDisplay) {
        this.snowDisplay = snowDisplay;
    }

}
