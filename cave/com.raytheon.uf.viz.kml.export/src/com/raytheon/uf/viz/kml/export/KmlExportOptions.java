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
package com.raytheon.uf.viz.kml.export;

import java.io.File;
import java.util.List;

/**
 * Contains any options which can be configured for KML export.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 4, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class KmlExportOptions {

    public enum KmlExportTimeMode {
        NONE, TIME_STAMP, TIME_SPAN;
    }

    private File kmzFileLocation;

    private int firstFrameIndex;

    private int lastFrameIndex;

    private boolean shadeEarth;

    // Google Earth requires a fairly large plot scale to make plots look nice
    // but World Wind prefers a smaller scale.
    private double plotIconScale;

    private boolean fillPlotBackground;

    private boolean preserveVisibility;

    private KmlExportTimeMode timeMode;

    private List<KmlPane> panes;

    private int paintSleepMillis;

    private int maxRefreshSeconds;

    public File getKmzFileLocation() {
        return kmzFileLocation;
    }

    public void setKmzFileLocation(File kmzFileLocation) {
        this.kmzFileLocation = kmzFileLocation;
    }

    public int getFirstFrameIndex() {
        return firstFrameIndex;
    }

    public void setFirstFrameIndex(int firstFrameIndex) {
        this.firstFrameIndex = firstFrameIndex;
    }

    public int getLastFrameIndex() {
        return lastFrameIndex;
    }

    public void setLastFrameIndex(int lastFrameIndex) {
        this.lastFrameIndex = lastFrameIndex;
    }

    public boolean isShadeEarth() {
        return shadeEarth;
    }

    public void setShadeEarth(boolean shadeEarth) {
        this.shadeEarth = shadeEarth;
    }

    public boolean isFillPlotBackground() {
        return fillPlotBackground;
    }

    public void setFillPlotBackground(boolean fillPlotBackground) {
        this.fillPlotBackground = fillPlotBackground;
    }

    public boolean isPreserveVisibility() {
        return preserveVisibility;
    }

    public void setPreserveVisibility(boolean preserveVisibility) {
        this.preserveVisibility = preserveVisibility;
    }

    public KmlExportTimeMode getTimeMode() {
        return timeMode;
    }

    public void setTimeMode(KmlExportTimeMode timeMode) {
        this.timeMode = timeMode;
    }

    public List<KmlPane> getPanes() {
        return panes;
    }

    public void setPanes(List<KmlPane> panes) {
        this.panes = panes;
    }

    public boolean isSinglePane() {
        return panes != null && panes.size() == 1;
    }

    public KmlPane getSinglPane() {
        if (isSinglePane()) {
            return panes.get(0);
        } else {
            return null;
        }
    }

    public int getPaintSleepMillis() {
        return paintSleepMillis;
    }

    public void setPaintSleepMillis(int paintSleepMillis) {
        this.paintSleepMillis = paintSleepMillis;
    }

    public int getMaxRefreshSeconds() {
        return maxRefreshSeconds;
    }

    public void setMaxRefreshSeconds(int maxRefreshSeconds) {
        this.maxRefreshSeconds = maxRefreshSeconds;
    }

    public double getPlotIconScale() {
        return plotIconScale;
    }

    public void setPlotIconScale(double plotIconScale) {
        this.plotIconScale = plotIconScale;
    }

}
