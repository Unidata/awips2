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
package com.raytheon.uf.viz.datadelivery.bandwidth.ui;

import java.util.ArrayList;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.datadelivery.bandwidth.data.BandwidthGraphData;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Canvas image class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 28, 2012    1269    lvenable    Initial creation.
 * Dec 13, 2012    1269    lvenable    Fixes and updates.
 * Nov 25, 2013    2545    mpduff      Data sorted by network.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public abstract class AbstractCanvasImage {
    /** No display value string */
    public final String NO_DISPLAY_STRING = "No Display Value";

    /** The canvas settings */
    protected CanvasSettings cs;

    /** The image */
    protected Image image;

    /** The display */
    protected Display display;

    /** The background color */
    protected Color bgColor;

    /** The text offset */
    public static final int TEXT_OFFSET = 25;

    /** The data for the graph */
    protected BandwidthGraphData graphData;

    /** Milliseconds per pixel */
    protected long millisPerPix = 0;

    /** Milliseconds in 48 hours */
    protected final long millis48Hrs = TimeUtil.MILLIS_PER_HOUR * 48;

    /** The GMT Time Zone */
    protected final TimeZone timeZone = TimeZone.getTimeZone("GMT");

    /** The image manager object */
    protected BandwidthImageMgr imageMgr;

    /**
     * Constructor.
     * 
     * @param parentComp
     *            The parent composite
     * @param cs
     *            The canvas settings
     * @param graphData
     *            The graph data object
     */
    public AbstractCanvasImage(Composite parentComp, CanvasSettings cs,
            BandwidthGraphData graphData, BandwidthImageMgr imageMgr) {
        this.cs = cs;
        this.display = parentComp.getDisplay();
        this.graphData = graphData;
        this.imageMgr = imageMgr;

        parentComp.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                disposeImage();
                disposeResources();
            }
        });
    }

    /**
     * Dispose the image.
     */
    protected final void disposeImage() {
        if (image != null) {
            image.dispose();
        }
    }

    /**
     * Set the canvas settings.
     * 
     * @param cs
     *            Canvas setting.
     */
    public void setCanvasSetting(CanvasSettings cs) {
        this.cs = cs;
    }

    /**
     * Regenerate the image.
     */
    public void regenerateImage() {
        disposeImage();
        image = new Image(display, cs.getImageWidth(), cs.getImageHeight());
        drawImage();
    }

    /**
     * Regenerate the images with the new data.
     * 
     * @param graphData
     *            New graph data object
     */
    public void regenerateImage(BandwidthGraphData graphData) {
        this.graphData = graphData;
        regenerateImage();
    }

    /**
     * Get the canvas settings.
     * 
     * @return The canvas settings
     */
    public CanvasSettings getCanvasSettings() {
        return cs;
    }

    /**
     * Get the image.
     * 
     * @return The image
     */
    public Image getImage() {
        return image;
    }

    /**
     * Get the tool tip string for this coordinate.
     * 
     * @param mouseCoord
     *            The mouse coordinate
     * 
     * @return The display string or null if no tool tip text available
     */
    public String getToolTipText(Point mouseCoord) {
        return null;
    }

    /**
     * Is there a selection in this canvas?
     * 
     * @return true if a selection exists
     */
    public boolean hasSelection() {
        return false;
    }

    /**
     * Perform an action when called.
     * 
     * @param mousePt
     *            Mouse coordinate where the mouse was clicked.
     */
    public void performAction(Point mousePt) {
        // Not used by default. Must be overridden.
    }

    /**
     * Get the data sorted.
     * 
     * @return List of subscription names
     */
    public final List<String> getSortedData() {

        if (graphData == null) {
            return new ArrayList<String>();
        }

        switch (imageMgr.getSortBy()) {
        case NAME_ASC:
            return graphData.getSortedNames(true, imageMgr.getNetwork());
        case NAME_DESC:
            return graphData.getSortedNames(false, imageMgr.getNetwork());
        case CURRENT_TIME:
            return graphData.getSubscriptionsSortedByTime(
                    imageMgr.getNetwork(), imageMgr.getCurrentTimeMillis(),
                    true);
        case SELECTED_INTERSECT:
            return graphData.getSubscriptionsSortedByTime(
                    imageMgr.getNetwork(), imageMgr.getSortTimeMillis(), true);
        case SELECTED_START:
            return graphData.getSubscriptionsSortedByTime(
                    imageMgr.getNetwork(), imageMgr.getSortTimeMillis(), false);
        default:
            return new ArrayList<String>();
        }
    }

    /**
     * Clear the canvas selection.
     */
    public void clearCanvasSelection() {
        // Do nothing. Override to use.
    }

    /**
     * Dispose the resources.
     */
    public abstract void disposeResources();

    /**
     * Draw the image.
     */
    public abstract void drawImage();
}
