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

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryGUIUtils.SubscriptionPriority;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 4, 2012     1269    mpduff      Initial creation.
 * Dec 13, 2012   1269     lvenable    Fixes and updates.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public interface IGraphOptions {
    /**
     * Get the show subscription lines flag.
     * 
     * @return true if subscription lines should be drawn
     */
    boolean isShowSubscriptionLines();

    /**
     * Set the show subscription lines flag.
     * 
     * @param showSubLines
     */
    void setShowSubscriptionLines(boolean showSubLines);

    /**
     * Get the color by priority flag.
     * 
     * @return true if coloring by priority
     */
    boolean isColorByPriority();

    /**
     * Set the color by priority flag.
     * 
     * @param colorByPriority
     */
    void setColorByPriority(boolean colorByPriority);

    /**
     * Get the live update flag.
     * 
     * @return true if dialog auto-updates
     */
    boolean isLiveUpdate();

    /**
     * Set the live update flag.
     * 
     * @param autoUpdate
     */
    void setLiveUpdate(boolean autoUpdate);

    /**
     * Get the current time in millis
     * 
     * @return the current time
     */
    long getCurrentTimeMillis();

    /**
     * Get the RGB color associated with the specified priority.
     * 
     * @param priority
     *            Priority.
     * @return RGB color.
     */
    RGB getPriorityColor(SubscriptionPriority priority);

    /**
     * Set the color associated with the specified priority.
     * 
     * @param priority
     *            Priority.
     * @param rgb
     *            RGB color.
     */
    void setPriorityColor(SubscriptionPriority priority, RGB rgb);
}
