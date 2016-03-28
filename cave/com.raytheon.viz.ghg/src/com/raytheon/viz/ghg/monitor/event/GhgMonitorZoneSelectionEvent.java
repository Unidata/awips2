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
package com.raytheon.viz.ghg.monitor.event;

import java.util.Collection;
import java.util.Collections;

/**
 * Ghg Monitor map selection event object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 02, 2010            mpduff      Initial creation
 * Feb 05, 2016   #5316    randerso    Changed to extend AbstractGhgMonitorEvent
 *                                     removed unnecessary field
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GhgMonitorZoneSelectionEvent extends AbstractGhgMonitorEvent {

    private Collection<String> highlightedZones = Collections.emptyList();

    /**
     * @param source
     */
    public GhgMonitorZoneSelectionEvent() {
        super();
    }

    /**
     * @return the highlightedZones
     */
    public Collection<String> getHighlightedZones() {
        return highlightedZones;
    }

    /**
     * @param highlightedZones
     *            the highlightedZones to set
     */
    public void setHighlightedZones(Collection<String> highlightedZones) {
        this.highlightedZones = highlightedZones;
    }
}
