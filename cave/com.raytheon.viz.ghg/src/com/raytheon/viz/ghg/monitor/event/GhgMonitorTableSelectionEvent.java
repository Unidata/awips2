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

import java.util.EventObject;
import java.util.List;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.viz.ghg.monitor.data.GhgData;

/**
 * Ghg Monitor table(spreadsheet) selection event object.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 2, 2010            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class GhgMonitorTableSelectionEvent extends EventObject {

    private static final long serialVersionUID = 3679672253157750734L;
    
    private List<GhgData> ghgData;

    private String[] highlightedZones = null;
    
    private RGB selectionColor = null;
    
    /**
     * @param source
     */
    public GhgMonitorTableSelectionEvent(Object source) {
        super(source);
    }
    
    /**
     * @param highlightedZones the highlightedZones to set
     */
    public void setHighlightedZones(String[] highlightedZones) {
        this.highlightedZones = highlightedZones;
    }
    
    /**
     * @return the highlightedZones
     */
    public String[] getHighlightedZones() {
        return highlightedZones;
    }

    /**
     * @return the ghgData
     */
    public List<GhgData> getGhgData() {
        return ghgData;
    }

    /**
     * @param ghgData the ghgData to set
     */
    public void setGhgData(List<GhgData> ghgData) {
        this.ghgData = ghgData;
    }

    /**
     * @return the selectionColor
     */
    public RGB getSelectionColor() {
        return selectionColor;
    }

    /**
     * @param selectionColor the selectionColor to set
     */
    public void setSelectionColor(RGB selectionColor) {
        this.selectionColor = selectionColor;
    }
}
