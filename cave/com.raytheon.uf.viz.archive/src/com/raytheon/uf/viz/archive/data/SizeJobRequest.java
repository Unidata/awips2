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
package com.raytheon.uf.viz.archive.data;

import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import com.raytheon.uf.common.archive.config.DisplayData;

/**
 * This class obtains information on a File in a Job in order to remove it from
 * the UI thread.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 15, 2013 1966       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class SizeJobRequest {

    /** Information from archive configuration manager. */
    final DisplayData displayData;

    /** Files or directories to obtain information on. */
    final List<File> files = new ArrayList<File>();

    /** Start time inclusive. */
    final Calendar startCal;

    /** End time exclusive. */
    final Calendar endCal;

    /**
     * Create and entry and place it on the queue.
     * 
     * @param displayData
     * @param startCal
     * @param endCal
     */
    public SizeJobRequest(DisplayData displayData, Calendar startCal,
            Calendar endCal) {
        this.displayData = displayData;
        this.startCal = startCal;
        this.endCal = endCal;
    }

    /**
     * 
     * @return displayData
     */
    public DisplayData getDisplayData() {
        return displayData;
    }
}
