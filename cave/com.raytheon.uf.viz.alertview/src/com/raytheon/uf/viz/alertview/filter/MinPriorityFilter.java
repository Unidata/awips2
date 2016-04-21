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
package com.raytheon.uf.viz.alertview.filter;

import com.raytheon.uf.viz.alertview.Alert;
import com.raytheon.uf.viz.alertview.Alert.Priority;

/**
 * An {@link AlertFilter} that returns true for {@link Alert}s that have a
 * priority equal to or above a specified level.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 17, 2015  4474     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class MinPriorityFilter implements AlertFilter {

    private final Priority priority;

    public MinPriorityFilter(Priority priority) {
        this.priority = priority;
    }

    @Override
    public boolean filter(Alert alert) {
        return alert.getPriority().ordinal() >= priority.ordinal();
    }

}
