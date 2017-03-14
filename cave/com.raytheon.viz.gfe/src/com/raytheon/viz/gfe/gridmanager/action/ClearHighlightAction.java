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
package com.raytheon.viz.gfe.gridmanager.action;

import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.core.msgs.HighlightMsg;
import com.raytheon.viz.gfe.core.parm.Parm;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 3, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ClearHighlightAction extends AbstractGridManagerAction {

    private Parm parm;

    private TimeRange timeRange;

    private String color;

    /**
     * @param text
     */
    public ClearHighlightAction(Parm parm, TimeRange timeRange, String color) {
        super("Clear Highlight");
        this.parm = parm;
        this.timeRange = timeRange;
        this.color = color;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        new HighlightMsg(parm, new TimeRange[] { timeRange }, false, color)
                .send();
    }
}
