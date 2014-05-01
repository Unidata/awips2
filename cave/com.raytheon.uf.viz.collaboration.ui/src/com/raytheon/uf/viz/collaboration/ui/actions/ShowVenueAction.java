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
package com.raytheon.uf.viz.collaboration.ui.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.ui.IViewReference;

import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.viz.ui.views.CaveWorkbenchPageManager;

/**
 * Bring a view for the given venue to the front of the display.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 5, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class ShowVenueAction extends Action {

    private final IVenueSession session;

    public ShowVenueAction(IVenueSession session) {
        super("Open Session");
        this.session = session;
    }

    @Override
    public void run() {
        for (IViewReference ref : CaveWorkbenchPageManager.getActiveInstance()
                .getViewReferences()) {
            if (session.getSessionId().equals(ref.getSecondaryId())) {
                CaveWorkbenchPageManager.getActiveInstance().activate(
                        ref.getView(true));
            }
        }
    }

}
