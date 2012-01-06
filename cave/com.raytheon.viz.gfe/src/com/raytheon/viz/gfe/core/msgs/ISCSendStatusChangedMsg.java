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
package com.raytheon.viz.gfe.core.msgs;

import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;

/**
 * Message indicate ISC Send Status has changed
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 25, 2010            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ISCSendStatusChangedMsg extends Message {
    private static final String COMMAND_ID = "com.raytheon.viz.gfe.iscSendEnable";

    private boolean enabled;

    public ISCSendStatusChangedMsg() {
        // TODO: determine initial state
    }

    public ISCSendStatusChangedMsg(boolean enabled) {
        this.enabled = enabled;
    }

    public boolean isEnabled() {
        return enabled;
    }

    @Override
    public void send() {
        super.send();

        if (PlatformUI.isWorkbenchRunning()) {
            ICommandService service = (ICommandService) PlatformUI
                    .getWorkbench().getActiveWorkbenchWindow().getService(
                            ICommandService.class);

            service.refreshElements(COMMAND_ID, null);
        }
    }
}
