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
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer         Description
 * ------------ ---------- ---------------- --------------------------
 * 03/30/11                randerso         Initial creation
 * 01/16/13     DR15722    jzeng/randerso   override send() to
 *                                          force the UI to update the button 
 *                                          whenever the message is sent. 
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ShowISCGridsMsg extends Message {
    private boolean showIscGrids;

    public ShowISCGridsMsg() {
        this(false);
    }

    public ShowISCGridsMsg(boolean show) {
        showIscGrids = show;
    }

    public boolean show() {
        return showIscGrids;
    }
    
    @Override
    public void send() {
        super.send();

        ICommandService service = (ICommandService) PlatformUI.getWorkbench()
                .getService(ICommandService.class);
        service.refreshElements("com.raytheon.viz.gfe.actions.showISCGrids",
                null);
    }

}
