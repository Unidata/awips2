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
package com.raytheon.uf.viz.d2d.ui.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.State;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.handlers.RegistryToggleState;

/**
 * Toggle the time options action command, which will cause the time options
 * dialog to open when the next product is loaded.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Apr 08, 2009  623      bgonzale  Initial creation
 * Apr 01, 2016  5531     bsteffen  Rewrite
 * 
 * </pre>
 * 
 * @author bgonzale
 */

public class TimeOptionsAction extends AbstractHandler {

    private static final String COMMAND_ID = "com.raytheon.uf.viz.d2d.ui.actions.timeOptionsAction";
    
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        return HandlerUtil.toggleCommandState(event.getCommand());
    }

    public static void setTimeOptionsSelected(boolean selected) {
        getTimeOptionsToggleState().setValue(Boolean.valueOf(selected));
    }

    public static boolean isTimeOptionsSelected() {
        return ((Boolean) getTimeOptionsToggleState().getValue())
                .booleanValue();
    }

    private static State getTimeOptionsToggleState() {
        ICommandService commandService = (ICommandService) PlatformUI
                .getWorkbench().getService(ICommandService.class);
        Command command = commandService.getCommand(COMMAND_ID);
        return command.getState(RegistryToggleState.STATE_ID);
    }

}
