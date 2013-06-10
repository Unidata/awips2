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

package com.raytheon.viz.ui.tools;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Defines a Tool Manager, which handles the tool registrations and activations
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 7/1/06                   chammack    Initial Creation.
 * 7/2/07                   chammack    Update for Eclipse 3.3
 * 27Jun08      #1181       ebabin      Update for Skewt edit break.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ModalToolManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(ModalToolManager.class);

    private Map<String, AbstractModalTool> toolMap = new HashMap<String, AbstractModalTool>();

    /**
     * Sets a modal tool to be selected
     * 
     * All other tools are set to be unselected
     * 
     * @param aTool
     */
    public synchronized void selectModalTool(AbstractModalTool aTool) {
        if (aTool == null) {
            return;
        }
        AbstractModalTool lastModalTool = toolMap.get(aTool.categoryId);
        if (lastModalTool != null && aTool != lastModalTool) {
            lastModalTool.deactivate();
        }
        toolMap.put(aTool.categoryId, aTool);
    }

    /**
     * Unsets a modal tool to be selected
     * 
     * @param aTool
     */
    public synchronized void deselectModalTool(AbstractModalTool aTool) {
        if (aTool == null) {
            return;
        }
        toolMap.remove(aTool.categoryId);
        aTool.deactivate();
    }

    /**
     * Get the last selected modal tool
     * 
     * @return modal tool or null if none selected
     */
    public Collection<AbstractModalTool> getSelectedModalTools() {
        return toolMap.values();
    }

    /**
     * Get the last selected modal tool for the id
     * 
     * @return modal tool or null if none selected
     */
    public AbstractModalTool getSelectedModalTool(String id) {
        return toolMap.get(id);
    }

    /**
     * Activate the default tool
     * 
     */
    public synchronized void activateToolSet(String defaultTool)
            throws VizException {
        boolean found = false;
        for (AbstractModalTool tool : toolMap.values()) {
            if (tool != null && tool.commandId.equals(defaultTool)) {
                found = true;
                break;
            }
        }
        if (!found) {
            try {
                ICommandService service = (ICommandService) PlatformUI
                        .getWorkbench().getService(ICommandService.class);
                if (defaultTool != null) {
                    Command c = service.getCommand(defaultTool);
                    c.executeWithChecks(new ExecutionEvent(c,
                            new HashMap<Object, Object>(), null, null));
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.CRITICAL,
                        "Error loading tool set", e);

                throw new VizException("Error loading tool set", e);
            }
        }
    }

}
