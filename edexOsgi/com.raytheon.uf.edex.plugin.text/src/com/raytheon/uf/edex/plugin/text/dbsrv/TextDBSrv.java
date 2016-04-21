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
package com.raytheon.uf.edex.plugin.text.dbsrv;

import com.raytheon.uf.common.dataplugin.text.dbsrv.ICommandExecutor;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.plugin.text.dbsrv.impl.CommandExecutor;

/**
 * Service for processing textdb queries
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 03, 2008 1538       jkorman     Initial implementation.
 * Mar 26, 2014 2835       rjpeter     Added logging.
 * May 15, 2014 2536       bclement    moved from uf.edex.textdbsrv
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class TextDBSrv {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(TextDBSrv.class);

    private static Integer instanceId = 0;

    private ICommandExecutor executor = null;

    public TextDBSrv() {
        super();
        synchronized (instanceId) {
            instanceId = instanceId + 1;
        }
        executor = new CommandExecutor();
    }

    /**
     * Processes a textdb message.
     * 
     * @param message
     * @return
     */
    public Message processMessage(Message message) {
        Message returnMessage = null;
        try {
            if (message != null) {
                returnMessage = executeMessage(message);

                if (returnMessage == null) {
                    returnMessage = CommandExecutor
                            .createErrorMessage("ERROR:Null return from execute");
                }
            } else {
                String errMsg = "Message content was null";
                returnMessage = CommandExecutor.createErrorMessage(errMsg);
            }
        } catch (Exception e) {
            returnMessage = CommandExecutor
                    .createErrorMessage("Processing of message failed: "
                            + e.getLocalizedMessage());
            statusHandler.error("Processing of message failed", e);
        }

        return returnMessage;
    }

    /**
     * 
     * @param command
     *            A command to execute.
     */
    public Message executeMessage(Message command) {
        return executor.execute(command);
    }

    /**
     * Execute an arbitrary string command.
     * 
     * @param command
     *            A command to execute.
     */
    public void executeString(String command) {
        executeCommand(command);
    }

    /**
     * Execute an arbitrary string command. This method the the actual execution
     * agent.
     * 
     * @param command
     *            A command to execute.
     */
    private synchronized void executeCommand(String command) {
        if ("read".equals(command)) {
            statusHandler.info("Processing command");
        }
    }
}
