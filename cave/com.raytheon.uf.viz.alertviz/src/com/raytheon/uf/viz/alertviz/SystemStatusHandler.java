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
package com.raytheon.uf.viz.alertviz;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.statushandlers.AbstractStatusHandler;
import org.eclipse.ui.statushandlers.StatusAdapter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.Marker;
import org.slf4j.MarkerFactory;

import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.alertviz.config.Category;
import com.raytheon.uf.viz.alertviz.internal.LogMessageDAO;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.status.VizStatusInternal;

/**
 * Implements status handling by converting status messages into StatusMessages
 * and sending them to alertviz.
 * 
 * Also logs to a file so the error can be traced to the specific process id and
 * as a safety net in case alertviz cannot be reached.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 09, 2008 1433       chammack    Initial creation
 * Aug 26, 2013 2142       njensen     Changed to use SLF4J
 * Jul 02, 2014 3337       njensen     Disabled logback packaging data
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class SystemStatusHandler extends AbstractStatusHandler {

    private transient static final Logger logger = LoggerFactory
            .getLogger("CaveLogger");

    private static final String WORKSTATION = "WORKSTATION";

    private static final Marker FATAL = MarkerFactory.getMarker("FATAL");

    static {
        /*
         * Disables the packaging data feature of logback (ie how the
         * stacktraces list the jar the class is in). Due to the viz dependency
         * tree, in some scenarios the determination of the packaging data can
         * spend an inordinate amount of time in the OSGi classloader trying to
         * find classes. If the viz dependency tree is cleaned up (ie
         * modularized, unnecessary imports removed, register buddies reduced)
         * then this may be able to be re-enabled without a performance hit.
         * 
         * Unfortunately there is no way to do this other than casting to a
         * logback Logger, see http://jira.qos.ch/browse/LOGBACK-730 and
         * http://jira.qos.ch/browse/LOGBACK-899
         */
        try {
            ((ch.qos.logback.classic.Logger) logger).getLoggerContext()
                    .setPackagingDataEnabled(false);
        } catch (Throwable t) {
            /*
             * given that this static block is for initializing the logger
             * correctly, if that went wrong let's not even try to "log" it,
             * just use stderr
             */
            System.err.println("Error disabling logback packaging data");
            t.printStackTrace();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.statushandlers.AbstractStatusHandler#handle(org.eclipse
     * .ui.statushandlers.StatusAdapter, int)
     */
    @Override
    public void handle(StatusAdapter statusAdapter, int style) {
        final IStatus status = statusAdapter.getStatus();
        StatusMessage sm = null;

        if (status instanceof VizStatusInternal) {
            VizStatusInternal vs = (VizStatusInternal) status;
            sm = vs.toStatusMessage();
        } else {
            sm = from(status);

        }

        Priority p = sm.getPriority();
        String msg = status.getMessage();
        Throwable t = status.getException();
        logStatus(p, msg, t);

        try {
            AlertVizClient.sendMessage(sm);
        } catch (final AlertvizException e) {
            // not a good situation, since we can't communicate with the log
            // server properly

            // log to internal log4j log
            Container.logInternal(Priority.CRITICAL,
                    "SystemStatusHandler: exception sending message: " + sm, e);

            // DO NOT SEND TO LOG HERE OR INFINITE LOOPS MAY OCCUR
            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    ErrorDialog.openError(
                            Display.getDefault().getActiveShell(),
                            "Error communicating with log server",
                            "Error communicating with log server "
                                    + e.getCause().getMessage(), status);
                }

            });

        }

    }

    /**
     * Construct a StatusMessage from a generic Eclipse IStatus
     * 
     * @param status
     * @return
     */
    public static StatusMessage from(IStatus status) {
        StatusMessage sm = new StatusMessage();
        sm.setCategory(WORKSTATION);

        sm.setMachineToCurrent();
        switch (status.getSeverity()) {
        case Status.ERROR:
            sm.setPriority(Priority.SIGNIFICANT);
            break;
        case Status.WARNING:
            sm.setPriority(Priority.INFO);
            break;
        default:
            sm.setPriority(Priority.VERBOSE);
        }
        sm.setSourceKey(WORKSTATION);
        StatusMessage.buildMessageAndDetails(status.getMessage(),
                status.getException(), sm);

        return sm;
    }

    /**
     * Acknowledge the message
     * 
     * NOTE: This only works when called from inside the alertviz container
     * 
     * @param username
     *            the username of who acknowledged the message
     * @throws AlertvizException
     */
    public static void acknowledge(StatusMessage sm, String username)
            throws AlertvizException {
        LogMessageDAO.getInstance().acknowledge(sm, username);
    }

    /**
     * Retrieve a status message by primary key
     * 
     * NOTE: This only works when called from inside the alertviz container
     * 
     * @param pk
     *            the primary key
     * @return the status message
     * @throws AlertvizException
     */
    public static StatusMessage retrieveByPk(int pk) throws AlertvizException {
        return LogMessageDAO.getInstance().loadByPk(pk);
    }

    /**
     * Get the current range of persisted StatusMessages
     * 
     * NOTE: This only works when called from inside the alertviz container
     * 
     * 
     * @return the range [min, max] of values
     * @throws AlertvizException
     */
    public static int[] getCurrentRange() throws AlertvizException {
        return LogMessageDAO.getInstance().getLogRange();
    }

    /**
     * Retrieve a list the last count messages of a set of specifies category
     * 
     * @param category
     *            the category
     * @param count
     *            the number of results
     * @return an array of status messages
     * @throws AlertvizException
     */
    public static StatusMessage[] retrieveByCategory(Category[] category,
            int count) throws AlertvizException {
        return LogMessageDAO.getInstance().load(count, category);
    }

    private void logStatus(Priority priority, String message, Throwable t) {
        switch (priority) {
        case CRITICAL:
            logger.error(FATAL, message, t);
            break;
        case SIGNIFICANT:
            logger.error(message, t);
            break;
        case PROBLEM:
            logger.warn(message, t);
            break;
        case EVENTA: // fall through
        case EVENTB:
            logger.info(message, t);
            break;
        case VERBOSE:
            logger.debug(message, t);
            break;
        }
    }

}
