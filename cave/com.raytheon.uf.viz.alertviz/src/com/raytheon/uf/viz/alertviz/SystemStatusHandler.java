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
import org.eclipse.ui.statushandlers.AbstractStatusHandler;
import org.eclipse.ui.statushandlers.StatusAdapter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.Marker;

import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.status.slf4j.UFMarkers;
import com.raytheon.uf.viz.alertviz.config.Category;
import com.raytheon.uf.viz.alertviz.internal.LogMessageDAO;

/**
 * Implements status handling by converting Eclipse status messages and sending
 * them to SLF4J. Only Eclipse logging should go through this class.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 09, 2008 1433       chammack    Initial creation
 * Aug 26, 2013 2142       njensen     Changed to use SLF4J
 * Jul 02, 2014 3337       njensen     Disabled logback packaging data
 * May 22, 2015 4473       njensen     Refactored
 * Feb 11, 2016 5314       dgilling    Add retrieveByRowOffset.
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class SystemStatusHandler extends AbstractStatusHandler {

    /*
     * TODO This should perhaps be changed to Eclipse since only Eclipse is
     * coming through here now. Workstation maintains how it used to work
     * though.
     */
    private static final String CATEGORY = "WORKSTATION";

    private transient static final Logger logger = LoggerFactory
            .getLogger("CaveLogger");

    @Override
    public void handle(StatusAdapter statusAdapter, int style) {
        final IStatus status = statusAdapter.getStatus();
        String msg = status.getMessage();
        Throwable t = status.getException();
        Marker m = UFMarkers.getCategoryMarker(CATEGORY);
        m.add(UFMarkers.getSourceMarker(CATEGORY));
        String plugin = status.getPlugin();
        if (plugin != null) {
            m.add(UFMarkers.getPluginMarker(plugin));
        }

        switch (status.getSeverity()) {
        case IStatus.CANCEL:
        case IStatus.ERROR:
            logger.error(m, msg, t);
            break;
        case IStatus.WARNING:
            logger.warn(m, msg, t);
            break;
        case IStatus.INFO:
            logger.info(m, msg, t);
            break;
        case IStatus.OK:
            logger.debug(m, msg, t);
            break;
        }

    }

    // TODO move static methods below elsewhere, they don't belong here

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
     * Retrieve a status message by row offset
     * 
     * @param index
     *            The row offset, 0-based index.
     * @return the status message
     * @throws AlertvizException
     *             If an error occurred querying the database for the status
     *             message
     */
    public static StatusMessage retrieveByRowOffset(int index)
            throws AlertvizException {
        return LogMessageDAO.getInstance().loadByRowOffset(index);
    }

    /**
     * Get the current number of persisted StatusMessages.
     * <p>
     * NOTE: This only works when called from inside the alertviz container
     * 
     * @return
     * @throws AlertvizException
     */
    public static int getMessageCount() throws AlertvizException {
        return LogMessageDAO.getInstance().getMessageCount();
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
}
