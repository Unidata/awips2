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
package com.raytheon.uf.viz.core.status;

import org.eclipse.ui.statushandlers.StatusManager;

import com.raytheon.uf.common.status.AbstractHandlerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Sends StatusMessages into the Eclipse Status Manager
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 6, 2008  1433       chammack    Initial creation
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class VizStatusHandler implements IUFStatusHandler {

    private final String category;

    private String source;

    private final String pluginId;

    private final AbstractHandlerFactory factory;

    public VizStatusHandler(AbstractHandlerFactory factory, String pluginId,
            String category) {
        this.factory = factory;
        this.category = category;
        this.pluginId = pluginId;
    }

    public VizStatusHandler(String pluginId, String category, String source) {
        this.category = category;
        this.source = source;
        this.pluginId = pluginId;
        this.factory = null;
    }

    @Override
    public boolean isPriorityEnabled(Priority p) {
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.status.IUFStatusHandler#handle(com.raytheon.uf
     * .common.status.UFStatus)
     */
    @Override
    public void handle(UFStatus status) {
        handle(status, this.category);
    }

    @Override
    public void handle(UFStatus status, String category) {
        if (this.source == null) {
            if (factory != null) {
                this.source = factory.getSource(source, pluginId);
            }
        }

        VizStatusInternal vizStatus = new VizStatusInternal(status, category,
                source, pluginId);
        StatusManager.getManager().handle(vizStatus);
    }

    @Override
    public void handle(Priority p, String msg) {
        handle(new UFStatus(p, msg));
    }

    @Override
    public void handle(Priority priority, String category, String message) {
        handle(priority, category, message, (Throwable) null);
    }

    @Override
    public void handle(Priority p, String msg, Throwable t) {
        handle(new UFStatus(p, msg, t));
    }

    @Override
    public void handle(Priority p, String category, String msg, Throwable t) {
        handle(new UFStatus(p, msg, t), category);
    }

    @Override
    public void debug(String message) {
        handle(Priority.DEBUG, message);
    }

    @Override
    public void debug(String category, String message) {
        handle(Priority.DEBUG, category, message);
    }

    @Override
    public void info(String message) {
        handle(Priority.INFO, message);
    }

    @Override
    public void info(String category, String message) {
        handle(Priority.INFO, category, message);
    }

    @Override
    public void warn(String message) {
        handle(Priority.WARN, message);
    }

    @Override
    public void warn(String category, String message) {
        handle(Priority.WARN, category, message);
    }

    @Override
    public void error(String message) {
        handle(Priority.ERROR, message);
    }

    @Override
    public void error(String category, String message) {
        handle(Priority.ERROR, category, message);
    }

    @Override
    public void error(String message, Throwable throwable) {
        handle(Priority.ERROR, message, throwable);
    }

    @Override
    public void error(String message, String category, Throwable throwable) {
        handle(Priority.ERROR, category, message, throwable);
    }

    @Override
    public void fatal(String message, Throwable throwable) {
        handle(Priority.FATAL, message, throwable);
    }

    @Override
    public void fatal(String message, String category, Throwable throwable) {
        handle(Priority.FATAL, category, message, throwable);
    }

}
