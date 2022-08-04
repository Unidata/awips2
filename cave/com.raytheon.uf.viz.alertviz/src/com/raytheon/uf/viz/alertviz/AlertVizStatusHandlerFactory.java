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

import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.status.AbstractHandlerFactory;
import com.raytheon.uf.common.status.FilterPatternContainer;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.StatusHandler;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * AlertViz Status Handler Factory
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 12, 2018 7515       randerso     Initial creation
 *
 * </pre>
 *
 * @author randerso
 */
public class AlertVizStatusHandlerFactory extends AbstractHandlerFactory {
    private static final String CATEGORY = "WORKSTATION";

    private static final StatusHandler instance = new StatusHandler(
            StatusHandler.class.getPackage().getName(), CATEGORY, CATEGORY);

    /**
     * @param category
     */
    public AlertVizStatusHandlerFactory() {
        super(CATEGORY);
    }

    @Override
    public IUFStatusHandler getInstance() {
        return instance;
    }

    @Override
    public IUFStatusHandler createInstance(String pluginId, String category,
            String source) {
        return new StatusHandler(pluginId, category, source);
    }

    @Override
    public IUFStatusHandler createInstance(AbstractHandlerFactory factory,
            String pluginId, String category) {
        return new StatusHandler(pluginId, category, getSource(null, pluginId));
    }

    @Override
    protected IUFStatusHandler createMonitorInstance(String pluginId,
            String monitorSource) {
        return null;
    }

    @Override
    protected FilterPatternContainer createSourceContainer() {
        return FilterPatternContainer.createDefault();
    }

    @Override
    protected void log(Priority priority, String pluginId, String category,
            String source, String message, Throwable throwable) {

        // create StatusMesage and route into AlertViz
        StatusMessage sm = new StatusMessage(source, category, priority,
                pluginId, message, throwable);
        AlertvizJob.getInstance().receive(sm);
    }

}
