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
package com.raytheon.uf.common.status;


/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 15, 2011            randerso     Initial creation
 * Oct 23, 2013 2303       bgonzale     Merged VizStatusHandler and SysErrStatusHandler into StatusHandler.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class DefaultStatusHandlerFactory extends
        AbstractHandlerFactory {
    private static final IUFStatusHandler handler = new StatusHandler(
            DefaultStatusHandlerFactory.class.getPackage().getName(),
            "DEFAULT", "DEFAULT");

    private static final String CATEGORY = "DEFAULT";


    public DefaultStatusHandlerFactory() {
        super(CATEGORY);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.status.IUFStatusHandlerFactory#getInstance()
     */
    @Override
    public IUFStatusHandler getInstance() {
        return handler;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.status.IUFStatusHandlerFactory#getInstance(java
     * .lang.String)
     */
    @Override
    public IUFStatusHandler getInstance(String name) {
        return handler;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.status.IUFStatusHandlerFactory#hasHandler(java
     * .lang.String)
     */
    @Override
    public IUFStatusHandler hasHandler(String name) {
        return handler;
    }

    @Override
    protected IUFStatusHandler createMonitorInstance(String pluginId,
            String monitorSource) {
        return null;
    }

    @Override
    protected IUFStatusHandler createInstance(String pluginId, String category,
            String source) {
        return new StatusHandler(category, source,
                pluginId);
    }

    @Override
    protected FilterPatternContainer createSourceContainer() {
        return FilterPatternContainer.createDefault();
    }

    @Override
    public IUFStatusHandler createInstance(AbstractHandlerFactory factory,
            String pluginId, String category) {
        return new StatusHandler(category, CATEGORY,
                pluginId);
    }

}
