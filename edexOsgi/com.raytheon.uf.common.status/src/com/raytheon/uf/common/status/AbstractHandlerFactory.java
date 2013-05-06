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

import java.util.HashMap;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;

/**
 * Abstract implementation of the Handler Factories.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 10, 2011            bgonzale     Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public abstract class AbstractHandlerFactory implements
        IUFStatusHandlerFactory, Observer {

    private final String defaultCategory;

    private final Map<String, IUFStatusHandler> namedHandlers = new HashMap<String, IUFStatusHandler>();

    private FilterPatternContainer sourceFilters;

    /**
     * 
     */
    public AbstractHandlerFactory(String category) {
        this.defaultCategory = category;
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
        return namedHandlers.get(name);
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
        IUFStatusHandler handler = namedHandlers.get(name);

        if (handler == null) {
            handler = getInstance(name, null, name);
            namedHandlers.put(name, handler);
        }
        return handler;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.status.IUFStatusHandlerFactory#getInstance(java
     * .lang.Class)
     */
    @Override
    public IUFStatusHandler getInstance(Class<?> cls) {
        return getInstance(cls, null, null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.status.IUFStatusHandlerFactory#getInstance(java
     * .lang.Class, java.lang.String)
     */
    @Override
    public IUFStatusHandler getInstance(Class<?> cls, String source) {
        return getInstance(cls, null, source);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.status.IUFStatusHandlerFactory#getInstance(java
     * .lang.Class, java.lang.String, java.lang.String)
     */
    @Override
    public IUFStatusHandler getInstance(Class<?> cls, String category,
            String source) {
        String pluginId = cls.getName();
        return getInstance(pluginId, category, source);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.status.IUFStatusHandlerFactory#getInstance(java
     * .lang.String, java.lang.String)
     */
    @Override
    public IUFStatusHandler getInstance(String pluginId, String source) {
        return getInstance(pluginId, null, source);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.status.IUFStatusHandlerFactory#getInstance(java
     * .lang.String, java.lang.String, java.lang.String)
     */
    @Override
    public IUFStatusHandler getInstance(String pluginId, String category,
            String source) {
        if (source == null) {
            return createInstance(this, pluginId, getCategory(category));
        } else {
            return createInstance(pluginId, getCategory(category), source);
        }
    }

    public String getSource(String source, String pluginId) {
        if (source == null) {
            source = getSource(pluginId);
        }
        return source;
    }

    public String getCategory(String category) {
        return category == null ? defaultCategory : category;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.status.IUFStatusHandlerFactory#getMonitorInstance
     * (java.lang.Class)
     */
    @Override
    public IUFStatusHandler getMonitorInstance(Class<?> cls) {
        return getMonitorInstance(cls, null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.status.IUFStatusHandlerFactory#getMonitorInstance
     * (java.lang.Class, java.lang.String)
     */
    @Override
    public IUFStatusHandler getMonitorInstance(Class<?> cls,
            String monitorSource) {
        String pluginId = cls.getName();
        monitorSource = (monitorSource == null) ? getSource(pluginId)
                : monitorSource;
        return createMonitorInstance(pluginId, monitorSource);
    }

    private String getSource(String pluginId) {
        String source = null;
        FilterPattern fPattern = getSourceFilters().findFilter(pluginId);

        if (fPattern != null) {
            source = fPattern.getName();
        }
        return source;
    }

    private FilterPatternContainer getSourceFilters() {
        if (sourceFilters == null) {
            sourceFilters = createSourceContainer();
        }
        return sourceFilters;
    }

    @Override
    public void update(Observable o, Object arg) {
        getSourceFilters();
    }

    protected abstract IUFStatusHandler createMonitorInstance(String pluginId,
            String monitorSource);

    protected abstract IUFStatusHandler createInstance(String pluginId,
            String category, String source);

    protected abstract FilterPatternContainer createSourceContainer();
}
