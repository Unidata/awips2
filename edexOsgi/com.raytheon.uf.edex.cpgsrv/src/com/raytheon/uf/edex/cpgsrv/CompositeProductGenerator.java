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
package com.raytheon.uf.edex.cpgsrv;

import java.util.concurrent.Executor;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.msg.DataURINotificationMessage;
import com.raytheon.edex.urifilter.IURIFilter;
import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.edex.urifilter.URIGenerateMessage;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.monitor.cpg.MonitorStateConfigurationManager;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.stats.ProcessEvent;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.event.EventBus;

/**
 * CompositeProductGenerator
 * 
 * Abstract class used in producing Composite products, More or less a factory
 * for CPG's. As messages come in it will grab the existing instance of the
 * URIFilter and CPG and check to see if it matches the URI's looked for, if so
 * generateProduct gets called with the list of URI's.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/07/2009   1981       dhladky    Initial Creation.
 * 30NOV2012    1372       dhladky    Added statistics
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public abstract class CompositeProductGenerator implements
        ICompositeProductGenerator, IURIFilter {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CompositeProductGenerator.class);

    /** plugin name of what's produced */
    public String compositeProductType = null;

    public PluginDataObject[] pdos = null;

    /**
     * The plugins DAO object
     */
    public PluginDao dao = null;

    /** Product produced URI */
    public String productURI = null;

    /** Your URIFilter object */
    public URIFilter[] filters = null;

    /** Data Access Object DB conn */
    public CoreDao cdao = null;

    /** Generator Name */
    public String name = null;

    /** Generated files productTime */
    public DataTime productTime = null;

    /** The logger */
    public final Log logger = LogFactory.getLog(getClass());

    /** monitor config **/
    public MonitorStateConfigurationManager msc = null;

    /** thread executor **/
    public Executor executor = null;

    protected String routeId = null;

    protected static final EventBus eventBus = EventBus.getInstance();

    public CompositeProductGenerator(String name, String compositeProductType) {
        this(name, compositeProductType, null);
    }

    /**
     * Public abstract constructor for CPG
     * 
     * @param name
     * @param compositeProductType
     * @param executor
     */
    public CompositeProductGenerator(String name, String compositeProductType,
            Executor executor) {
        // create CPG
        if (isRunning()) {
            setGeneratorName(name);
            setCompositeProductType(compositeProductType);
            setExecutor(executor);
            configureFilters();
            createFilters();
        }

        routeId = getGeneratorName() + "Generate";
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.urifilter.IURIFilter#matchURIs(com.raytheon.edex.msg
     * .DataURINotificationMessage)
     */
    @Override
    public void matchURIs(DataURINotificationMessage messages) {

        if (messages instanceof DataURINotificationMessage) {
            URIFilter[] filters = getFilters();
            if (filters != null) {
                for (URIFilter filter : filters) {
                    synchronized (filter) {
                        if (filter.isMatched(messages)) {
                            try {
                                EDEXUtil.getMessageProducer()
                                        .sendAsync(
                                                routeId,
                                                SerializationUtil
                                                        .transformToThrift(filter
                                                                .createGenerateMessage()));
                            } catch (Exception e) {
                                logger.error(
                                        getGeneratorName()
                                                + ": filter: "
                                                + filter.getName()
                                                + ": failed to route filter to generator",
                                        e);
                            }

                            filter.reset();
                        }
                    }
                }
            }
        }
    }

    /**
     * Generate a product
     * 
     * @param filter
     */
    public void generate(URIGenerateMessage genMessage) {
        try {
            genMessage.setDeQueuedTime(System.currentTimeMillis());
            setProductTime(genMessage);
            generateProduct(genMessage);
            persistRecords();
            fireTopicUpdate();
            log(genMessage);

        } catch (Throwable t) {
            statusHandler.handle(Priority.ERROR, "CPG encountered an error", t);
        } finally {
            clear();
        }
    }

    /**
     * plugin name for composite product
     */
    @Override
    public String getCompositeProductType() {
        return compositeProductType;
    }

    /**
     * plugin name for composite product
     */
    @Override
    public void setCompositeProductType(String compositeProductType) {
        this.compositeProductType = compositeProductType;
    }

    /**
     * Handled in the subclass, creates the return data object. Up to the
     * extending class what this is.
     */
    @Override
    public abstract void generateProduct(URIGenerateMessage genMessage);

    /**
     * Creates a filter for your type of data.
     * 
     * @return
     */
    protected abstract void createFilters();

    /**
     * take care of any special configuration things
     */
    protected abstract void configureFilters();

    /**
     * reconstruct filters on config changes
     */
    public void resetFilters() {
        filters = null;
        configureFilters();
        createFilters();
    }

    /**
     * Secure the time for the new product being created
     * 
     * @return
     */
    @Override
    public DataTime getProductTime() {
        return productTime;
    }

    /**
     * Sets the productTime
     */
    @Override
    public void setProductTime(URIGenerateMessage genMessage) {
        if (genMessage.getValidTime() != null) {
            productTime = new DataTime(genMessage.getValidTime());
        } else {
            productTime = new DataTime(genMessage.getCurrentTime());
        }
    }

    /**
     * Get the URIFilter[]
     * 
     * @return
     */
    public URIFilter[] getFilters() {
        return filters;
    }

    /**
     * Gets the Generators name.
     * 
     * @return
     */
    public String getGeneratorName() {
        return name;
    }

    /**
     * Set the generator name.
     * 
     * @param name
     */
    public void setGeneratorName(String name) {
        this.name = name;
    }

    /**
     * Make a DB request
     * 
     * @param sql
     * @return
     */
    public Object[] dbRequest(String sql) {
        if (logger.isDebugEnabled()) {
            logger.debug(getGeneratorName() + ":  SQL to run: " + sql);
        }

        Object[] results = null;
        try {
            if (cdao == null) {
                try {
                    cdao = new CoreDao(DaoConfig.DEFAULT);
                } catch (Exception ed1) {
                    logger.error(getGeneratorName()
                            + " Core DAO access failed. " + ed1);
                }
            }
            results = cdao.executeSQLQuery(sql);

        } catch (Exception ed2) {
            logger.error(getGeneratorName()
                    + " SQL Query Failed to process. SQL=" + sql + " : " + ed2);
        }
        return results;
    }

    /**
     * Gets the Monitor Configuration Manager
     * 
     * @return
     */
    public MonitorStateConfigurationManager getConfigManager() {
        if (msc == null) {
            msc = MonitorStateConfigurationManager.getInstance();
        }
        return msc;
    }

    /**
     * Tells whether it is on or off
     * 
     * @return
     */
    public abstract boolean isRunning();

    /**
     * Get list of plugins
     * 
     * @return
     */
    public PluginDataObject[] getPluginDataObjects() {
        return pdos;
    }

    /**
     * Set list of plugins
     * 
     * @param pdos
     */
    public void setPluginDataObjects(PluginDataObject[] pdos) {
        this.pdos = pdos;
    }

    /**
     * Sets the DAO
     * 
     * @param dao
     */
    public void setPluginDao(PluginDao dao) {
        this.dao = dao;
    }

    /**
     * Get the DAO
     * 
     * @return
     */
    public PluginDao getDao() {
        return dao;
    }

    /**
     * Persist the CPG records out
     */
    protected void persistRecords() {
        try {
            PluginDataObject[] pdos = getPluginDataObjects();
            if (pdos != null && getDao() != null && pdos.length > 0) {
                EDEXUtil.checkPersistenceTimes(pdos);
                getDao().persistRecords(pdos);
            }
        } catch (PluginException e) {
            e.printStackTrace();
        }
    }

    /**
     * Fires an update to the Topic
     */
    protected void fireTopicUpdate() {
        try {
            if (getPluginDataObjects() != null
                    && getPluginDataObjects().length > 0) {
                EDEXUtil.getMessageProducer().sendAsync("cpgProcessAlerts",
                        getPluginDataObjects());
            }
        } catch (EdexException e) {
            e.printStackTrace();
        }
    }

    /**
     * clear the list of PDO's
     */
    protected void clear() {
        if (pdos != null) {
            pdos = null;
        }
        if (dao != null) {
            dao = null;
        }
    }

    /**
     * the executor runner
     * 
     * @return
     */
    public Executor getExecutor() {
        return executor;
    }

    public void setExecutor(Executor executor) {
        this.executor = executor;
    }

    /**
     * Log process statistics
     * 
     * @param message
     */
    @Override
    public void log(URIGenerateMessage message) {

        if (getPluginDataObjects() != null && getPluginDataObjects().length > 0) {

            long curTime = System.currentTimeMillis();
            ProcessEvent processEvent = new ProcessEvent();
            String pluginName = getPluginDataObjects()[0].getPluginName();

            if (pluginName != null) {
                processEvent.setPluginName(pluginName);
            }

            Long dequeueTime = message.getDeQueuedTime();
            if (dequeueTime != null) {
                long elapsedMilliseconds = curTime - dequeueTime;
                processEvent.setProcessingTime(elapsedMilliseconds);
            }

            Long enqueueTime = message.getEnQueuedTime();
            if (enqueueTime != null) {
                long latencyMilliseconds = curTime - enqueueTime;
                processEvent.setProcessingLatency(latencyMilliseconds);
            }

            // processing in less than 0 millis isn't trackable, usually due to
            // an
            // error occurred and statement logged incorrectly
            if ((processEvent.getProcessingLatency() > 0)
                    && (processEvent.getProcessingTime() > 0)) {
                eventBus.publish(processEvent);
            }
        }
    }
}
