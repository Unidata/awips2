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
package com.raytheon.edex.plugin.gfe.smartinit;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;

import org.hibernate.HibernateException;
import org.hibernate.LockOptions;
import org.hibernate.Session;
import org.hibernate.Transaction;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.edex.plugin.gfe.server.database.VGridDatabase;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Smart Init Aggregator/Queue for Camel
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 11, 2008            njensen     Initial creation
 * Oct 6, 2009    3172     njensen     Based on GribNotifyMessages
 * Jun 13, 2013  #2044     randerso    Refactored to use IFPServer,
 *                                     moved smartInit queuing code here 
 *                                     from other modules, general code cleanup
 * 10/16/2014   3454       bphillip    Upgrading to Hibernate 4                               
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SmartInitQueue {
    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SmartInitQueue.class);

    private static SmartInitQueue queue;

    /**
     * Create the single instance of the queue. Should only be used by spring.
     * 
     * @return the smartInit queue
     */
    public static synchronized SmartInitQueue createQueue() {
        if (queue == null) {
            queue = new SmartInitQueue();
        }
        return queue;
    }

    /**
     * get single instance of queue if it exists
     * 
     * @return the singleton instance or null
     */
    public static SmartInitQueue getQueue() {
        return queue;
    }

    private Map<SmartInitRecordPK, SmartInitRecord> initSet;

    private SmartInitQueue() {
        initSet = new HashMap<SmartInitRecordPK, SmartInitRecord>();
    }

    /**
     * Queue a SmartInit to be run
     * 
     * @param site
     *            the site ID
     * @param config
     *            server configuration
     * @param dbId
     *            the Database ID
     * @param validTime
     *            model run time
     * @param calcAll
     *            true to force recalculation of all parameters
     * @param priority
     *            priority for smartInit see constants defined in
     *            {@link SmartInitRecord}
     */
    public void queue(String site, IFPServerConfig config, DatabaseID dbId,
            Date validTime, boolean calcAll, int priority) {
        String gfeModel = dbId.getModelName();
        List<String> siteInitModules = config.initModels(gfeModel);

        StringBuilder initNameBuilder = new StringBuilder(120);
        List<SmartInitRecord> inits = new ArrayList<SmartInitRecord>(
                siteInitModules.size());
        for (String moduleName : siteInitModules) {
            initNameBuilder.setLength(0);
            initNameBuilder.append(site);
            initNameBuilder.append("_GRID_D2D_");
            initNameBuilder.append(moduleName);
            initNameBuilder.append('_');
            initNameBuilder.append(dbId.getModelTime());

            SmartInitRecord record = new SmartInitRecord(
                    initNameBuilder.toString(), moduleName, validTime,
                    dbId.toString(), calcAll, priority);
            inits.add(record);
        }

        mergeInits(inits);
    }

    private void mergeInits(Collection<SmartInitRecord> inits) {
        for (SmartInitRecord record : inits) {
            try {
                DatabaseID toAdd = new DatabaseID(record.getDbName());
                IFPServerConfig config = IFPServerConfigManager
                        .getServerConfig(toAdd.getSiteId());
                String modelTime = toAdd.getModelTime();
                int hour = Integer.parseInt(modelTime.substring(9, 11));
                if (config.initSkip(toAdd.getModelName(), hour)) {
                    continue;
                }
            } catch (GfeConfigurationException e) {
                statusHandler
                        .handle(Priority.ERROR, e.getLocalizedMessage(), e);
                continue;
            }

            synchronized (this) {
                SmartInitRecordPK id = record.getId();
                SmartInitRecord oldRecord = initSet.get(id);
                if (oldRecord == null) {
                    initSet.put(id, record);
                } else {
                    Date newInsertTime = record.getInsertTime();
                    if (newInsertTime.getTime() > oldRecord.getInsertTime()
                            .getTime()) {
                        oldRecord.setInsertTime(newInsertTime);
                    }
                    oldRecord.setManual(oldRecord.isManual()
                            || record.isManual());
                    oldRecord.setPriority(Math.min(oldRecord.getPriority(),
                            record.getPriority()));
                }
            }
        }
    }

    /**
     * Queue a manual smartInit request
     * 
     * @param init
     *            init request
     * 
     *            <pre>
     *  Examples:
     *     OAX_GRID_D2D_RAP13_20100923_0900 or
     *     OAX_GRID_D2D_RAP13_20100923_0900:1 or
     *     OAX_GRID_D2D_RAP13_20100923_0900:1:myRAP13
     * </pre>
     */
    public void addManualInit(String init) {
        Collection<SmartInitRecord> manualInits = splitManual(init);
        mergeInits(manualInits);
        // force update the tables
        fireSmartInit();
    }

    /**
     * Flush the in memory smartInit queue to database.
     * 
     * This is done on a timer to reduce the number of database writes
     * 
     */
    public void fireSmartInit() {
        Map<SmartInitRecordPK, SmartInitRecord> initsToStore = null;

        // copy off inits to store, allowing other threads to continue
        // accumulating
        synchronized (this) {
            if (initSet.size() > 0) {
                initsToStore = initSet;
                initSet = new HashMap<SmartInitRecordPK, SmartInitRecord>(
                        (int) (initsToStore.size() * 1.25) + 1);
            }
        }

        if (initsToStore != null) {
            CoreDao cd = new CoreDao(DaoConfig.DEFAULT);
            Session s = null;
            try {
                s = cd.getSession();
                Transaction tx = null;
                SmartInitRecord oldRecord = null;

                for (SmartInitRecord record : initsToStore.values()) {
                    try {
                        tx = s.beginTransaction();

                        oldRecord = (SmartInitRecord) s.get(
                                SmartInitRecord.class, record.getId(),
                                LockOptions.UPGRADE);

                        if (oldRecord == null) {
                            s.save(record);
                        } else {
                            Date newInsertTime = record.getInsertTime();
                            oldRecord.setPriority(Math.min(
                                    oldRecord.getPriority(),
                                    record.getPriority()));
                            if (oldRecord.getInsertTime().getTime() < newInsertTime
                                    .getTime()) {
                                oldRecord.setInsertTime(newInsertTime);
                            }
                            oldRecord.setManual(oldRecord.isManual()
                                    || record.isManual());
                            s.update(oldRecord);
                        }
                        tx.commit();
                    } catch (Throwable t) {
                        statusHandler.handle(Priority.ERROR,
                                "Error adding smartInit [" + record.getId()
                                        + "] to database queue", t);

                        if (tx != null) {
                            try {
                                tx.rollback();
                            } catch (HibernateException e) {
                                statusHandler
                                        .handle(Priority.ERROR,
                                                "Error rolling back smart init lock transaction",
                                                e);
                            }
                        }
                    }
                }
            } finally {
                if (s != null) {
                    try {
                        s.close();
                    } catch (HibernateException e) {
                        statusHandler.handle(Priority.ERROR,
                                "Error closing smart init lock session", e);
                    }
                }
            }
        }

    }

    private Collection<SmartInitRecord> splitManual(String initName) {
        List<SmartInitRecord> rval = new ArrayList<SmartInitRecord>(60);

        try {
            if (initName == null) {
                return rval;
            }

            // OAX_GRID_D2D_RAP13_20100923_0900 or
            // OAX_GRID_D2D_RAP13_20100923_0900:1 or
            // OAX_GRID_D2D_RAP13_20100923_0900:1:myRAP13
            String[] tokens = initName.split("[:]");

            int index = tokens[0].indexOf("_GRID_D2D_");
            if (index < 0) {
                return rval;
            }

            DatabaseID dbId = new DatabaseID(tokens[0]);
            if (!dbId.isValid()) {
                return rval;
            }

            IFPServer ifpServer = IFPServer.getActiveServer(dbId.getSiteId());
            if (ifpServer == null) {
                return rval;
            }

            VGridDatabase db = (VGridDatabase) ifpServer.getGridParmMgr()
                    .getDatabase(dbId);
            if (db == null) {
                return rval;
            }

            boolean calcAll = true;
            if ((tokens.length > 1) && tokens[1].equals("0")) {
                calcAll = false;
            }

            List<String> siteInitModules;
            String gfeModel = dbId.getModelName();
            String dbName = dbId.toString();

            if ((tokens.length > 2) && (tokens[2].length() > 0)) {
                siteInitModules = new ArrayList<String>();
                siteInitModules.add(tokens[2]);
            } else {
                IFPServerConfig config = IFPServerConfigManager
                        .getServerConfig(dbId.getSiteId());
                siteInitModules = config.initModels(gfeModel);
            }

            int priority = SmartInitRecord.MANUAL_SMART_INIT_PRIORITY;
            if (tokens.length > 3) {
                priority = Integer.parseInt(tokens[3]);
            }

            SortedSet<Date> validTimes = db.getValidTimes();
            for (String module : siteInitModules) {
                for (Date validTime : validTimes) {
                    SmartInitRecord record = new SmartInitRecord(
                            dbName.replace(gfeModel, module), module,
                            validTime, dbName, calcAll, priority);
                    rval.add(record);
                }
            }
        } catch (Exception e) {
            statusHandler.error("Failed to parse manual smartInit request", e);
        }

        return rval;
    }
}
