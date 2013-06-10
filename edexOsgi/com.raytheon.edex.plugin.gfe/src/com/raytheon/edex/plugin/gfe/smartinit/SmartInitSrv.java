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

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Executor;

import jep.JepException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.gfe.config.GFESiteActivation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.props.EnvProperties;
import com.raytheon.uf.edex.core.props.PropertiesFactory;

/**
 * Service that runs smart inits
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Apr 22, 2008             njensen     Initial creation
 * Jul 17, 2009   #2590   	njensen     Multiple site support
 * Jul 28, 2010   #6725  	jdynina		Manual init support
 * Aug 27, 2010   #3688     wkwock      Find model class for a model
 * Aug 24, 2013   #1949     rjpeter     Updated start up logic
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SmartInitSrv {

    private final Map<Long, SmartInitScript> cachedInterpreters = new HashMap<Long, SmartInitScript>();

    private static boolean enabled = true;

    protected final SmartInitSrvConfig cfg;

    protected final Executor executor;

    static {
        EnvProperties env = PropertiesFactory.getInstance().getEnvProperties();
        enabled = Boolean.parseBoolean(env.getEnvValue("GFESMARTINIT"));
    }

    public SmartInitSrv(SmartInitSrvConfig config) {
        cfg = config;
        this.executor = config.getExecutor();
        for (int i = 0; i < cfg.getThreads(); i++) {
            SmartInitThread thread = new SmartInitThread();
            thread.pendingInitMinTimeMillis = cfg.getPendingInitMinTimeMillis();
            thread.runningInitTimeOutMillis = cfg.getRunningInitTimeOutMillis();
            thread.threadSleepInterval = cfg.getThreadSleepInterval();
            executor.execute(thread);
        }
    }

    protected class SmartInitThread implements Runnable {
        // default of 2 minutes
        private int pendingInitMinTimeMillis = 120000;

        private int runningInitTimeOutMillis = 300000;

        private int threadSleepInterval = 30000;

        private final transient Log logger = LogFactory.getLog(getClass());

        @Override
        public void run() {
            try {
                // Wait for server to come fully up due to route dependencies
                while (!EDEXUtil.isRunning()) {
                    try {
                        Thread.sleep(threadSleepInterval);
                    } catch (InterruptedException e) {
                        // ignore
                    }
                }

                // run forever
                while (true) {
                    SmartInitRecord record = SmartInitTransactions
                            .getSmartInitToRun(pendingInitMinTimeMillis,
                                    runningInitTimeOutMillis);
                    if (record != null) {
                        runSmartInit(record);
                    } else {
                        try {
                            Thread.sleep(threadSleepInterval);
                        } catch (Exception e) {
                            // ignore
                        }
                    }
                }
            } finally {
                // Make sure OS resources are released at thread death
                SmartInitScript script = cachedInterpreters.remove(Thread
                        .currentThread().getId());
                script.dispose();
            }
        }

        public void runSmartInit(SmartInitRecord record) {
            if (enabled) {
                try {
                    SmartInitScript initScript = null;
                    List<String> sitePathsAdded = new ArrayList<String>(2);

                    String init = record.getSmartInit();
                    String dbName = record.getDbName()
                            + (record.isManual() ? ":1" : ":0");

                    DatabaseID db = new DatabaseID(record.getDbName());
                    if (GFESiteActivation.getInstance().getActiveSites()
                            .contains(db.getSiteId())) {
                        try {
                            long id = Thread.currentThread().getId();
                            initScript = cachedInterpreters.get(id);

                            if (initScript == null) {
                                initScript = SmartInitFactory.constructInit();
                                cachedInterpreters.put(id, initScript);
                            }

                            IPathManager pathMgr = PathManagerFactory
                                    .getPathManager();
                            LocalizationContext ctx = pathMgr
                                    .getContextForSite(
                                            LocalizationType.EDEX_STATIC,
                                            db.getSiteId());
                            LocalizationContext baseCtx = pathMgr.getContext(
                                    LocalizationType.EDEX_STATIC,
                                    LocalizationLevel.BASE);

                            File file = pathMgr.getFile(ctx, "smartinit");
                            if ((file != null) && file.exists()) {
                                initScript.addSitePath(file.getPath(), pathMgr
                                        .getFile(baseCtx, "smartinit")
                                        .getPath());
                                sitePathsAdded.add(file.getPath());
                            }
                            file = pathMgr.getFile(ctx,
                                    FileUtil.join("config", "gfe"));
                            if ((file != null) && file.exists()) {
                                initScript.addSitePath(
                                        file.getPath(),
                                        pathMgr.getFile(baseCtx,
                                                FileUtil.join("config", "gfe"))
                                                .getPath());
                                sitePathsAdded.add(file.getPath());
                            }

                            HashMap<String, Object> argMap = new HashMap<String, Object>();
                            argMap.put("dbName", dbName);
                            argMap.put("model", init);
                            argMap.put("validTime", record.getId()
                                    .getValidTime());

                            initScript.execute(argMap);
                        } catch (Throwable e) {
                            logger.error("Error running smart init for "
                                    + record.getId(), e);
                        } finally {
                            try {
                                for (String path : sitePathsAdded) {
                                    initScript.removeSitePath(path);
                                }
                            } catch (JepException e) {
                                this.logger
                                        .error("Error cleaning up smart init interpreter's sys.path",
                                                e);
                            }
                        }
                    } else {
                        this.logger.warn("Site " + db.getSiteId()
                                + " has been disabled. Smart init for "
                                + record.getDbName()
                                + " will not be processed.");
                    }
                } catch (Throwable t) {
                    this.logger.error("Error in SmartInitSrv", t);
                }
                SmartInitTransactions.removeSmartInit(record);
            }
        }
    }
}
