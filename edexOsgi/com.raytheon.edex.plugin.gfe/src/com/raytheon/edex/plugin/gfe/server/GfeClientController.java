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
package com.raytheon.edex.plugin.gfe.server;

import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.CountDownLatch;

import org.apache.commons.collections.map.LRUMap;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.gfe.request.GfeClientRequest;
import com.raytheon.uf.common.jms.qpid.IBrokerRestProvider;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.RunProcess;
import com.raytheon.uf.common.util.SystemUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.IContextStateProcessor;
import com.raytheon.uf.edex.site.ISiteActivationListener;

/**
 * GFE Client Controller manages a pool of GFE Client Server processes and sends
 * GFEClientRequests to them via JMS queues
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Dec 07, 2016  6092     randerso  Initial creation
 * Oct 22, 2019  7724     tgurney   Broker REST API cleanup
 *
 * </pre>
 *
 * @author randerso
 */

public class GfeClientController
        implements IContextStateProcessor, ISiteActivationListener {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GfeClientController.class);

    private static final String GFECLIENT_CMD = "/awips2/GFESuite/bin/gfeclient.sh";

    private static final String GFECLIENT_QUEUE = "gfeClientQueue";

    private static final String GFECLIENT_URI_PREFIX = "jms-generic:queue:";

    private static final long STARTUP_TIMEOUT = 30 * TimeUtil.MILLIS_PER_SECOND;

    private static final long SHUTDOWN_TIMEOUT = TimeUtil.MILLIS_PER_MINUTE;

    private class ProcessKey {
        public final String site;

        public final String config;

        public final String user;

        public ProcessKey(String site, String config, String user) {
            this.site = site;
            this.config = config;
            this.user = user;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + getOuterType().hashCode();
            result = prime * result + (config == null ? 0 : config.hashCode());
            result = prime * result + (site == null ? 0 : site.hashCode());
            result = prime * result + (user == null ? 0 : user.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            ProcessKey other = (ProcessKey) obj;
            if (!getOuterType().equals(other.getOuterType())) {
                return false;
            }
            if (config == null) {
                if (other.config != null) {
                    return false;
                }
            } else if (!config.equals(other.config)) {
                return false;
            }
            if (site == null) {
                if (other.site != null) {
                    return false;
                }
            } else if (!site.equals(other.site)) {
                return false;
            }
            if (user == null) {
                if (other.user != null) {
                    return false;
                }
            } else if (!user.equals(other.user)) {
                return false;
            }
            return true;
        }

        private GfeClientController getOuterType() {
            return GfeClientController.this;
        }

    }

    private class ProcessMonitor extends Thread {

        private ProcessKey key;

        private RunProcess process;

        public ProcessMonitor(ProcessKey key, RunProcess process) {
            this.key = key;
            this.process = process;
        }

        @Override
        public void run() {
            int status = process.waitFor();

            synchronized (processMap) {
                processMap.remove(key);
            }

            String queue = getGfeClientQueue(key.site, key.config, key.user);
            deleteQueue(queue);

            if (status == 0) {
                String msg = String.format(
                        "GfeClientServer for %s terminated normally.", queue);
                statusHandler.info(msg);
            } else {
                String msg = String.format(
                        "GfeClientServer for %s terminated with status code %d:\n%s",
                        queue, status, process.getStderr());
                statusHandler.error(msg);
            }
        }

    }

    private class ProcessMap extends LRUMap {
        private static final long serialVersionUID = 1L;

        public ProcessMap(int maxSize) {
            super(maxSize, 1.0f);
        }

        @Override
        public Object put(Object key, Object value) {
            ProcessMonitor pm = new ProcessMonitor((ProcessKey) key,
                    (RunProcess) value);
            pm.start();
            return super.put(key, value);
        }

        @Override
        protected boolean removeLRU(LinkEntry entry) {
            ProcessKey key = (ProcessKey) entry.getKey();
            RunProcess process = (RunProcess) entry.getValue();

            statusHandler.info(String.format(
                    "GfeClientServer for queue %s being shutdown due to max processes being exceeded.",
                    getGfeClientQueue(key.site, key.config, key.user)));

            shutDownProcess(key, process);
            return super.removeLRU(entry);
        }

    }

    private IBrokerRestProvider provider;

    private ProcessMap processMap;

    private volatile boolean shuttingDownController = false;

    /**
     * Constructor
     *
     * These parameters are injected from camel xml
     *
     * @param provider
     *            broker rest provider
     * @param maxProcesses
     *            maximum number of GfeClientServer processes that may be
     *            simultaneously active
     */
    public GfeClientController(IBrokerRestProvider provider, int maxProcesses) {
        this.provider = provider;
        processMap = new ProcessMap(maxProcesses);
    }

    private static String getGfeClientQueue(String siteId, String config,
            String user) {
        String hostname = SystemUtil.getHostName();
        return String.join(".", GFECLIENT_QUEUE, siteId, config, user,
                hostname);
    }

    private static String getGfeClientURI(String siteId, String config,
            String user) {
        return GFECLIENT_URI_PREFIX + getGfeClientQueue(siteId, config, user);
    }

    /**
     * @param request
     */
    public void processRequest(GfeClientRequest request) {

        ProcessKey key = new ProcessKey(request.getSiteID(),
                request.getConfigFile(), request.getUser());

        String queue = getGfeClientQueue(request.getSiteID(),
                request.getConfigFile(), request.getUser());

        boolean startingProcess = false;
        synchronized (processMap) {
            if (!shuttingDownController) {
                RunProcess process = (RunProcess) processMap.get(key);
                if (process == null) {
                    startingProcess = true;
                    process = startProcess(request.getSiteID(), queue,
                            request.getUser());
                    processMap.put(key, process);
                }
            }
        }

        if (startingProcess) {
            // wait for the GfeClient queue to be created
            try {
                boolean success = true;
                long t0 = System.currentTimeMillis();
                while (!provider.queueReady(queue)) {
                    Thread.sleep(10);

                    if (System.currentTimeMillis() - t0 > STARTUP_TIMEOUT) {
                        success = false;
                        statusHandler.error(String.format(
                                "GfeClientServer %s startup timed out", queue));
                        break;
                    }
                }

                if (success) {
                    long t1 = System.currentTimeMillis();
                    statusHandler.info(
                            String.format("GfeClientServer %s started in %d ms",
                                    queue, t1 - t0));
                }

            } catch (CommunicationException | InterruptedException e) {
                statusHandler.error("Error starting GfeClientServer " + queue,
                        e);
            }
        }

        try {
            EDEXUtil.getMessageProducer().sendAsyncThriftUri(
                    getGfeClientURI(request.getSiteID(),
                            request.getConfigFile(), request.getUser()),
                    request);
        } catch (Exception e) {
            statusHandler.error(e.getLocalizedMessage(), e);
        }
    }

    private RunProcess startProcess(String site, String queue, String user) {
        StringBuilder command = new StringBuilder(GFECLIENT_CMD);
        command.append(" -server ").append(System.getenv("HTTP_SERVER"));
        command.append(" -site ").append(site);
        command.append(" -queue ").append(queue);
        command.append(" -u ").append(user);

        RunProcess proc = null;
        try {
            proc = RunProcess.getRunProcess().exec(command.toString());
        } catch (IOException e) {
            statusHandler.error("Error starting GfeClientServer: " + command,
                    e);
        }
        return proc;
    }

    private void shutDownProcess(ProcessKey key, RunProcess process) {
        // shut down process
        GfeClientRequest shutDown = new GfeClientRequest(
                GfeClientRequest.SHUTDOWN_CMD, key.site, key.config, key.user,
                new String[0]);
        try {
            String uri = getGfeClientURI(key.site, key.config, key.user);
            EDEXUtil.getMessageProducer().sendAsyncThriftUri(uri, shutDown);

            // wait for process to finish or time out
            process.waitFor(SHUTDOWN_TIMEOUT);

            // if it hasn't finished by now, kill it!
            if (!process.isExecComplete()) {
                statusHandler.warn(String.format(
                        "GfeClientServer %s failed to shutdown in %d seconds. Process will be killed.",
                        getGfeClientQueue(key.site, key.config, key.user),
                        SHUTDOWN_TIMEOUT / TimeUtil.MILLIS_PER_SECOND));
                process.stop();

            }

            String queue = getGfeClientQueue(key.site, key.config, key.user);
            deleteQueue(queue);

        } catch (Exception e) {
            statusHandler.error(e.getLocalizedMessage(), e);
        }
    }

    private void deleteQueue(String queue) {
        try {
            provider.deleteQueue(queue);
        } catch (CommunicationException e) {
            statusHandler.error(
                    "Error attempting to delete GFEClient queue " + queue, e);
        }
    }

    /**
     * Shutdown a list of processes
     *
     * @param processes
     *            the ProcessMap entries to be shutdown
     */
    private void shutDownProcesses(Collection<?> processes) {

        CountDownLatch latch = new CountDownLatch(processes.size());
        for (Object obj : processes) {
            if (obj instanceof Entry) {
                @SuppressWarnings({ "rawtypes", "unchecked" })
                Entry<ProcessKey, RunProcess> entry = (Entry) obj;

                // start a thread to shutdown the process
                Thread thread = new Thread() {
                    @Override
                    public void run() {
                        shutDownProcess(entry.getKey(), entry.getValue());
                        latch.countDown();
                    }
                };
                thread.start();
            }
        }

        // wait for the shutDown threads to complete
        try {
            latch.await();
        } catch (InterruptedException e) {
            // do nothing
        }
    }

    /**
     * Shuts down all GFE Client processes for a given site
     *
     * @param siteID
     *            the site being shutdown
     */
    @SuppressWarnings("unchecked")
    private void shutDownSite(String siteID) {

        Set<Object> processes;
        synchronized (processMap) {
            processes = new HashSet<>(processMap.size(), 1.0f);
            for (Object obj : processMap.entrySet()) {
                @SuppressWarnings({ "rawtypes" })
                Entry<ProcessKey, RunProcess> entry = (Entry) obj;
                if (siteID.equals(entry.getKey().site)) {
                    processes.add(obj);
                }
            }
        }

        shutDownProcesses(processes);
    }

    /**
     * Shuts down all GFE Client processes
     */
    @SuppressWarnings("unchecked")
    private void shutDownController() {

        Set<Object> processes;
        synchronized (processMap) {
            shuttingDownController = true;
            processes = new HashSet<>(processMap.entrySet());
        }

        shutDownProcesses(processes);
    }

    @Override
    public void preStart() {
        // do nothing
    }

    @Override
    public void postStart() {
        // do nothing
    }

    @Override
    public void preStop() {
        shutDownController();
    }

    @Override
    public void postStop() {
        // do nothing
    }

    @Override
    public void activateSite(String siteID) throws Exception {
        // do nothing
    }

    @Override
    public void deactivateSite(String siteID) throws Exception {
        shutDownSite(siteID);
    }

    @Override
    public Set<String> getActiveSites() {
        return Collections.emptySet();
    }

    @Override
    public String validateConfig(String siteID) {
        return "";
    }

    @Override
    public void registered() {
        // do nothing
    }
}
