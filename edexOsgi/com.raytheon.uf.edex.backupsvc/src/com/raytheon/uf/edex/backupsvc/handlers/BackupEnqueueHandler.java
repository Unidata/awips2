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
package com.raytheon.uf.edex.backupsvc.handlers;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.backupsvc.BackupServiceException;
import com.raytheon.uf.common.backupsvc.request.BackupEnqueueRequest;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.serialization.comm.response.GenericResponse;
import com.raytheon.uf.edex.backupsvc.database.BackupJobDao;
import com.raytheon.uf.edex.backupsvc.service.BackupServiceConfigManager;

/**
 * Handler for BackupEnqueueRequest
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 10, 2016 5937       tgurney     Initial creation
 * Dec  9, 2016 5937       tgurney     Better config handling
 * Jul 20, 2017 6352       tgurney     Add versionRequired parameters to enqueue
 * Oct  8, 2019 7929       tgurney     Store the request uncompressed
 *
 * </pre>
 *
 * @author tgurney
 */

public class BackupEnqueueHandler
        implements IRequestHandler<BackupEnqueueRequest> {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private BackupJobDao dao;

    private BackupServiceConfigManager configMgr;

    public BackupEnqueueHandler() {
        dao = new BackupJobDao();
        configMgr = BackupServiceConfigManager.getInstance();
    }

    /**
     * Enqueue a request to send to backup hosts. A new job with the same name
     * as a job that is already in queue will supersede that already existing
     * job. If you don't want this behavior then you need to use a unique job
     * name every time you enqueue a job. Note that this method will not create
     * a job (i.e. is a no-op) if no backup hosts are configured.
     *
     * @param request
     *            Request to send
     * @param jobName
     *            Job name
     * @param priority
     *            Request priority (lower number = higher priority)
     * @param hostnames
     *            List of hosts to send the request to. If empty, send to all
     *            hosts configured with BackupService
     * @param minVersionRequired
     *            Host must have at least this EDEX version to receive this
     *            request
     * @param maxVersionRequired
     *            Host must have no greater than this EDEX version to receive
     *            this request
     * @throws SerializationException
     * @throws BackupServiceException
     *             If the enqueue failed. This is either a database-related
     *             error, or a failure to serialize the provided request, or a
     *             problem with BackupService configuration
     */
    private void enqueue(IServerRequest request, String jobName, int priority,
            List<String> hostnames, String minVersionRequired,
            String maxVersionRequired)
            throws BackupServiceException, SerializationException {
        List<String> filteredHostnames = new ArrayList<>();
        List<String> configuredHostnames = configMgr.getHostnamesOnly();
        if (configuredHostnames.isEmpty()) {
            throw new BackupServiceException("No backup hosts are configured.");
        }
        if (hostnames != null) {
            for (String hostname : hostnames) {
                if (configuredHostnames.contains(hostname)) {
                    filteredHostnames.add(hostname);
                } else {
                    // Host specified that is not configured
                    throw new BackupServiceException("Host " + hostname
                            + " is not a configured backup host.");
                }
            }
        } else {
            // No hostnames specified. Send to all configured hosts
            filteredHostnames = configuredHostnames;
        }
        byte[] blob = DynamicSerializationManager
                .getManager(SerializationType.Thrift).serialize(request);
        try {
            dao.createNewJob(jobName, priority, blob, filteredHostnames,
                    minVersionRequired, maxVersionRequired);
        } catch (Exception e) {
            throw new BackupServiceException(e);
        }
    }

    @Override
    public Object handleRequest(BackupEnqueueRequest request) throws Exception {
        GenericResponse response = new GenericResponse();
        response.setSuccess(false);
        try {
            enqueue(request.getRequest(), request.getJobName(),
                    request.getPriority(), request.getHosts(),
                    request.getMinVersionRequired(),
                    request.getMaxVersionRequired());
            response.setSuccess(true);
        } catch (Exception e) {
            logger.error("Failed to enqueue backup job " + request.getJobName()
                    + ": ", e);
            response.setMessage(e.getMessage());
        }
        return response;
    }

}
