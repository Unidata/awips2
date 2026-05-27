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
package com.raytheon.uf.edex.backupsvc.service;

import java.util.HashSet;
import java.util.Set;

import com.raytheon.uf.common.backupsvc.request.GetBackupHostCapabilitiesMapRequest;

/**
 * Stores BackupService capabilities for this server.
 *
 * A "capability" in the BackupService context is just a string associated with
 * a particular server. EDEX plugins may contribute capabilities by calling
 * register(). Then anyone (client or server-side) may send a
 * {@link GetBackupHostCapabilitiesMapRequest} to the server running
 * BackupService to retrieve a list of all configured backup servers and the
 * capabilities of each server.
 *
 * Example use cases:
 *
 * 1. A part of the system that wants to enqueue a backup job may query for a
 * list of servers that have a particular capability, and only enqueue the job
 * for those servers.
 *
 * 2. Creators of BackupService jobs can tailor a job's IServerRequest to each
 * individual server, depending on what capabilities each server has.
 *
 * Notes:
 *
 * - The total number of different capabilities that a server may have is not
 * enumerable. Any EDEX code can call registerCapability() with any string, and
 * in doing so create a new capability.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2017 6352       tgurney     Initial creation
 *
 * </pre>
 *
 * @author tgurney
 */

public class BackupServiceCapabilityManager {

    private static final BackupServiceCapabilityManager INSTANCE = new BackupServiceCapabilityManager();

    private Set<String> capabilities = new HashSet<>();

    private BackupServiceCapabilityManager() {
    }

    public static BackupServiceCapabilityManager getInstance() {
        return INSTANCE;
    }

    public void register(String capability) {
        capabilities.add(capability);
    }

    /** @return the list of capabilities that this server has */
    public Set<String> getCapabilities() {
        return new HashSet<>(capabilities);
    }
}
