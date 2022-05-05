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
package com.raytheon.edex.plugin.gfe.server.database;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Manages NetCDF databases. This is not for normal D2D data. It is only to
 * support static netCDF databases for GFE like the CRM and NED topo and VDATUMS
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 17, 2012            randerso    Initial creation
 * Oct 10  2012     #1260  randerso    Added exception handling for domain not
 *                                     overlapping the dataset
 * Oct 19  2017     #6126  dgilling    Make NetCDFDatabaseManager objects owned
 *                                     by GridParmManager.
 *
 * </pre>
 *
 * @author randerso
 * @version 1.0
 */

public class NetCDFDatabaseManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(NetCDFDatabaseManager.class);

    private final Map<DatabaseID, NetCDFGridDatabase> databaseMap;

    public NetCDFDatabaseManager(IFPServerConfig config) {
        this.databaseMap = new HashMap<>();

        initializeNetCDFDatabases(config);
    }

    /**
     * Initialize the netCDF databases for the given site's configuration.
     *
     * @param config
     *            The site's configuration containing the NetCDF databases to
     *            initialize.
     */
    private void initializeNetCDFDatabases(IFPServerConfig config) {
        Map<String, String> netCDFDirs = config.netCDFDirs();

        for (Entry<String, String> entry : netCDFDirs.entrySet()) {
            processNetCDFDir(entry.getKey(), entry.getValue(), config);
        }
    }

    private void processNetCDFDir(String dirName, String modelName,
            IFPServerConfig config) {
        File dir = new File(dirName);
        if (!dir.exists() || !dir.canRead() || !dir.isDirectory()) {
            statusHandler
                    .error("Missing or unreadable directory in NETCDFDIRS: "
                            + dirName);
            return;
        }

        List<File> validFiles = new ArrayList<>();
        for (File file : dir.listFiles()) {
            if (NetCDFFile.validFileName(file.getName())) {
                if (file.isFile()) {
                    if (!file.canRead()) {
                        statusHandler.error("Skipping unreadable netCDF file: "
                                + file.getAbsolutePath());
                    } else {
                        validFiles.add(file);
                    }
                }
            }
        }

        if (validFiles.isEmpty()) {
            return;
        }

        /*
         * filenames are time stamps of form YYYYMMDD_hhmm sort in descending
         * order so most recent is first
         */
        Collections.sort(validFiles,
                Comparator.comparing(File::getName).reversed());

        NetCDFFile firstOne = new NetCDFFile(validFiles.get(0)
                .getAbsolutePath(), modelName);
        int numVer = config.desiredDbVersions(NetCDFGridDatabase.getDBID(
                firstOne, config));

        for (int i = 0; i < validFiles.size(); i++) {
            if (i >= numVer) {
                // no more to do
                break;
            }

            NetCDFFile file = new NetCDFFile(validFiles.get(i)
                    .getAbsolutePath(), modelName);

            if (file.isValid()) {
                DatabaseID dbId = NetCDFGridDatabase.getDBID(file, config);

                try {
                    NetCDFGridDatabase db = new NetCDFGridDatabase(config, file);
                    statusHandler.handle(Priority.EVENTB,
                            "New netCDF Database: " + dbId);
                    databaseMap.put(dbId, db);
                } catch (GfeException e) {
                    statusHandler
                            .error("Error instantiating NetCDFGridDatabase ["
                                    + dbId + "]", e);
                }
            }
        }
    }

    public List<DatabaseID> getDatabaseIds() {
        return Collections
                .unmodifiableList(new ArrayList<>(databaseMap.keySet()));
    }

    public GridDatabase getDb(DatabaseID dbId) {
        return databaseMap.get(dbId);
    }
}
