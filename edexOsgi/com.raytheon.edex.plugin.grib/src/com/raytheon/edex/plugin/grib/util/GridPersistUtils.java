/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     2120 South 72nd Street, Suite 900
 *                         Omaha, NE 68124
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.edex.plugin.grib.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.datastorage.audit.DataStorageAuditUtils;
import com.raytheon.uf.common.datastorage.audit.MetadataStatus;

/**
 *
 * Contains constants and utility methods for persisting grid data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 12, 2021 8664       mapeters    Initial creation (extracted from
 *                                     obsolete GridPersister)
 * Feb 16, 2022 8608       mapeters    Use DataStorageAuditUtils
 *
 * </pre>
 *
 * @author mapeters
 */
public class GridPersistUtils {

    /**
     * Private constructor to prevent instantiation
     */
    private GridPersistUtils() {
    }

    /**
     * Handle case of multiple quadrants stored in one transaction.
     *
     * @param pdos
     *            the PDOs to filter and audit duplicates from
     * @return the retained PDOs
     */
    public static PluginDataObject[] eliminateAndAuditDuplicates(
            PluginDataObject... pdos) {
        if (pdos != null && pdos.length > 1) {
            // dup elim by dataURI
            Map<String, PluginDataObject> pdoMap = new HashMap<>(pdos.length,
                    1);
            List<PluginDataObject> dups = new ArrayList<>();
            for (PluginDataObject pdo : pdos) {
                PluginDataObject dup = pdoMap.put(pdo.getDataURI(), pdo);
                if (dup != null) {
                    dups.add(dup);
                }
            }

            if (!dups.isEmpty()) {
                /*
                 * Audit that the discarded duplicates won't have any metadata
                 * stored for them.
                 */
                DataStorageAuditUtils.auditMetadataStatuses(MetadataStatus.NA,
                        dups);
                pdos = pdoMap.values().toArray(new PluginDataObject[0]);
            }
        }

        return pdos;
    }
}
