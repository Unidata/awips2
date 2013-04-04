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
package com.raytheon.edex.services;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.datastorage.DuplicateRecordStorageException;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageStatus;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * Performs persistence services to non-database stores
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 31, 2008            chammack     Initial creation
 * 02/06/09     1990       bphillip    Refactored to use plugin specific daos
 * Nov 02, 2012 1302       djohnson    Remove unused method, fix formatting.
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class PersistSrv {

    private final Log logger = LogFactory.getLog(getClass());

    private static final PersistSrv instance = new PersistSrv();

    public static PersistSrv getInstance() {
        return instance;
    }

    private PersistSrv() {
    }

    public PluginDataObject[] persist(PluginDataObject[] pdo) {

        if (pdo == null || pdo.length == 0) {
            return new PluginDataObject[0];
        }

        Set<PluginDataObject> pdoList = new HashSet<PluginDataObject>();
        EDEXUtil.checkPersistenceTimes(pdo);

        try {
            PluginDao dao = PluginFactory.getInstance().getPluginDao(
                    pdo[0].getPluginName());
            StorageStatus ss = dao.persistToHDF5(pdo);
            StorageException[] se = ss.getExceptions();
            pdoList.addAll(Arrays.asList(pdo));
            if (se != null) {
                Map<PluginDataObject, StorageException> pdosThatFailed = new HashMap<PluginDataObject, StorageException>();
                for (StorageException s : se) {
                    IDataRecord rec = s.getRecord();

                    if (rec != null) {
                        // If we have correlation info and it's a pdo, use that
                        // for the error message...
                        Object corrObj = rec.getCorrelationObject();
                        if (corrObj != null
                                && corrObj instanceof PluginDataObject) {
                            pdosThatFailed.put((PluginDataObject) corrObj, s);
                        } else {
                            // otherwise, do the best we can with the group
                            // information
                            logger.error("Persisting record " + rec.getGroup()
                                    + "/" + rec.getName() + " failed.", s);
                        }
                    } else {
                        // All we know is something bad happened.
                        logger.error("Persistence error occurred: ", s);
                    }
                }

                // Produce error messages for each pdo that failed
                int errCnt = 0;
                boolean suppressed = false;
                for (Map.Entry<PluginDataObject, StorageException> e : pdosThatFailed
                        .entrySet()) {
                    if (errCnt > 50) {
                        logger.warn("More than 50 errors occurred in this batch.  The remaining errors will be suppressed.");
                        suppressed = true;
                        continue;
                    }

                    if (!suppressed) {
                        if (e.getValue() instanceof DuplicateRecordStorageException) {
                            logger.warn("Duplicate record encountered (duplicate ignored): "
                                    + e.getKey().getDataURI());

                        } else {
                            logger.error(
                                    "Error persisting record " + e.getKey()
                                            + " to database: ", e.getValue());
                        }
                    }

                    // Remove from the pdoList so the pdo is not propagated
                    // to the next service
                    pdoList.remove(e.getKey());
                    errCnt++;

                }
            }
        } catch (Throwable e1) {
            logger.error(
                    "Critical persistence error occurred.  Individual records that failed will be logged separately.",
                    e1);
            for (PluginDataObject p : pdo) {
                logger.error("Record "
                        + p
                        + " failed persistence due to critical error logged above.");
            }
        }

        return pdoList.toArray(new PluginDataObject[pdoList.size()]);
    }
}
