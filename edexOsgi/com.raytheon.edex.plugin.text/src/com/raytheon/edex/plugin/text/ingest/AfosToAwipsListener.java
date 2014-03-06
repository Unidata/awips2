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
package com.raytheon.edex.plugin.text.ingest;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.StringUtils;

import com.raytheon.edex.plugin.text.dao.AfosToAwipsDao;
import com.raytheon.uf.common.dataplugin.text.AfosWmoIdDataContainer;
import com.raytheon.uf.common.dataplugin.text.db.AfosToAwips;
import com.raytheon.uf.common.dataplugin.text.db.AfosToAwipsId;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.ndm.ingest.INationalDatasetSubscriber;

/**
 * Updates the afos_to_awips table in the database based on the supplied
 * afos2awips.txt file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 11, 2011            bfarmer     Initial creation
 * Mar 06, 2014   2876     mpduff      New NDM plugin.
 * 
 * </pre>
 * 
 * @author bfarmer
 * @version 1.0
 */

public class AfosToAwipsListener implements INationalDatasetSubscriber {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AfosToAwipsListener.class);

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.site.ingest.INationalDatasetSubscriber#notify(
     * java.lang.String, java.io.File)
     */
    @Override
    public void notify(String fileName, File file) {
        List<AfosToAwipsId> incoming = new ArrayList<AfosToAwipsId>();
        BufferedReader fis = null;
        try {
            fis = new BufferedReader(new InputStreamReader(new FileInputStream(
                    file)));
            String line = null;
            try {
                while ((line = fis.readLine()) != null) {
                    String[] values = line.split("\\s+");
                    if (values.length == 3) {
                        String awipsId = StringUtils.rightPad(values[0].trim()
                                .toUpperCase(), AfosToAwipsDao.AFOSID_LENGTH);
                        String wmotaaii = values[1].trim().toUpperCase();
                        String wmocccc = values[2].trim().toUpperCase();
                        incoming.add(new AfosToAwipsId(awipsId, wmotaaii,
                                wmocccc));
                    }
                }
            } catch (IOException e) {
                statusHandler.handle(
                        Priority.PROBLEM,
                        "Could not read ingested afos2awips file:"
                                + file.getAbsolutePath(), e);
                return;
            } finally {
                if (fis != null) {
                    try {
                        fis.close();
                    } catch (IOException e) {
                        // ignore
                    }
                }
            }
        } catch (FileNotFoundException e) {
            statusHandler.handle(
                    Priority.PROBLEM,
                    "Could not read ingested afos2awips file:"
                            + file.getAbsolutePath(), e);
            return;
        }
        AfosToAwipsDao afosDao = new AfosToAwipsDao();
        AfosWmoIdDataContainer allRecords = afosDao.getAllRecords();
        AfosWmoIdDataContainer deleteRecords = new AfosWmoIdDataContainer();
        for (AfosToAwips oldId : allRecords.getIdList()) {
            if (!incoming.remove(oldId.getId())) {
                deleteRecords.add(oldId);
            }
        }
        // At this point, deleteRecords should hold all records currently in DB
        // that are not in the incoming dataset, while incoming should hold only
        // records that are in the new dataset but not currently in the DB, and
        // thus should be added.
        for (AfosToAwips delete : deleteRecords.getIdList()) {
            afosDao.removeRecord(delete.getId());
        }
        for (AfosToAwipsId add : incoming) {
            afosDao.addRecord(add);
        }
        statusHandler.handle(Priority.INFO,
                "Successfully processed " + file.getAbsolutePath());
    }
}
