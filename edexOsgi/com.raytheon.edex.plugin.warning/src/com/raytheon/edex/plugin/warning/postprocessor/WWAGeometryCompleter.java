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

package com.raytheon.edex.plugin.warning.postprocessor;

import java.util.Set;

import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.util.UGCToGeometryUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Adds geometries to warning records that don't have them
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * May 16, 2016  5657     tgurney   Initial creation
 * 
 * </pre>
 * 
 * @author tgurney
 */
public class WWAGeometryCompleter {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WWAGeometryCompleter.class);

    public WWAGeometryCompleter() {
    }

    /**
     * Add geometry to those records that have a list of UGC zones but no
     * geometry.
     * 
     * @param records
     * @return The records, with geometries added where necessary.
     */
    public AbstractWarningRecord[] addMissingGeometries(
            AbstractWarningRecord[] records) {
        for (AbstractWarningRecord record : records) {
            Set<String> ugcZones = record.getUgcZones();
            if (record.getGeometry() == null) {
                if (ugcZones != null) {
                    try {
                        record.setGeometry(UGCToGeometryUtil.ugcsToGeometry(
                                ugcZones, record.getPhen()));
                        statusHandler.info(record.getDataURI()
                                + ": Added geometry in post-processing");
                    } catch (Exception e) {
                        statusHandler.handle(
                                Priority.ERROR,
                                e.getLocalizedMessage() + " ("
                                        + record.getDataURI() + ")", e);
                    }
                } else {
                    statusHandler.info(record.getDataURI()
                            + ": Record has no UGCs and no geometry. "
                            + "Not modifying this record.");
                }
            }
        }
        return records;
    }

}