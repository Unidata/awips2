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
package com.raytheon.uf.edex.plugin.mpe.dao.impl;

import com.raytheon.uf.common.dataplugin.shef.tables.Datalimits;
import com.raytheon.uf.common.dataplugin.shef.tables.DatalimitsId;
import com.raytheon.uf.edex.plugin.mpe.dao.AbstractIHFSDbDao;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * IHFS Database Dao for interacting with the {@link Datalimits} entity.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 22, 2016 5699       bkowal      Initial creation
 * Nov 10, 2016 5999       bkowal      Added {@link #getObsLimits()}.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class DatalimitsDao extends AbstractIHFSDbDao<Datalimits, DatalimitsId> {

    public DatalimitsDao() {
        super(Datalimits.class);
    }

    /**
     * Retrieves all of the {@link Datalimits} records.
     * 
     * @return a {@link List} of the {@link Datalimits} records that were
     *         retrieved.
     */
    public List<Datalimits> getAllDatalimitRecords() {
        return findByNamedQuery(Datalimits.SELECT_ALL_DATALIMITS_RECORDS);
    }

    public List<Datalimits> getObsLimits() {
        List<?> returnObjects = findPartialRecordByNamedQuery(
                Datalimits.SELECT_OBS_LIMITS_FROM_DATALIMITS);
        if (returnObjects.isEmpty()) {
            return Collections.emptyList();
        }

        List<Datalimits> records = new ArrayList<>(returnObjects.size());
        for (Object returnObject : returnObjects) {
            Object[] objects = (Object[]) returnObject;
            // column return order: id, monthdayend, rocMax, alarmRocLimit,
            // alertRocLimit
            Datalimits datalimits = new Datalimits();
            datalimits.setId((DatalimitsId) objects[0]);
            datalimits.setMonthdayend((String) objects[1]);
            if (objects[2] != null) {
                datalimits.setRocMax((double) objects[2]);
            }
            if (objects[3] != null) {
                datalimits.setAlarmRocLimit((double) objects[3]);
            }
            if (objects[4] != null) {
                datalimits.setAlertRocLimit((double) objects[4]);
            }
            records.add(datalimits);
        }
        return records;
    }
}