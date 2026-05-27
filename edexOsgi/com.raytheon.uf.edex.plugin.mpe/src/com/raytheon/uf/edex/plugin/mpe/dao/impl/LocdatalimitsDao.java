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

import com.raytheon.uf.common.dataplugin.shef.tables.Locdatalimits;
import com.raytheon.uf.common.dataplugin.shef.tables.LocdatalimitsId;
import com.raytheon.uf.edex.plugin.mpe.dao.AbstractIHFSDbDao;

import java.util.ArrayList;
import java.util.List;
import java.util.Collections;
/**
 * IHFS Database Dao for interacting with the {@link Locdatalimits} entity.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 22, 2016 5699       bkowal      Initial creation
 * Sep 13, 2016 5631       bkowal      Added {@link #getMaxPrecipForLid(String)}.
 * Nov 10, 2016 5999       bkowal      Added {@link #getObsLimits()}.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class LocdatalimitsDao
        extends AbstractIHFSDbDao<Locdatalimits, LocdatalimitsId> {

    public LocdatalimitsDao() {
        super(Locdatalimits.class);
    }

    public List<Locdatalimits> getAllLocdatalimitRecords() {
        return findByNamedQuery(Locdatalimits.SELECT_ALL_LOCDATALIMITS_RECORDS);
    }

    public List<Locdatalimits> getObsLimits() {
        List<?> returnObjects = findPartialRecordByNamedQuery(
                Locdatalimits.SELECT_OBS_LIMITS_FROM_LOCDATALIMITS);
        if (returnObjects.isEmpty()) {
            return Collections.emptyList();
        }

        List<Locdatalimits> records = new ArrayList<>(returnObjects.size());
        for (Object returnObject : returnObjects) {
            Object[] objects = (Object[]) returnObject;
            // column return order: id, monthdayend, rocMax, alarmRocLimit,
            // alertRocLimit
            Locdatalimits locdatalimits = new Locdatalimits();
            locdatalimits.setId((LocdatalimitsId) objects[0]);
            locdatalimits.setMonthdayend((String) objects[1]);
            if (objects[2] != null) {
                locdatalimits.setRocMax((double) objects[2]);
            }
            if (objects[3] != null) {
                locdatalimits.setAlarmRocLimit((double) objects[3]);
            }
            if (objects[4] != null) {
                locdatalimits.setAlertRocLimit((double) objects[4]);
            }
            records.add(locdatalimits);
        }
        return records;
    }

    /**
     * Retrieves the {@link Locdatalimits} records with pe = 'PP' and dur in
     * (1006, 2001, 5004).
     * 
     * @return a {@link List} of the {@link Locdatalimits} records
     */
    public List<Locdatalimits> getPeAndDurInLocdatalimitRecords() {
        return findByNamedQuery(
                Locdatalimits.SELECT_PE_AND_DUR_LOCDATALIMITS_RECORDS);
    }

    /**
     * Retrieves {@link Locdatalimits} records with pe = "TA".
     * 
     * @return a {@link List} of the {@link Locdatalimits} records
     */
    public List<Locdatalimits> getPeFromDatalimitRecords() {
        return findByNamedQuery(Locdatalimits.SELECT_PE_LOCDATALIMITS_RECORDS);
    }

    /**
     * Retrieves the {@link Locdatalimits} records with pe = 'PP' and dur = 1001
     * for the specified lid.
     * 
     * @param lid
     *            the specified lid
     * @return the {@link List} of {@link Locdatalimits} records that were
     *         retrieved
     */
    public List<Locdatalimits> getMaxPrecipForLid(final String lid) {
        return findByNamedQueryAndNamedParam(
                Locdatalimits.SELECT_MAX_LOCDATALIMITS_PRECIP, "lid", lid);
    }
    
    /**
     * Retrieves the {@link Locdatalimits} associated with the PP (Precipitation
     * Accumulation) Physical Element and a duration code of 1006, 2001, or
     * 5004.
     * 
     * @return the retrieved {@link Locdatalimits}
     */
    public List<Locdatalimits> getPPWithDur1006_2001_5004Limits() {
        List<?> returnObjects = findPartialRecordByNamedQuery(
                Locdatalimits.SELECT_PP_WITH_DUR_1006_2001_5004_LIMITS_FROM_LOCDATALIMITS);
        if (returnObjects.isEmpty()) {
            return Collections.emptyList();
        }

        List<Locdatalimits> records = new ArrayList<>(returnObjects.size());
        for (Object returnObject : returnObjects) {
            Object[] objects = (Object[]) returnObject;
            // column return order: id, monthdayend, grossRangeMax
            Locdatalimits locdatalimits = new Locdatalimits();
            locdatalimits.setId((LocdatalimitsId) objects[0]);
            locdatalimits.setMonthdayend((String) objects[1]);
            if (objects[2] != null) {
                locdatalimits.setGrossRangeMax((double) objects[2]);
            }
            records.add(locdatalimits);
        }
        return records;
    }

    /**
     * Retrieves the {@link Locdatalimits} associated with the TA (Temperature)
     * Physical Element.
     * 
     * @return the retrieved {@link Locdatalimits}
     */
    public List<Locdatalimits> getTALimits() {
        List<?> returnObjects = findPartialRecordByNamedQuery(
                Locdatalimits.SELECT_TA_LIMITS_FROM_LOCDATALIMITS);
        if (returnObjects.isEmpty()) {
            return Collections.emptyList();
        }

        List<Locdatalimits> records = new ArrayList<>(returnObjects.size());
        for (Object returnObject : returnObjects) {
            Object[] objects = (Object[]) returnObject;
            // column return order: id, monthdayend, grossRangeMin,
            // grossRangeMax
            Locdatalimits locdatalimits = new Locdatalimits();
            locdatalimits.setId((LocdatalimitsId) objects[0]);
            locdatalimits.setMonthdayend((String) objects[1]);
            if (objects[2] != null) {
                locdatalimits.setGrossRangeMin((double) objects[2]);
            }
            if (objects[3] != null) {
                locdatalimits.setGrossRangeMax((double) objects[3]);
            }
            records.add(locdatalimits);
        }
        return records;
    }
}