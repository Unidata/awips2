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

import java.util.List;
import java.util.Calendar;
import java.util.Collections;

import com.raytheon.uf.common.dataplugin.shef.tables.Dailypp;
import com.raytheon.uf.common.dataplugin.shef.tables.DailyppId;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.plugin.mpe.dao.AbstractIHFSDbDao;

/**
 * IHFS Database Dao for interacting with the {@link Dailypp} entity.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 30, 2018 7184       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class DailyPPDao extends AbstractIHFSDbDao<Dailypp, DailyppId> {

    public DailyPPDao() {
        super(Dailypp.class);
    }

    public List<Dailypp> retrieveNonMissingDataWithinObsTimeRange(
            final Calendar startObsTime, final Calendar endObsTime) {
        final String[] names = { "startObsTime", "endObsTime" };
        final Object[] parameters = { startObsTime.getTime(),
                endObsTime.getTime() };
        List<Dailypp> results = findByNamedQueryAndNamedParams(
                Dailypp.SELECT_NON_MISSING_DATA_BETWEEN_OBS_TIME_RANGE, names,
                parameters);
        if (CollectionUtil.isNullOrEmpty(results)) {
            return Collections.emptyList();
        }
        return results;
    }
}