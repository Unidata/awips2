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

import java.util.Calendar;
import java.util.List;

import com.raytheon.uf.common.dataplugin.shef.tables.DPRRadar;
import com.raytheon.uf.common.dataplugin.shef.tables.DPRRadarId;
import com.raytheon.uf.edex.plugin.mpe.dao.AbstractIHFSDbDao;

/**
 * IHFS Database Dao for interacting with the {@link DPRRadar} entity.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 22, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class DPRRadarDao extends AbstractIHFSDbDao<DPRRadar, DPRRadarId>
        implements IGriddedRadarRetrievalDao<DPRRadar> {

    public DPRRadarDao() {
        super(DPRRadar.class);
    }

    /**
     * Selects all {@link DPRRadar} records for the specified radar id between
     * the specified start and end obs times.
     * 
     * @param radarId
     *            the specified radar id
     * @param startObsTime
     *            the specified start obs time
     * @param endObsTime
     *            the specified end obs time
     * @return a {@link List} of retrieved {@link DPRRadar}s
     */
    @Override
    public List<DPRRadar> selectByRadIdBetweenObsTime(final String radarId,
            final Calendar startObsTime, final Calendar endObsTime) {
        return findByNamedQueryAndNamedParams(
                DPRRadar.SELECT_BY_RAD_ID_BETWEEN_OBS_TIME,
                new String[] { "radid", "startObsTime", "endObsTime" },
                new Object[] { radarId, startObsTime.getTime(),
                        endObsTime.getTime() });
    }
}