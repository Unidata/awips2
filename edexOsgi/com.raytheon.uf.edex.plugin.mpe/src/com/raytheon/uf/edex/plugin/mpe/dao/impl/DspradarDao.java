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

import com.raytheon.uf.common.dataplugin.shef.tables.Dspradar;
import com.raytheon.uf.common.dataplugin.shef.tables.DspradarId;
import com.raytheon.uf.edex.plugin.mpe.dao.AbstractIHFSDbDao;

/**
 * IHFS Database Dao for interacting with the {@link Dspradar} entity.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 21, 2016 5588       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 */

public class DspradarDao extends AbstractIHFSDbDao<Dspradar, DspradarId>
        implements IGriddedRadarRetrievalDao<Dspradar> {

    public DspradarDao() {
        super(Dspradar.class);
    }

    @Override
    public List<Dspradar> selectByRadIdBetweenObsTime(String radarId,
            Calendar startObsTime, Calendar endObsTime) {
        return findByNamedQueryAndNamedParams(
                Dspradar.SELECT_BY_RAD_ID_BETWEEN_OBS_TIME,
                new String[] { "radid", "startObsTime", "endObsTime" },
                new Object[] { radarId, startObsTime.getTime(),
                        endObsTime.getTime() });
    }
}