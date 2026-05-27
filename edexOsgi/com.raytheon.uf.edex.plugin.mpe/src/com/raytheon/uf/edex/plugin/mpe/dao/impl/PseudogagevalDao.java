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

import com.raytheon.uf.common.dataplugin.shef.tables.Pseudogageval;
import com.raytheon.uf.common.dataplugin.shef.tables.PseudogagevalId;
import com.raytheon.uf.edex.plugin.mpe.dao.AbstractIHFSDbDao;

import java.util.Date;
import java.util.List;

/**
 * IHFS Database Dao for interacting with the {@link Pseudogageval} entity.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 6, 2016  5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class PseudogagevalDao
        extends AbstractIHFSDbDao<Pseudogageval, PseudogagevalId> {

    public PseudogagevalDao() {
        super(Pseudogageval.class);
    }

    public List<Pseudogageval> retrieveGagesWithinTimeSpan(final Date startTime,
            final Date endTime) {
        return findByNamedQueryAndNamedParams(
                Pseudogageval.SELECT_WITHIN_OBS_TIME_ORDER_ASC,
                new String[] { "startTime", "endTime" },
                new Object[] { startTime, endTime });
    }
}