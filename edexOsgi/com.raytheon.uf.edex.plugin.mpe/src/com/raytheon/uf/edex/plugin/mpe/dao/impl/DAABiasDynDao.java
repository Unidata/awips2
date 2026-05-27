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

import com.raytheon.uf.common.dataplugin.shef.tables.DAABiasDyn;
import com.raytheon.uf.common.dataplugin.shef.tables.DAABiasDynId;
import com.raytheon.uf.edex.plugin.mpe.dao.AbstractIHFSDbDao;

/**
 * IHFS Database Dao for interacting with the {@link DAABiasDyn} entity.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 3, 2016  5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class DAABiasDynDao extends AbstractIHFSDbDao<DAABiasDyn, DAABiasDynId> {

    public DAABiasDynDao() {
        super(DAABiasDyn.class);
    }

    public List<DAABiasDyn> selectForRadIdWithinTimeRange(final String radarId,
            final String officeId, final Calendar obsTime) {
        return findByNamedQueryAndNamedParams(
                DAABiasDyn.SELECT_FOR_RADID_AND_OFFICE_FOR_TIME,
                new String[] { "radId", "obsTime", "officeId" },
                new Object[] { radarId, obsTime.getTime(), officeId });
    }
}