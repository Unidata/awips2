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

import com.raytheon.uf.common.dataplugin.shef.tables.Rwradarresult;
import com.raytheon.uf.common.dataplugin.shef.tables.RwradarresultId;
import com.raytheon.uf.edex.plugin.mpe.dao.AbstractIHFSDbDao;

import java.util.List;
import java.util.Calendar;

/**
 * IHFS Database Dao for interacting with the {@link Rwradarresult} entity.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 21, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class RwradarresultDao
        extends AbstractIHFSDbDao<Rwradarresult, RwradarresultId> {

    public RwradarresultDao() {
        super(Rwradarresult.class);
    }

    public List<Rwradarresult> selectByObsTimeOrderedByRadId(
            final Calendar obsTime) {
        return findByNamedQueryAndNamedParam(
                Rwradarresult.SELECT_BY_OBSTIME_ORDERED_BY_RAD_ID, "obstime",
                obsTime.getTime());
    }
}