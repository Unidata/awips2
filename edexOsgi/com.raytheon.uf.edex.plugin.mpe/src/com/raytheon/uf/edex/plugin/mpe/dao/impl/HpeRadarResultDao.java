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

import java.util.Date;

import com.raytheon.uf.common.plugin.hpe.data.HpeRadarResult;
import com.raytheon.uf.common.plugin.hpe.data.HpeRadarResultId;
import com.raytheon.uf.edex.plugin.mpe.dao.AbstractIHFSDbDao;

/**
 * IHFS Database Dao for interacting with the {@link HpeRadarResult} entity.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 11, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class HpeRadarResultDao
        extends AbstractIHFSDbDao<HpeRadarResult, HpeRadarResultId> {

    public HpeRadarResultDao() {
        super(HpeRadarResult.class);
    }

    public HpeRadarResult retrieveByProductNameAndProductTime(
            final String productName, final Date productTime) {
        return retrieveById(new HpeRadarResultId(productName, productTime));
    }
}