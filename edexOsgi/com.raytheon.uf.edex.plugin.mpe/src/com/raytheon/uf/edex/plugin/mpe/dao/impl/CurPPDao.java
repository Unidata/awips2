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
import java.util.List;

import com.raytheon.uf.common.dataplugin.shef.tables.Curpp;
import com.raytheon.uf.common.dataplugin.shef.tables.CurppId;
import com.raytheon.uf.edex.plugin.mpe.dao.AbstractIHFSDbDao;

/**
 * IHFS Database Dao for interacting with the {@link Curpp} entity.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 14, 2016 5571       skorolev    Initial creation
 * 
 * </pre>
 * 
 * @author skorolev
 */
public class CurPPDao extends AbstractIHFSDbDao<Curpp, CurppId> {

    public CurPPDao() {
        super(Curpp.class);
    }

    /**
     * Gets list of CurPP records.
     * 
     * @param start
     * @param finish
     * @return
     * @throws Exception
     */
    public List<Curpp> getRecordList(Date start, Date finish) throws Exception {

        return findByNamedQueryAndNamedParams(
                Curpp.SELECT_CURPP_BY_START_AND_FINISH, new String[] { "start",
                        "finish" }, new Object[] { start, finish });
    }
}
