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

import java.util.Collections;
import java.util.List;

import com.raytheon.uf.common.dataplugin.shef.tables.Rwparams;
import com.raytheon.uf.common.dataplugin.shef.tables.RwparamsId;
import com.raytheon.uf.edex.plugin.mpe.dao.AbstractIHFSDbDao;

/**
 * IHFS Database Dao for interacting with the {@link Rwparams} entity.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 30, 2016 5631       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class RwparamsDao extends AbstractIHFSDbDao<Rwparams, RwparamsId> {

    public RwparamsDao() {
        super(Rwparams.class);
    }

    /**
     * Retrieves all available {@link Rwparams} record(s) from the database.
     * 
     * @return a {@link List} of the retrieved {@link Rwparams}s.
     */
    public List<Rwparams> getRwparamsRecords() {
        List<Rwparams> records = findByNamedQuery(Rwparams.SELECT_RWPARAMS);
        if (records == null || records.isEmpty()) {
            return Collections.emptyList();
        }
        return records;
    }
}