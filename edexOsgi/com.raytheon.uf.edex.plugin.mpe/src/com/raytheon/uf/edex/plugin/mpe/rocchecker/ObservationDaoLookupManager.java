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
package com.raytheon.uf.edex.plugin.mpe.rocchecker;

import com.raytheon.uf.edex.plugin.mpe.dao.AbstractIHFSObservationDbDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.HeightDao;

import java.util.Map;
import java.util.HashMap;

import com.raytheon.uf.common.dataplugin.shef.data.Observation;
import com.raytheon.uf.common.dataplugin.shef.tables.Height;

/**
 * Keeps track of and allows access to the {@link AbstractIHFSObservationDbDao}
 * associated with entities that {@link Observation}s can be constructed from.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 22, 2016 5699           bkowal     Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class ObservationDaoLookupManager {

    private static final ObservationDaoLookupManager INSTANCE = new ObservationDaoLookupManager();

    private final Map<Class<?>, AbstractIHFSObservationDbDao<?, ?>> entityObservationDaoMap = new HashMap<>(
            ObservationTable.values().length, 1.0f);

    protected ObservationDaoLookupManager() {
        entityObservationDaoMap.put(Height.class, new HeightDao());
        /*
         * TODO: register other observation daos.
         */
    }

    public static ObservationDaoLookupManager getInstance() {
        return INSTANCE;
    }

    public AbstractIHFSObservationDbDao<?, ?> lookupObservationDao(
            final Class<?> entityClass) {
        return entityObservationDaoMap.get(entityClass);
    }
}