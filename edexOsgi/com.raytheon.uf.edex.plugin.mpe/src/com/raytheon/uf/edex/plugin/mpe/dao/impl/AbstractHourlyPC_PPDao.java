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

import java.io.Serializable;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypc;
import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypp;
import com.raytheon.uf.edex.plugin.mpe.dao.AbstractIHFSDbDao;

/**
 * Abstract DAO defining common retrieval methods for the retrieval of
 * {@link Hourlypp} and {@link Hourlypc} records from the IHFS database.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 7, 2016  5756       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public abstract class AbstractHourlyPC_PPDao<T, I extends Serializable>
        extends AbstractIHFSDbDao<T, I> {

    public enum HOURLY_ENTITY {
        PC(Hourlypc.class), PP(Hourlypp.class);

        private final Class<?> entityClass;

        private HOURLY_ENTITY(final Class<?> entityClass) {
            this.entityClass = entityClass;
        }

        public Class<?> getEntityClass() {
            return entityClass;
        }
    }

    protected final String[] paramsLidTsObsTime = new String[] { "lid", "ts",
            "start", "finish" };

    protected final String[] paramsTsObsTime = { "ts", "start", "finish" };

    @SuppressWarnings("unchecked")
    protected AbstractHourlyPC_PPDao(HOURLY_ENTITY hourlyEntity) {
        super((Class<T>) hourlyEntity.getEntityClass());
    }

    /**
     * Retrieves hourly records using lid and ts = "ts" between start and finish
     * dates.
     * 
     * @param lid
     *            local ID
     * @param ts
     *            type-source
     * @param start
     *            obsTime
     * @param finish
     *            obsTime
     * @return the {@link List} of retrieved hourly records
     */
    public abstract List<T> getHourlyForTsSglEQLidObstime(String lid, String ts,
            Date start, Date finish);

    /**
     * Retrieves hourly records using lid and ts != "ts" between start and
     * finish dates.
     * 
     * @param lid
     *            local ID
     * @param ts
     *            type-source
     * @param start
     *            obsTime
     * @param finish
     *            obsTime
     * @return the {@link List} of retrieved hourly records
     */
    public abstract List<T> getHourlyForTsSglNOTLidObstime(String lid,
            String ts, Date start, Date finish);

    /**
     * Retrieves hourly records using lid and ts from list of "ts" between start
     * and finish dates.
     * 
     * @param lid
     *            local ID
     * @param ts
     *            type-source
     * @param start
     *            obsTime
     * @param finish
     *            obsTime
     * @return the {@link List} of retrieved hourly records
     */
    public abstract List<T> getHourlyForTsMultiEQLidObstime(String lid,
            Collection<String> ts, Date start, Date finish);

    /**
     * Retrieves hourly records using lid and ts not from list of "ts" between
     * start and finish dates.
     * 
     * @param lid
     *            local ID
     * @param ts
     *            type-source
     * @param start
     *            obsTime
     * @param finish
     *            obsTime
     * @return the {@link List} of retrieved hourly records
     */
    public abstract List<T> getHourlyForTsMultiNOTLidObstime(String lid,
            Collection<String> ts, Date start, Date finish);

    /**
     * Retrieves hourly records using ts = "ts" between start and finish dates.
     * 
     * @param ts
     *            type-source
     * @param start
     *            obsTime
     * @param finish
     *            obsTime
     * @return the {@link List} of retrieved hourly records
     */
    public abstract List<T> getHourlyForTsSglEQObstime(String ts, Date start,
            Date finish);

    /**
     * Retrieves hourly records using ts != "ts" between start and finish dates.
     * 
     * @param ts
     *            type-source
     * @param start
     *            obsTime
     * @param finish
     *            obsTime
     * @return the {@link List} of retrieved hourly records
     */
    public abstract List<T> getHourlyForTsSglNOTObstime(String ts, Date start,
            Date finish);

    /**
     * Retrieves hourly records using ts from list of "ts" between start and
     * finish dates.
     * 
     * @param ts
     *            type-source
     * @param start
     *            obsTime
     * @param finish
     *            obsTime
     * @return the {@link List} of retrieved hourly records
     */
    public abstract List<T> getHourlyForTsMultiEQObstime(Collection<String> ts,
            Date start, Date finish);

    /**
     * Retrieves hourly records using ts not from list of "ts" between start and
     * finish dates.
     * 
     * @param ts
     *            type-source
     * @param start
     *            obsTime
     * @param finish
     *            obsTime
     * @return the {@link List} of retrieved hourly records
     */
    public abstract List<T> getHourlyForTsMultiNOTObstime(Collection<String> ts,
            Date start, Date finish);

    /**
     * Retrieves hourly records using lid between start and finish dates.
     * 
     * @param lid
     *            local ID
     * @param start
     *            obsTime
     * @param finish
     *            obsTime
     * @return the {@link List} of retrieved hourly records
     */
    public abstract List<T> getHourlyForLidObstime(String lid, Date start,
            Date finish);

    /**
     * Retrieves hourly records between start and finish dates.
     * 
     * @param start
     *            obsTime
     * @param finish
     *            obsTime
     * @return the {@link List} of retrieved hourly records
     */
    public abstract List<T> getHourlyForObstime(Date start, Date finish);
}