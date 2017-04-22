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
package com.raytheon.uf.edex.plugin.mpe.dao;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;
import java.util.Calendar;
import java.util.Collection;
import java.util.ArrayList;

import com.raytheon.uf.common.dataplugin.shef.data.IObservation;
import com.raytheon.uf.common.dataplugin.shef.data.ObsHqlConstants;
import com.raytheon.uf.common.dataplugin.shef.data.Observation;

/**
 * Further abstraction of a {@link AbstractIHFSDbDao} used to provide a common
 * access point for retrieval of data records as {@link Observation}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 21, 2016 5699       bkowal      Initial creation
 * Jun 29, 2016 5699       bkowal      Added {@link #updateObservationQualityCode(Observation, long)}
 *                                     and {@link #retrieveById(Observation)}.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public abstract class AbstractIHFSObservationDbDao<T extends IObservation, I extends Serializable>
        extends AbstractIHFSDbDao<T, I> {

    /*
     * Retrieve all available observations between two time periods with the
     * specified quality.
     */
    private final String getObservationsQuery;

    /*
     * Retrieve all available observations between two time periods with the
     * specified quality and lid.
     */
    private final String getObservationsForLidQuery;

    /*
     * Retrieve all available observations between the two time periods with the
     * specified quality and PE.
     */
    private final String getObservationsForPEQuery;

    /*
     * Retrieve all available observations between two time periods with the
     * specified quality, lid, and PE.
     */
    private final String getObservationsForLidAndPeQuery;

    /**
     * Constructor.
     * 
     * @param entityClass
     *            the entity that the dao is associated with
     * @param getObservationsQuery
     *            name of query used to retrieve all available observations
     *            between two time periods with the specified quality.
     * @param getObservationsForLidQuery
     *            name of query used to retrieve all available observations
     *            between two time periods with the specified quality and lid.
     * @param getObservationsForPEQuery
     *            name of query used to retrieve all available observations
     *            between the two time periods with the specified quality and
     *            PE.
     * @param getObservationsForLidAndPeQuery
     *            name of query used to retrieve all available observations
     *            between two time periods with the specified quality, lid, and
     *            PE.
     */
    protected AbstractIHFSObservationDbDao(Class<T> entityClass,
            final String getObservationsQuery,
            final String getObservationsForLidQuery,
            final String getObservationsForPEQuery,
            final String getObservationsForLidAndPeQuery) {
        super(entityClass);
        if (getObservationsQuery == null) {
            throw new IllegalArgumentException(
                    "Required argument 'getObservationsQuery' cannot be NULL.");
        }
        if (getObservationsForLidQuery == null) {
            throw new IllegalArgumentException(
                    "Required argument 'getObservationsForLidQuery' cannot be NULL.");
        }
        if (getObservationsForPEQuery == null) {
            throw new IllegalArgumentException(
                    "Required argument 'getObservationsForPEQuery' cannot be NULL.");
        }
        if (getObservationsForLidAndPeQuery == null) {
            throw new IllegalArgumentException(
                    "Required argument 'getObservationsForLidAndPeQuery' cannot be NULL.");
        }
        this.getObservationsQuery = getObservationsQuery;
        this.getObservationsForLidQuery = getObservationsForLidQuery;
        this.getObservationsForPEQuery = getObservationsForPEQuery;
        this.getObservationsForLidAndPeQuery = getObservationsForLidAndPeQuery;
    }

    /**
     * Retrieves all {@link Observation}s with the specified quality excluding
     * records set to the specified missing value between the specified
     * beginning and end date/time.
     * 
     * @param beginObsTime
     *            the specified beginning date/time
     * @param endObsTime
     *            the specified ending date/time
     * @param qualityCode
     *            the specified quality
     * @param missingValue
     *            the specified missing value
     * @return a {@link List} of the {@link Observation}s that were retrieved
     */
    public List<Observation> getObservations(final Calendar beginObsTime,
            final Calendar endObsTime, final Integer qualityCode,
            final Double missingValue) {
        validateCommonArguments(beginObsTime, endObsTime, qualityCode,
                missingValue);
        final String[] parameters = new String[] {
                ObsHqlConstants.START_OBS_TIME_PARAM,
                ObsHqlConstants.END_OBS_TIME_PARAM,
                ObsHqlConstants.QUALITY_CODE_PARAM, ObsHqlConstants.VALUE_PARAM };
        final Object[] values = new Object[] { beginObsTime.getTime(),
                endObsTime.getTime(), qualityCode, missingValue };

        return convertRecordsToObservations(findByNamedQueryAndNamedParams(
                getObservationsQuery, parameters, values));
    }

    /**
     * Retrieves all {@link Observation}s with the specified quality for the
     * specified lid(s) excluding records set to the specified missing value
     * between the specified beginning and end date/time.
     * 
     * @param beginObsTime
     *            the specified beginning date/time
     * @param endObsTime
     *            the specified ending date/time
     * @param qualityCode
     *            the specified quality
     * @param missingValue
     *            the specified missing value
     * @param lid
     *            the specified lid(s)
     * @return a {@link List} of the {@link Observation}s that were retrieved
     */
    public List<Observation> getObservationsForLid(final Calendar beginObsTime,
            final Calendar endObsTime, final Integer qualityCode,
            final Double missingValue, final Collection<String> lid) {
        validateCommonArguments(beginObsTime, endObsTime, qualityCode,
                missingValue);
        validateArgument(lid, ObsHqlConstants.LID_PARAM);
        final String[] parameters = new String[] {
                ObsHqlConstants.START_OBS_TIME_PARAM,
                ObsHqlConstants.END_OBS_TIME_PARAM,
                ObsHqlConstants.QUALITY_CODE_PARAM,
                ObsHqlConstants.VALUE_PARAM, ObsHqlConstants.LID_PARAM };
        final Object[] values = new Object[] { beginObsTime.getTime(),
                endObsTime.getTime(), qualityCode, missingValue, lid };

        return convertRecordsToObservations(findByNamedQueryAndNamedParams(
                getObservationsForLidQuery, parameters, values));
    }

    /**
     * Retrieves all {@link Observation}s with the specified quality for the
     * specified pe(s) excluding records set to the specified missing value
     * between the specified beginning and end date/time.
     * 
     * @param beginObsTime
     *            the specified beginning date/time
     * @param endObsTime
     *            the specified ending date/time
     * @param qualityCode
     *            the specified quality
     * @param missingValue
     *            the specified missing value
     * @param pe
     *            the specified pe(s)
     * @return a {@link List} of the {@link Observation}s that were retrieved
     */
    public List<Observation> getObservationsForPE(final Calendar beginObsTime,
            final Calendar endObsTime, final Integer qualityCode,
            final Double missingValue, final Collection<String> pe) {
        validateArgument(pe, ObsHqlConstants.PE_PARAM);
        validateCommonArguments(beginObsTime, endObsTime, qualityCode,
                missingValue);
        final String[] parameters = new String[] {
                ObsHqlConstants.START_OBS_TIME_PARAM,
                ObsHqlConstants.END_OBS_TIME_PARAM,
                ObsHqlConstants.QUALITY_CODE_PARAM,
                ObsHqlConstants.VALUE_PARAM, ObsHqlConstants.PE_PARAM };
        final Object[] values = new Object[] { beginObsTime.getTime(),
                endObsTime.getTime(), qualityCode, missingValue, pe };

        return convertRecordsToObservations(findByNamedQueryAndNamedParams(
                getObservationsForPEQuery, parameters, values));
    }

    /**
     * Retrieves all {@link Observation}s with the specified quality for the
     * specified pe(s) and lid(s) excluding records set to the specified missing
     * value between the specified beginning and end date/time.
     * 
     * @param beginObsTime
     *            the specified beginning date/time
     * @param endObsTime
     *            the specified ending date/time
     * @param qualityCode
     *            the specified quality
     * @param missingValue
     *            the specified missing value
     * @param lid
     *            the specified lid(s)
     * @param pe
     *            the specified pe(s)
     * @return a {@link List} of the {@link Observation}s that were retrieved
     */
    public List<Observation> getObservationsForLidAndPE(
            final Calendar beginObsTime, final Calendar endObsTime,
            final Integer qualityCode, final Double missingValue,
            final Collection<String> lid, final Collection<String> pe) {
        validateCommonArguments(beginObsTime, endObsTime, qualityCode,
                missingValue);
        validateArgument(lid, ObsHqlConstants.LID_PARAM);
        validateArgument(pe, ObsHqlConstants.PE_PARAM);
        final String[] parameters = new String[] {
                ObsHqlConstants.START_OBS_TIME_PARAM,
                ObsHqlConstants.END_OBS_TIME_PARAM,
                ObsHqlConstants.QUALITY_CODE_PARAM,
                ObsHqlConstants.VALUE_PARAM, ObsHqlConstants.LID_PARAM,
                ObsHqlConstants.PE_PARAM };
        final Object[] values = new Object[] { beginObsTime.getTime(),
                endObsTime.getTime(), qualityCode, missingValue, lid, pe };

        return convertRecordsToObservations(findByNamedQueryAndNamedParams(
                getObservationsForLidAndPeQuery, parameters, values));
    }

    /**
     * Updates the quality code for the specified {@link Observation} to the
     * specified quality code.
     * 
     * @param observation
     *            the specified {@link Observation}
     * @param updatedQualityCode
     *            the specified quality code
     * @return
     */
    public I updateObservationQualityCode(final Observation observation,
            final long updatedQualityCode) {
        final I id = getIdForObservation(observation);
        if (id == null) {
            throw new IllegalStateException(
                    "Received unexpected NULL id. Method 'getIdForObservation' has not been properly implemented.");
        }

        T record = retrieveById(observation);
        if (record == null) {
            return null;
        }

        record.setQualityCode((int) updatedQualityCode);
        persist(record);

        return id;
    }

    /**
     * Retrieves the entity associated with the specified {@link Observation}
     * based on the identifier constructed from the observation.
     * 
     * @param observation
     *            the specified {@link Observation}
     * @return the retrieved entity
     */
    public T retrieveById(final Observation observation) {
        final I id = getIdForObservation(observation);
        if (id == null) {
            throw new IllegalStateException(
                    "Received unexpected NULL id. Method 'getIdForObservation' has not been properly implemented.");
        }
        return retrieveById(id);
    }

    /**
     * Validates that all arguments common to every public method in this
     * abstract dao are not {@code null}.
     * 
     * @param beginObsTime
     *            the 'beginObsTime' argument value
     * @param endObsTime
     *            the 'endObsTime' argument value
     * @param qualityCode
     *            the 'qualityCode' argument value
     * @param missingValue
     *            the 'missingValue' argument value
     */
    private void validateCommonArguments(final Calendar beginObsTime,
            final Calendar endObsTime, final Integer qualityCode,
            final Double missingValue) {
        if (beginObsTime == null) {
            throw new IllegalArgumentException(
                    "Required argument 'beginObsTime' cannot be NULL.");
        }
        if (endObsTime == null) {
            throw new IllegalArgumentException(
                    "Required argument 'endObsTime' cannot be NULL.");
        }
        if (qualityCode == null) {
            throw new IllegalArgumentException(
                    "Required argument 'qualityCode' cannot be NULL.");
        }
        if (missingValue == null) {
            throw new IllegalArgumentException(
                    "Required argument 'missingValue' cannot be NULL.");
        }
    }

    /**
     * Validates the specified {@link Collection} is not {@code null} or empty
     * 
     * @param arg
     *            the specified {@link Collection} to validate
     * @param argName
     *            the name associated with the argument
     */
    private void validateArgument(final Collection<String> arg,
            final String argName) {
        if (arg == null || arg.isEmpty()) {
            throw new IllegalArgumentException("Required argument '" + argName
                    + "' cannot be NULL or empty.");
        }
    }

    /**
     * Iteratively invokes {@link #convertRecordToObservation(Object)} for every
     * data type specific record returned by the query that was executed.
     * 
     * @param records
     *            the specified records to convert
     * @return a {@link List} of the converted record(s) as {@link Observation}s
     */
    protected List<Observation> convertRecordsToObservations(
            final List<T> records) {
        if (records == null || records.isEmpty()) {
            return Collections.emptyList();
        }

        List<Observation> observations = new ArrayList<>(records.size());
        for (T record : records) {
            observations.add(convertRecordToObservation(record));
        }
        return observations;
    }

    /**
     * Converts a record of the data type associated with this dao to a
     * {@link Observation}
     * 
     * @param record
     *            the specified record to convert
     * @return an {@link Observation}
     */
    protected abstract Observation convertRecordToObservation(T record);

    /**
     * Returns the id value of the entity associated with the specified
     * {@link Observation}.
     * 
     * @param observation
     * @return the associated id value
     */
    protected abstract I getIdForObservation(final Observation observation);
}