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
package com.raytheon.edex.plugin.grib.decoderpostprocessors.precipitation;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.JAXBException;

import com.raytheon.edex.plugin.grib.decoderpostprocessors.DecoderPostProcessor;
import com.raytheon.edex.plugin.grib.decoderpostprocessors.GribPostProcessor;
import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.parameter.lookup.ParameterLookup;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.plugin.grid.dao.GridDao;

/**
 * Configurable grib post processor for accumulating precipitation.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 28, 2015 3756       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */
public class PrecipAccumPostProcessor extends DecoderPostProcessor {

    private static final GridRecord[] EMPTY_ARR = new GridRecord[0];

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GribPostProcessor.class);

    /**
     * The configured set of Accumulation Configurations. Note: This is not
     * thread safe if modified.
     */
    private static final AccumulationConfigs configs = initConfigs();

    /**
     * Constructor.
     *
     * @throws JAXBException
     */
    public PrecipAccumPostProcessor() {
        super();
    }

    @Override
    public GridRecord[] process(GridRecord record) throws GribException {
        if (configs == null) {
            return EMPTY_ARR;
        }

        return generatePrecipGrids(record);
    }

    /**
     * Generates precipitation grids for a record. If no records can be
     * generated, an empty array is returned.
     *
     * @param record
     *            The record.
     * @return The generated precipitation grids, if any.
     * @throws GribException
     */
    private GridRecord[] generatePrecipGrids(GridRecord record)
            throws GribException {
        List<GridRecord> retRecords = new ArrayList<>();

        List<AccumulationConfig> accumConfigs = configs.getAccumulations(record
                .getDatasetId());

        for (AccumulationConfig accumConfig : accumConfigs) {
            retRecords.addAll(generatePrecipGrids(record, accumConfig));
        }

        return retRecords.toArray(EMPTY_ARR);
    }

    /**
     * Generates precipitation grids for a record and an accumulation config. An
     * empty collection is returned if no grids were created.
     *
     * @param record
     *            The record.
     * @param accumConfig
     *            An accumulation config who's model matches the record's
     *            dataset id.
     * @return The created records, if any.
     * @throws GribException
     */
    private Collection<GridRecord> generatePrecipGrids(GridRecord record,
            AccumulationConfig accumConfig) throws GribException {
        List<GridRecord> retRecords = new ArrayList<>();

        List<AccumulationCreationConfig> creations = accumConfig.getCreations(
                record.getParameter().getAbbreviation(), true);
        for (AccumulationCreationConfig creation : creations) {
            retRecords.addAll(generatePrecipGrids(record, creation, true));
        }

        // look for out-of-order creations where this is the subtrahend
        creations = accumConfig.getCreations(record.getParameter()
                .getAbbreviation(), false);
        for (AccumulationCreationConfig creation : creations) {
            retRecords.addAll(generatePrecipGrids(record, creation, false));
        }

        return retRecords;
    }

    /**
     * Generates precipitation grids for a record and accumulation creation
     * config. An empty collection is returned if no grids were created.
     *
     * @param record
     *            The record.
     * @param creation
     *            The accumulation creation config.
     * @param isMinuend
     *            If true, the record will is used as the minuend. If false, the
     *            record is used as the subtrahend.
     * @return The created records, if any.
     * @throws GribException
     */
    private Collection<? extends GridRecord> generatePrecipGrids(
            GridRecord record, AccumulationCreationConfig creation,
            boolean isMinuend) throws GribException {

        List<GridRecord> minuendInv = new ArrayList<>();
        List<GridRecord> subtrahendInv = new ArrayList<>();
        Set<Integer> targetForecastInv = new HashSet<>();
        initInventories(record, isMinuend, creation, minuendInv, subtrahendInv,
                targetForecastInv);

        List<GridRecord> retRecords = new ArrayList<>();
        int targetForecast = creation.getForecastPeriodSeconds();
        int minFcst;
        for (GridRecord minRec : minuendInv) {
            minFcst = minRec.getDataTime().getFcstTime();
            if (!targetForecastInv.contains(minFcst)) {
                targetForecastInv.add(minFcst);
                if (isMinuend && minFcst == targetForecast) {
                    /*
                     * this record matches the target forecast period. create a
                     * copy of it with the desired parameter.
                     */
                    retRecords.add(createRecord(creation, minRec, null));
                } else {
                    for (GridRecord subRec : subtrahendInv) {
                        if (minFcst - subRec.getDataTime().getFcstTime() == targetForecast) {
                            retRecords.add(createRecord(creation, minRec,
                                    subRec));
                        }
                    }
                }
            }
        }

        return retRecords;
    }

    /**
     * Initializes minuendInv, subtrahendInv, and targetForecastInv.
     *
     * @throws GribException
     */
    private void initInventories(GridRecord record, boolean isMinuend,
            AccumulationCreationConfig create, List<GridRecord> minuendInv,
            List<GridRecord> subtrahendInv, Set<Integer> targetForecastInv)
            throws GribException {
        String datasetId = record.getDatasetId();
        Date refTime = record.getDataTime().getRefTime();
        if (isMinuend) {
            minuendInv.add(record);
            subtrahendInv.addAll(getPrecipInventory(refTime,
                    create.getSubtrahendParam(), datasetId));
        } else {
            minuendInv.addAll(getPrecipInventory(refTime,
                    create.getMinuendParam(), datasetId));
            subtrahendInv.add(record);
        }
        targetForecastInv.addAll(getPrecipForecastTimes(refTime,
                create.getAccumulationParam(), datasetId));
    }

    /**
     * @param creation
     *            The accumulation creation config.
     * @param minRec
     *            The minuend record.
     * @param subRec
     *            The subtrahend record. Can be null if the minRec's forecast
     *            time matches the desired forecast period, skipping the need
     *            to calculate the precipitation values.
     * @return
     * @throws GribException
     */
    protected GridRecord createRecord(AccumulationCreationConfig creation,
            GridRecord minRec, GridRecord subRec) throws GribException {
        GridRecord created = createAccumRecord(creation, minRec);
        if (subRec != null) {
            calculatePrecipValues(subRec, created);
        }
        return created;
    }

    /**
     * Calculates the accumulation values. In general, this is the difference
     * between the created record's value (which is currently a copy from the
     * minuend record's value) and the subtrahend record's value, i.e.
     * createdValue[i] = minuendValue[i] - subtrahendValue[i], capped to 0 on
     * the low end.
     *
     * @param subRec
     *            The subtrahend record.
     * @param created
     *            The record that was created. It's expected that it's current
     *            data is a copy of the minuend record's data.
     * @throws GribException
     */
    protected void calculatePrecipValues(GridRecord subRec, GridRecord created)
            throws GribException {
        float[] subData = getMessageData(subRec);
        float[] accumData = (float[]) created.getMessageData();

        float newVal;
        for (int i = 0; i < accumData.length; i++) {
            newVal = accumData[i] - subData[i];
            accumData[i] = newVal < 0F ? 0F : newVal;
        }
    }

    /**
     * Creates the record for the accumulation based on the accumulation
     * creation config and base record.
     *
     * @param creation
     *            The accumulation creation config.
     * @param baseRec
     *            The record to base the accumulation off of.
     * @return
     * @throws GribException
     */
    protected GridRecord createAccumRecord(AccumulationCreationConfig creation,
            GridRecord baseRec) throws GribException {

        GridRecord createdRec = new GridRecord(baseRec);
        createdRec.setId(0);
        createdRec.getInfo().setId(null);
        createdRec.setDataURI(null);

        float[] data = getMessageData(baseRec);
        float[] dataCopy = Arrays.copyOf(data, data.length);
        createdRec.setMessageData(dataCopy);

        updateParameter(creation, baseRec, createdRec);
        updateRefTime(creation, createdRec);

        return createdRec;
    }

    /**
     * Updates the created record's datatime.
     *
     * @param creation
     * @param createdRec
     */
    private void updateRefTime(AccumulationCreationConfig creation,
            GridRecord createdRec) {
        Calendar refTime = createdRec.getDataTime().getRefTimeAsCalendar();
        int fcstTime = createdRec.getDataTime().getFcstTime();


        // Calculate the end time by adding the reference time + forecast time
        Calendar endTime = (Calendar) refTime.clone();
        endTime.add(Calendar.SECOND, fcstTime);

        // Start time is endTime - forecast period
        Calendar startTime = (Calendar) refTime.clone();
        startTime.add(Calendar.SECOND,
                fcstTime - creation.getForecastPeriodSeconds());

        TimeRange validPeriod = new TimeRange(startTime, endTime);
        DataTime newDataTime = new DataTime(refTime, fcstTime, validPeriod);

        createdRec.setDataTime(newDataTime);
    }

    /**
     * Updates the created record's parameter.
     *
     * @param creation
     * @param base
     * @param createdRec
     */
    private void updateParameter(AccumulationCreationConfig creation,
            GridRecord base, GridRecord createdRec) {
        Parameter param = ParameterLookup.getInstance().getParameter(
                creation.getAccumulationParam());
        Parameter createdParam;
        if (param == null) {
            createdParam = new Parameter(creation.getAccumulationParam(),
                    "Precipitation Accumulation", base.getParameter().getUnit());
        } else {
            createdParam = new Parameter(param.getAbbreviation(),
                    param.getName(), base.getParameter().getUnit());
        }
        createdRec.setParameter(createdParam);
    }

    /**
     * Retrieves an inventory of GridRecords for the reftime, parameter
     * abbreviation, and datasetId
     *
     * @param refTime
     *            The reftime.
     * @param param
     *            The parameter abbreviation.
     * @param datasetId
     *            The datasetId.
     * @return
     * @throws GribException
     */
    @SuppressWarnings("unchecked")
    private List<GridRecord> getPrecipInventory(Date refTime, String param,
            String datasetId) throws GribException {
        GridDao dao = null;
        try {
            dao = new GridDao();
        } catch (PluginException e) {
            throw new GribException("Error instantiating grib dao.", e);
        }
        DatabaseQuery query = new DatabaseQuery(GridRecord.class);
        query.addQueryParam(GridConstants.PARAMETER_ABBREVIATION, param);
        query.addQueryParam(GridConstants.DATASET_ID, datasetId);
        query.addQueryParam("dataTime.refTime", refTime);
        query.addOrder("dataTime.fcstTime", true);

        try {
            return (List<GridRecord>) dao.queryByCriteria(query);
        } catch (DataAccessLayerException e) {
            throw new GribException("Error getting " + param
                    + " inventory for " + datasetId, e);
        }
    }

    /**
     * Retrieves an inventory of forecast times for the refTime, parameter
     * abbreviation, and datasetId.
     *
     * @param refTime
     *            The reftime.
     * @param param
     *            The parameter abbreviation.
     * @param datasetId
     *            The datasetId.
     * @return
     * @throws GribException
     */
    @SuppressWarnings("unchecked")
    private List<Integer> getPrecipForecastTimes(Date refTime, String param,
            String datasetId) throws GribException {
        GridDao dao = null;
        try {
            dao = new GridDao();
        } catch (PluginException e) {
            throw new GribException("Error instantiating grib dao!", e);
        }
        DatabaseQuery query = new DatabaseQuery(GridRecord.class);
        query.addQueryParam(GridConstants.PARAMETER_ABBREVIATION, param);
        query.addQueryParam(GridConstants.DATASET_ID, datasetId);
        query.addQueryParam("dataTime.refTime", refTime);
        query.addReturnedField("dataTime.fcstTime");
        try {
            return (List<Integer>) dao.queryByCriteria(query);
        } catch (DataAccessLayerException e) {
            throw new GribException("Error getting " + param
                    + " forecast inventory for " + datasetId, e);
        }
    }

    /**
     * Initializes the configuration.
     */
    private static AccumulationConfigs initConfigs() {
        AccumulationConfigs retConfigs = null;

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationLevel[] levels = new LocalizationLevel[] {
                LocalizationLevel.BASE, LocalizationLevel.REGION,
                LocalizationLevel.CONFIGURED, LocalizationLevel.SITE };

        LocalizationFile locFile;

        Map<LocalizationLevel, LocalizationFile> files = pathMgr
                .getTieredLocalizationFile(LocalizationType.EDEX_STATIC,
                        "/grib/postProcessModels/precipitationAccumulation.xml");
        AccumulationConfigs configs = null;
        for (LocalizationLevel level : levels) {
            locFile = files.get(level);
            if (locFile == null) {
                continue;
            }

            try (InputStream is = locFile.openInputStream()) {
                JAXBManager manager = new JAXBManager(AccumulationConfigs.class);
                configs = (AccumulationConfigs) manager
                        .unmarshalFromInputStream(is);
            } catch (IOException | LocalizationException | JAXBException
                    | SerializationException e) {
                statusHandler
                        .fatal("Could not initialize the precipitation accumulation config.",
                                e);
            }
            if (retConfigs == null) {
                retConfigs = configs;
            } else {
                retConfigs.merge(configs);
            }
        }
        return retConfigs;
    }

    @Override
    public PostProcessorType getType() {
        return PostProcessorType.POST_PERSIST;
    }
}
