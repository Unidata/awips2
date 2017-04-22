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
package com.raytheon.uf.edex.netcdf.decoder;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.Buffer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;

import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ucar.ma2.Section;
import ucar.nc2.Dimension;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.netcdf.decoder.exception.NetcdfDecoderException;
import com.raytheon.uf.edex.netcdf.decoder.util.NetcdfDecoderUtils;
import com.raytheon.uf.edex.netcdf.description.data.DataDescription;
import com.raytheon.uf.edex.netcdf.description.data.mask.AbstractDataMaskDescription;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.date.DataTimeDescription;
import com.raytheon.uf.edex.netcdf.description.field.indirect.DelegateFieldDescription;
import com.raytheon.uf.edex.netcdf.description.field.level.LevelDescription;
import com.raytheon.uf.edex.netcdf.description.field.parameter.ParameterDescription;
import com.raytheon.uf.edex.netcdf.description.product.NetcdfProductDescription;
import com.raytheon.uf.edex.netcdf.description.product.NetcdfProductDescriptions;

/**
 *
 * Base decoder which can read {@link NetcdfFile}s and extract out one or more
 * {@link PersistablePluginDataObject}s. The decode is controlled by
 * {@link NetcdfProductDescriptions} which are loaded from an
 * {@link IPathManager}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * May 19, 2016  5584     nabowle  Initial creation
 *
 * </pre>
 *
 * @author nabowle
 */
public abstract class AbstractNetcdfDecoder {
    protected static final Set<String> DEFAULT_EXCLUDED_FIELDS;

    static {
        /*
         * Excluded since getDataURI()/getIdentifier() will create an incomplete
         * URI which won't later be recreated when all fields have been
         * extracted.
         */
        Set<String> fields = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);
        fields.add("dataURI");
        fields.add("identifier");

        DEFAULT_EXCLUDED_FIELDS = Collections.unmodifiableSet(fields);
    }

    protected static final PersistablePluginDataObject[] EMPTY_ARRAY = new PersistablePluginDataObject[0];

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    protected List<NetcdfProductDescriptions> descriptions;

    protected LevelFactory levelFactory;

    private String localizationPath;

    public AbstractNetcdfDecoder(String localizationPath,
            IPathManager pathManager) {
        this(localizationPath, pathManager, null);
    }

    public AbstractNetcdfDecoder(String localizationPath,
            IPathManager pathManager, LevelFactory levelFactory) {
        super();
        this.localizationPath = localizationPath;

        if (levelFactory == null) {
            setLevelFactory(LevelFactory.getInstance());
        } else {
            setLevelFactory(levelFactory);
        }

        setPathManager(pathManager);
    }

    /**
     * Partially decodes the file to generate one NetcdfRecordInfo per record in
     * the file.
     *
     * @param file
     *            The netcdf file.
     * @return An iterator over the NetcdfRecordInfos
     */
    public Iterator<NetcdfRecordInfo> split(File file) {
        List<NetcdfRecordInfo> recordInfos = new ArrayList<>();
        NetcdfFile netcdfFile = null;
        List<NetcdfProductDescriptions> matchingDescriptions = new ArrayList<>();
        try {
            netcdfFile = NetcdfFile.open(file.getAbsolutePath());
            List<NetcdfProductDescriptions> theDescs = getDescriptions();
            for (NetcdfProductDescriptions descs : theDescs) {
                try {
                    /*
                     * Create records from each of the matching product
                     * descriptions that specify data to extract. Matching
                     * product descriptions that do not contain a data
                     * description will be used later to update the record infos
                     * created here.
                     */
                    if (descs.matches(netcdfFile, logger)) {
                        matchingDescriptions.add(descs);
                        List<NetcdfRecordInfo> newInfos = createRecordInfos(
                                file, netcdfFile, descs);

                        if (newInfos != null) {
                            recordInfos.addAll(newInfos);
                        }
                    }
                } catch (NetcdfDecoderException | InvalidDescriptionException e) {
                    logger.error(
                            "Error generating records from a description.", e);
                }
            }

            if (!recordInfos.isEmpty()) {
                try {
                    /*
                     * Product descriptions that specify how to extract non-data
                     * fields may be spread across multiple files, so update the
                     * record infos against the matching descriptions.
                     */
                    for (NetcdfProductDescriptions descs : matchingDescriptions) {
                        for (NetcdfProductDescription desc : descs
                                .getDescriptions()) {
                            if (desc.matches(netcdfFile, logger)
                                    && desc.getData() == null) {
                                desc.updateFields(netcdfFile, recordInfos);
                            }
                        }
                    }

                    recordInfos = processDeferredDescriptions(netcdfFile,
                            recordInfos);
                } catch (InvalidDescriptionException | NetcdfDecoderException e) {
                    logger.error("Error updating the record infos.", e);
                    recordInfos = Collections.emptyList();
                }
            } else {
                logger.warn("No valid records were found in file: {}",
                        file.getName());
            }
        } catch (IOException e) {
            logger.error("Unable to open the file {} for splitting.",
                    file.getName(), e);
            recordInfos.clear();
        } finally {
            if (netcdfFile != null) {
                try {
                    netcdfFile.close();
                } catch (IOException e) {
                    logger.warn("Error closing the netcdf file {}",
                            file.getPath(), e);
                }
            }
        }
        return recordInfos.iterator();
    }

    /**
     * Get the records from the netcdf file.
     *
     * @param file
     *            The reference to the file on disk.
     * @param netcdfFile
     *            The opened NetcdfFile
     * @return The records created from this set of descriptions.
     * @throws NetcdfDecoderException
     */
    public List<NetcdfRecordInfo> createRecordInfos(File file,
            NetcdfFile netcdfFile, NetcdfProductDescriptions descs)
            throws InvalidDescriptionException, NetcdfDecoderException {
        List<NetcdfRecordInfo> records = new ArrayList<>();
        for (NetcdfProductDescription desc : descs.getDescriptions()) {
            if (desc.matches(netcdfFile, this.logger)) {
                if (desc.getData() != null) {
                    if (desc.getData().isPresent(netcdfFile)) {
                        NetcdfRecordInfo newRecord = createRecordInfo(file,
                                netcdfFile, desc);
                        if (newRecord != null) {
                            records.add(newRecord);
                        }
                    } else if (desc.isDebug()) {
                        logger.debug("description skipped because the data variable "
                                + desc.getData().getVariableName()
                                + " is not present");
                    }
                }
            } else {
                if (desc.isDebug() && this.logger.isDebugEnabled()) {
                    logger.debug("description skipped because the file does not match the configured pattern(s).");
                }
            }
        }
        return records;
    }

    /**
     * Creates a record based on a description. The records will then be updated
     * with the fields extracted the description.
     *
     * @param file
     *            The actual file.
     * @param netcdfFile
     *            The NetcdfFile object for file.
     * @param description
     *            The description to create records from.
     * @return The created record.
     * @throws InvalidDescriptionException
     *             if this description, or the default description, is invalid.
     */
    protected NetcdfRecordInfo createRecordInfo(File file,
            NetcdfFile netcdfFile, NetcdfProductDescription description)
            throws NetcdfDecoderException {
        List<NetcdfRecordInfo> records = new ArrayList<>();

        NetcdfRecordInfo info = newRecord();
        info.setFile(file);
        records.add(info);

        try {
            description.updateFields(netcdfFile, records);
        } catch (InvalidDescriptionException e) {
            throw new NetcdfDecoderException(
                    "Unable to decode the fields while creating a record.", e);
        }

        return info;
    }

    /**
     * Process the deferred descriptions.
     *
     * Base processing decodes the datatime, level (assuming the data has
     * levels), and every multi-record field found in the initial processing,
     * creating new record infos if the data has multiple datatimes and/or
     * levels, and for every deferred field.
     *
     *
     * @param netcdfFile
     *            The netcdf file being decoded.
     * @param currentInfos
     *            The current list of record infos.
     * @return A new list of record infos that have been post-processed.
     * @throws InvalidDescriptionException
     *             If a description is invalid.
     * @throws NetcdfDecoderException
     *             If there's an issue decoding the data.
     */
    protected List<NetcdfRecordInfo> processDeferredDescriptions(
            NetcdfFile netcdfFile, List<NetcdfRecordInfo> currentInfos)
            throws InvalidDescriptionException, NetcdfDecoderException {

        List<NetcdfRecordInfo> newInfos = new ArrayList<>();
        List<NetcdfRecordInfo> levelProcessedInfos = new ArrayList<>();
        List<NetcdfRecordInfo> timeProcessedInfos = new ArrayList<>();
        for (NetcdfRecordInfo info : currentInfos) {
            levelProcessedInfos.clear();
            timeProcessedInfos.clear();
            Variable dataVar = NetcdfDecoderUtils.getDataVariable(info,
                    netcdfFile);
            Parameter param = (Parameter) info.getBeanMap().get(
                    ParameterDescription.PARAMETER_KEY);
            if (param != null && dataVar != null) {
                NetcdfDecoderUtils.updateParameterFromVariable(param, dataVar);
            }

            extractLevel(netcdfFile, info, dataVar, levelProcessedInfos);

            for (NetcdfRecordInfo levelProcessedInfo : levelProcessedInfos) {
                extractDataTime(netcdfFile, levelProcessedInfo, dataVar,
                        timeProcessedInfos);
            }

            extractDeferredFields(netcdfFile, timeProcessedInfos, newInfos);
        }

        return newInfos;
    }

    /**
     * Extracts deferred fields, creating new NetcdfRecordInfos as necessary.
     * Each record info will be cloned once per value in a deferred field
     * description. If no deferred field descriptions are found for a record
     * info, outInfos will contain that original record info.
     *
     * @param netcdfFile
     * @param infos
     * @param outInfos
     * @param deferredProcessedInfos
     * @throws InvalidDescriptionException
     */
    protected void extractDeferredFields(NetcdfFile netcdfFile,
            List<NetcdfRecordInfo> infos, List<NetcdfRecordInfo> outInfos)
            throws InvalidDescriptionException {
        List<NetcdfRecordInfo> deferredProcessedInfos = new LinkedList<>();
        for (NetcdfRecordInfo info : infos) {
            deferredProcessedInfos.clear();
            deferredProcessedInfos.add(info);
            for (Entry<String, Object> entry : info.getDeferredDescriptions()
                    .entrySet()) {
                if (entry.getValue() instanceof DelegateFieldDescription) {
                    DelegateFieldDescription deferred = (DelegateFieldDescription) entry
                            .getValue();
                    if (deferred.isPresent(netcdfFile)) {
                        boolean isNumeric = deferred.isNumeric(netcdfFile);
                        long length = deferred.getLength(netcdfFile);
                        ListIterator<NetcdfRecordInfo> iter = deferredProcessedInfos
                                .listIterator();
                        while (iter.hasNext()) {
                            NetcdfRecordInfo next = iter.next();
                            iter.remove();
                            Object fieldVal;
                            for (int i = 0; i < length; i++) {
                                if (isNumeric) {
                                    fieldVal = deferred
                                            .getNumber(netcdfFile, i);
                                } else {
                                    fieldVal = deferred
                                            .getString(netcdfFile, i);
                                }
                                NetcdfRecordInfo copy = new NetcdfRecordInfo(
                                        next, getExcludedFields());
                                copy.addField(entry.getKey(), fieldVal, true);
                                copy.addDimensionIndex(deferred.getName(), i);
                                iter.add(copy);
                            }
                        }
                    } else {
                        logger.warn("Unable to decode deferred field description: "
                                + deferred.getName()
                                + " for field "
                                + entry.getKey());
                    }
                }
            }
            outInfos.addAll(deferredProcessedInfos);
        }
    }

    /**
     * @param netcdfFile
     * @param info
     * @param dataVar
     * @param outInfos
     * @throws InvalidDescriptionException
     * @throws NetcdfDecoderException
     */
    protected void extractLevel(NetcdfFile netcdfFile,
            NetcdfRecordInfo info, Variable dataVar,
            List<NetcdfRecordInfo> outInfos)
            throws InvalidDescriptionException, NetcdfDecoderException {
        if (!info.getBeanMap().containsKey("level")) {
            outInfos.add(info);
            return;
        }

        LevelDescription levelDesc = (LevelDescription) info
                .getDeferredDescription(LevelDescription.LEVEL_KEY);
        if (levelDesc != null) {
            Level level;
            Dimension levelDim = NetcdfDecoderUtils.getLevelDimension(dataVar,
                    levelDesc);
            int numLevels = levelDim == null ? 1 : levelDim.getLength();
            if (numLevels > 1) {
                NetcdfRecordInfo infoCopy;
                for (int j = 0; j < numLevels; j++) {
                    infoCopy = new NetcdfRecordInfo(info, getExcludedFields());
                    level = levelDesc.getLevel(netcdfFile, levelFactory, j);
                    if (level == null) {
                        throw new NetcdfDecoderException(
                                "Unable to decode the configured level");
                    }
                    infoCopy.addDimensionIndex(levelDim.getName(), j);
                    infoCopy.addField(LevelDescription.LEVEL_KEY, level, true);
                    outInfos.add(infoCopy);
                }
            } else {
                level = levelDesc.getLevel(netcdfFile, this.levelFactory);
                if (level == null) {
                    throw new NetcdfDecoderException(
                            "Unable to decode the configured level");
                }
                info.addField(LevelDescription.LEVEL_KEY, level, true);
                outInfos.add(info);
            }
        } else {
            outInfos.add(info);
        }
    }

    /**
     * @param netcdfFile
     * @param info
     * @param dataVar
     * @param outInfos
     * @throws InvalidDescriptionException
     * @throws NetcdfDecoderException
     */
    protected void extractDataTime(NetcdfFile netcdfFile,
            NetcdfRecordInfo info, Variable dataVar,
            List<NetcdfRecordInfo> outInfos)
            throws InvalidDescriptionException, NetcdfDecoderException {
        DataTimeDescription dataTimeDesc = (DataTimeDescription) info
                .getDeferredDescription(DataTimeDescription.DATATIME_KEY);
        if (dataTimeDesc != null) {
            DataTime dataTime;
            Dimension dataTimeDim = NetcdfDecoderUtils.getDataTimeDimension(
                    dataVar, dataTimeDesc);
            int numDataTimes = dataTimeDim == null ? 1 : dataTimeDim
                    .getLength();
            if (numDataTimes > 1) {
                NetcdfRecordInfo infoCopy;
                for (int j = 0; j < numDataTimes; j++) {
                    infoCopy = new NetcdfRecordInfo(info, getExcludedFields());
                    dataTime = dataTimeDesc.getDataTime(netcdfFile, j);
                    if (dataTime == null) {
                        throw new NetcdfDecoderException(
                                "Unable to decode the configured dataTime");
                    }
                    infoCopy.addDimensionIndex(dataTimeDim.getName(), j);
                    infoCopy.addField(DataTimeDescription.DATATIME_KEY,
                            dataTime, true);
                    outInfos.add(infoCopy);
                }
            } else {
                dataTime = dataTimeDesc.getDataTime(netcdfFile);
                if (dataTime == null) {
                    throw new NetcdfDecoderException(
                            "Unable to decode the configured dataTime");
                }
                info.addField(DataTimeDescription.DATATIME_KEY, dataTime, true);
                outInfos.add(info);
            }
        } else {
            outInfos.add(info);
        }
    }

    /**
     * Converts a record info into the record and decodes the data for that
     * record.
     *
     * @param recordInfo
     *            The record info.
     * @return An array containing a single record, or an empty array if the
     *         record could not be decoded properly.
     */
    public PersistablePluginDataObject[] decode(NetcdfRecordInfo recordInfo) {
        NetcdfFile netcdfFile = null;
        try {
            netcdfFile = NetcdfFile
                    .open(recordInfo.getFile().getAbsolutePath());
            extractData(netcdfFile, recordInfo);

            PersistablePluginDataObject record = (PersistablePluginDataObject) recordInfo
                    .getBeanMap().getBean();

            validateRecord(record);

            return new PersistablePluginDataObject[] { record };
        } catch (NetcdfDecoderException | InvalidDescriptionException e) {
            logger.error("Unable to generate a record", e);
        } catch (IOException e) {
            logger.error("Unable to read the file, {}", recordInfo.getFile()
                    .getAbsolutePath(), e);
        } finally {
            if (netcdfFile != null) {
                try {
                    netcdfFile.close();
                } catch (IOException e) {
                    logger.warn("Error closing the netcdf file {}", recordInfo
                            .getFile().getPath(), e);
                }
            }
        }
        return EMPTY_ARRAY;
    }

    /**
     * Extract the data.
     *
     * @param netcdfFile
     *            The netcdf file.
     * @param info
     *            The record info.
     * @throws NetcdfDecoderException
     * @throws InvalidDescriptionException
     */
    protected void extractData(NetcdfFile netcdfFile, NetcdfRecordInfo info)
            throws NetcdfDecoderException, InvalidDescriptionException {
        DataDescription dataDesc = (DataDescription) info
                .getDeferredDescription(DataDescription.DATA_KEY);
        Variable dataVariable = netcdfFile.findVariable(dataDesc.getVariable()
                .getName());
        if (dataVariable == null) {
            throw new NetcdfDecoderException(
                    "Cannot find the data variable.");
        }

        Section sect = NetcdfDecoderUtils.getRecordSection(dataVariable,
                info.getDimensionIndexMap());

        Number fillValue = NetcdfDecoderUtils
                .getNoDataValue(dataVariable, null);
        Number scaleValue = NetcdfDecoderUtils.getNumericAttributeValue(
                dataVariable, null, NetcdfDecoderUtils.SCALE_FACTOR);
        Number addOffsetValue = NetcdfDecoderUtils.getNumericAttributeValue(
                dataVariable, null, NetcdfDecoderUtils.ADD_OFFSET);

        Buffer numericData = NetcdfDecoderUtils.readData(dataVariable, sect,
                fillValue, scaleValue, addOffsetValue,
                info.getFlipDimensionIndices());

        applyMasks(netcdfFile, info, dataDesc, numericData);
        if (numericData == null) {
            throw new NetcdfDecoderException("Unable to decode the data.");
        }

        addDataToInfo(info, numericData);
    }

    /**
     * @param info
     * @param numericData
     */
    protected void addDataToInfo(NetcdfRecordInfo info, Buffer numericData) {
        info.addField(DataDescription.DATA_KEY, numericData, true);
    }

    /**
     * @param netcdfFile
     * @param info
     * @param dataDesc
     * @param numericData
     * @throws NetcdfDecoderException
     */
    protected void applyMasks(NetcdfFile netcdfFile, NetcdfRecordInfo info,
            DataDescription dataDesc, Buffer numericData)
            throws NetcdfDecoderException {
        if (dataDesc.getMasks() != null) {
            try {
                for (AbstractDataMaskDescription mask : dataDesc.getMasks()) {
                    mask.applyMask(numericData, netcdfFile,
                            info.getDimensionIndexMap());
                }
            } catch (InvalidDescriptionException e) {
                throw new NetcdfDecoderException(
                        "Unable to apply the data mask.", e);
            }
        }
    }

    /**
     * The {@link IPathManager} is used to look up description files.
     */
    public void setPathManager(IPathManager pathManager) {
        JAXBManager jaxb;
        try {
            jaxb = getJaxbManager();
        } catch (JAXBException e) {
            logger.error(
                    "Unable to initialize the JAXB Manager. No descriptions can be loaded.",
                    e);
            return;
        }
        LocalizationFile[] files = pathManager.listStaticFiles(
                this.localizationPath, new String[] { ".xml" }, true, true);
        List<NetcdfProductDescriptions> descriptions = new ArrayList<>();
        for (LocalizationFile file : files) {
            logger.info("Loading netcdf product descriptions from "
                    + file.getPath());
            try (InputStream inputStream = file.openInputStream()) {
                NetcdfProductDescriptions unmarshalled = (NetcdfProductDescriptions) jaxb
                        .unmarshalFromInputStream(inputStream);
                try {
                    unmarshalled.validate();
                    unmarshalled.mergeDefaultIntoDescriptions();
                    descriptions.add(unmarshalled);
                } catch (InvalidDescriptionException e) {
                    logger.warn("Discarding product description from {}: {}.",
                            file.getPath(), e.getMessage(), e);
                }
            } catch (LocalizationException | IOException
                    | SerializationException e) {
                logger.error("Unable to load product descriptions from {}",
                        file.getPath(), e);
            }
        }
        setDescriptions(descriptions);
    }

    public List<NetcdfProductDescriptions> getDescriptions() {
        return this.descriptions;
    }

    public void setDescriptions(List<NetcdfProductDescriptions> descriptions) {
        this.descriptions = descriptions;
    }

    public void setLevelFactory(LevelFactory levelFactory) {
        this.levelFactory = levelFactory;
    }

    protected void validateRecord(PersistablePluginDataObject record)
            throws NetcdfDecoderException {
        if (record.getDataTime() == null) {
            throw new NetcdfDecoderException("dataTime is missing.");
        }
    }

    /**
     * Get the set of record fields to exclude when copying records.
     *
     * @return The set of excluded fields.
     */
    protected Set<String> getExcludedFields() {
        return DEFAULT_EXCLUDED_FIELDS;
    }

    /**
     * Get the JAXBManager to use to unmarshal descriptions.
     *
     * @return
     * @throws JAXBException
     */
    protected abstract JAXBManager getJaxbManager() throws JAXBException;

    /**
     * @return a new instance of the record class.
     */
    protected abstract NetcdfRecordInfo newRecord();
}
