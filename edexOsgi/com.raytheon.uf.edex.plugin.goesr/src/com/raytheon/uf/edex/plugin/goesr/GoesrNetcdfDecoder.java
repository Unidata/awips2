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
package com.raytheon.uf.edex.plugin.goesr;

import java.io.File;
import java.io.IOException;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.unit.SI;
import javax.xml.bind.JAXBException;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import ucar.nc2.Attribute;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.dataplugin.satellite.SatMapCoverage;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.edex.netcdf.decoder.AbstractNetcdfDecoder;
import com.raytheon.uf.edex.netcdf.decoder.NetcdfRecordInfo;
import com.raytheon.uf.edex.netcdf.decoder.exception.NetcdfDecoderException;
import com.raytheon.uf.edex.netcdf.decoder.util.NetcdfDecoderUtils;
import com.raytheon.uf.edex.netcdf.description.data.DataDescription;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.direct.VariableDescription;
import com.raytheon.uf.edex.netcdf.description.product.NetcdfProductDescription;
import com.raytheon.uf.edex.netcdf.description.product.NetcdfProductDescriptions;
import com.raytheon.uf.edex.plugin.goesr.decoder.lookup.GoesrNetcdfProductDescription;
import com.raytheon.uf.edex.plugin.goesr.decoder.lookup.GoesrNetcdfProductDescriptions;
import com.raytheon.uf.edex.plugin.goesr.decoder.lookup.GoesrUtils;
import com.raytheon.uf.edex.plugin.goesr.description.data.GoesrDataDescription;
import com.raytheon.uf.edex.plugin.goesr.exception.GoesrProjectionException;
import com.raytheon.uf.edex.plugin.goesr.geospatial.GoesrProjectionFactory;
import com.raytheon.uf.edex.plugin.goesr.geospatial.GoesrSatelliteHeight;

/**
 * This decoder attempts to open a potential GOES-R netCDF file, decode the data
 * contained in it, and make it available to be stored.
 *
 * <pre>
 * The code implements the
 * Ground Segment (GS) to Advanced Weather Interactive Processing System (AWIPS)
 * Interface Control Document (ICD)
 * DOCUMENT CONTROL NUMBER: 7034704 CDRL SE-08 REVISION B
 * Date 31 MAY 2012
 * *****
 * Some variances between the code and the revision are noted. These are
 * due to discrepancies between the ICD and the sample data. These have
 * been reported.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 31, 2012  796       jkorman     Initial creation
 * Jul  5, 2013  2123      mschenke    Changed to use in-memory netcdf object
 * Feb 13, 2015  4043      bsteffen    Include scene number in sector.
 * Apr 17, 2015  4336      bsteffen    Rewrite to be configurable for other attribute conventions.
 * Sep 28, 2015  4872      bsteffen    Decode File instead of byte[]
 * Mar 16, 2016  5456      bsteffen    Fix log statements
 * May 26, 2016 5584       nabowle     Refactor and rename for consolidation.
 *
 * </pre>
 *
 * @author nabowle
 */

public class GoesrNetcdfDecoder extends AbstractNetcdfDecoder {

    private static final String UNITS_KEY = "units";

    private static final String COVERAGE_KEY = "coverage";

    private GoesrProjectionFactory projectionFactory;

    public GoesrNetcdfDecoder(GoesrProjectionFactory projectionFactory,
            IPathManager manager) {
        this(getDefaultLocalizationPath(), projectionFactory, manager);
    }

    public GoesrNetcdfDecoder(String localizationPath,
            GoesrProjectionFactory projectionFactory, IPathManager manager) {
        super(localizationPath, manager);
        this.projectionFactory = projectionFactory;
    }

    @Override
    public NetcdfRecordInfo createRecordInfo(File file,
            NetcdfFile netcdfFile, NetcdfProductDescription description)
            throws NetcdfDecoderException {
        NetcdfRecordInfo record = super.createRecordInfo(file,
                netcdfFile, description);

        if (record == null) {
            return record;
        }

        if (!(description instanceof GoesrNetcdfProductDescription)) {
            throw new NetcdfDecoderException(
                    "The configured description is not a "
                            + GoesrNetcdfProductDescription.class
                                    .getSimpleName());
        }

        GoesrDataDescription dataDesc = (GoesrDataDescription) description
                .getData();

        String gridMapping = null;
        String units = null;
        if (dataDesc.getVariable() != null) {
            Variable dataVariable = netcdfFile.findVariable(dataDesc
                    .getVariable().getName());
            if (dataVariable == null) {
                if (description.isDebug()) {
                    logger.debug("Description skipped because data variable "
                            + dataDesc.getVariable().getName()
                            + " cannot be found.");
                }
                return null;
            }

            try {
                units = GoesrUtils.getUnits(netcdfFile, dataDesc, dataVariable);
            } catch (InvalidDescriptionException e) {
                throw new NetcdfDecoderException("Error decoding units.", e);
            }

            gridMapping = dataVariable.findAttribute("grid_mapping")
                    .getStringValue();
        } else if (dataDesc.getBitset() != null) {
            Variable dataVariable;
            Attribute attr;
            for (String variable : dataDesc.getBitset()) {
                dataVariable = netcdfFile.findVariable(variable);
                attr = dataVariable.findAttribute("grid_mapping");
                if (attr != null) {
                    if (gridMapping == null) {
                        gridMapping = attr.getStringValue();
                    } else if (!attr.getStringValue().equals(gridMapping)) {
                        throw new GoesrProjectionException(
                                "Bitset projections do not match.");
                    }
                }
            }
        }

        if (gridMapping == null) {
            if (description.isDebug()) {
                logger.debug("Description skipped because the gridMapping could not be decoded. ");
            }
            return null;
        }
        SatMapCoverage coverage = projectionFactory.getCoverage(netcdfFile,
                gridMapping);

        record.getBeanMap().put(COVERAGE_KEY, coverage);
        if (units != null) {
            record.addField(UNITS_KEY, units);
        }

        return record;

    }

    @Override
    protected void extractData(NetcdfFile netcdfFile, NetcdfRecordInfo info)
            throws NetcdfDecoderException, InvalidDescriptionException {
        if (this.projectionFactory == null) {
            throw new NetcdfDecoderException(
                    "Cannot decode goesr data because no projection factory is available.");
        }

        GoesrDataDescription dataDesc = (GoesrDataDescription) info
                .getDeferredDescription(DataDescription.DATA_KEY);

        VariableDescription variable = dataDesc.getVariable();
        List<String> bitset = dataDesc.getBitset();

        SatMapCoverage coverage = (SatMapCoverage) info.getField(COVERAGE_KEY);
        long[] sizes = new long[] { coverage.getNx(), coverage.getNy() };

        if (variable != null) {
            extractDataFromVariable(netcdfFile, dataDesc, info, variable, sizes);
        } else if (bitset != null) {
            try {
                extractDataFromBitset(netcdfFile, dataDesc, info, bitset, sizes);
            } catch (IOException e) {
                throw new NetcdfDecoderException(
                        "Unable to read data from bitset"
                                + Arrays.toString(bitset.toArray()), e);
            }
        }

        Number satHeight = (Number) info.getField("satHeight");
        if (satHeight == null) {
            /*
             * Some data formats, which are in a geostationary projection, do
             * not define that satellite height, except in the projection
             * metadata.
             */
            CoordinateReferenceSystem crs = ((SatMapCoverage) info
                    .getField("coverage")).getCrs();
            double satHeightD = GoesrSatelliteHeight.getOrbitalHeight(crs,
                    SI.KILOMETER);
            if (!Double.isNaN(satHeightD)) {
                info.addField("satHeight", (int) satHeightD);
            }
        }
    }

    /**
     * Extract data from a variable, similar to
     * {@link AbstractNetcdfDecoder#extractData(NetcdfFile, NetcdfRecordInfo)},
     * but insert the scale factor and add offset into the IDataRecord's
     * attributes.
     */
    private void extractDataFromVariable(NetcdfFile netcdfFile,
            DataDescription dataDesc, NetcdfRecordInfo info,
            VariableDescription variable, long[] sizes)
            throws NetcdfDecoderException {
        Variable dataVariable = netcdfFile.findVariable(variable.getName());
        if (dataVariable == null) {
            throw new NetcdfDecoderException(
                    "Unable to find the data variable "
                    + variable.getName());
        }
        Buffer data;
        data = NetcdfDecoderUtils.readData(
                dataVariable,
                NetcdfDecoderUtils.getRecordSection(dataVariable,
                        info.getDimensionIndexMap()), null, null, null, null);
        applyMasks(netcdfFile, info, dataDesc, data);

        Map<String, Object> attributes = new HashMap<>();

        Number addOffset = NetcdfDecoderUtils.getNumericAttributeValue(
                dataVariable, null, NetcdfDecoderUtils.ADD_OFFSET);
        if (addOffset != null) {
            attributes.put(SatelliteRecord.SAT_ADD_OFFSET,
                    addOffset.floatValue());
        }
        Number scaleFactor = NetcdfDecoderUtils.getNumericAttributeValue(
                dataVariable, null, NetcdfDecoderUtils.SCALE_FACTOR);
        if (scaleFactor != null) {
            attributes.put(SatelliteRecord.SAT_SCALE_FACTOR,
                    scaleFactor.floatValue());
        }

        Number fillValue = GoesrUtils.getFillValue(dataVariable, data);

        IDataRecord storageRecord = DataStoreFactory.createStorageRecord(
                SatelliteRecord.SAT_DATASET_NAME, null, data.array(), 2, sizes);
        storageRecord.setFillValue(fillValue);
        storageRecord.setDataAttributes(attributes);
        storageRecord.setGroup(((SatelliteRecord) info.getBeanMap().getBean())
                .getDataURI());
        info.addField(DataDescription.DATA_KEY, storageRecord, true);
    }

    private void extractDataFromBitset(NetcdfFile netcdfFile,
            DataDescription dataDesc, NetcdfRecordInfo info,
            List<String> bitset, long[] sizes) throws IOException,
            NetcdfDecoderException {
        if (bitset.size() > 7) {
            throw new NetcdfDecoderException(
                    "Bitset can only currently support 7 fields, this is too many: "
                            + bitset.toString());
        }
        byte[] data = null;
        int bit = 0;
        for (String variable : bitset) {
            Variable dataVariable = netcdfFile.findVariable(variable);

            Object rawField = dataVariable.read().copyTo1DJavaArray();
            if (!(rawField instanceof byte[])) {
                throw new NetcdfDecoderException("Unexpected type["
                        + rawField.getClass() + "] for variable[" + variable
                        + "]");
            }
            byte[] field = (byte[]) rawField;
            int fillValue = 0xFF & GoesrUtils.getFillValue(dataVariable, field)
                    .intValue();

            if (data == null) {
                data = field;
                if (fillValue != GoesrDataDescription.BITSET_FILL) {
                    for (int i = 0; i < field.length; i += 1) {
                        if (fillValue == (0xFF & field[i])) {
                            field[i] = (byte) GoesrDataDescription.BITSET_FILL;
                        }
                    }
                }
            } else {
                for (int i = 0; i < field.length; i += 1) {
                    if (fillValue == (0xFF & field[i])) {
                        /*
                         * Do Nothing, this is an assumption. When thinking
                         * about a bitset, normally fill is not an option for an
                         * input but this data has fill and it is used. Do
                         * nothing means if the data is already filled it will
                         * stay filled, so any index where all fields are filled
                         * will be filled but if any one field is valid then all
                         * filled fields will be 0s
                         */
                        continue;
                    } else if (GoesrDataDescription.BITSET_FILL == (0xFF & field[i])) {
                        data[i] = 0;
                    }
                    if (field[i] != 0) {
                        data[i] = (byte) (data[i] + (1 << bit));
                    }
                }
            }
            bit += 1;
        }
        ByteBuffer dataBuff = ByteBuffer.wrap(data);
        applyMasks(netcdfFile, info, dataDesc, dataBuff);

        IDataRecord storageRecord = DataStoreFactory.createStorageRecord(
                SatelliteRecord.SAT_DATASET_NAME, null, dataBuff.array(), 2,
                sizes);
        storageRecord.setFillValue(GoesrDataDescription.BITSET_FILL);
        storageRecord.setGroup(((SatelliteRecord) info.getBeanMap().getBean())
                .getDataURI());
        info.addField(DataDescription.DATA_KEY, storageRecord, true);
    }

    protected static String getDefaultLocalizationPath() {
        return "satellite/goesr/descriptions/";
    }

    @Override
    protected JAXBManager getJaxbManager() throws JAXBException {
        return new JAXBManager(GoesrNetcdfProductDescriptions.class);
    }

    public void setProjectionFactory(GoesrProjectionFactory projectionFactory) {
        this.projectionFactory = projectionFactory;
    }

    @Override
    protected NetcdfRecordInfo newRecord() {
        return new NetcdfRecordInfo(new SatelliteRecord());
    }

    @Override
    public void setDescriptions(List<NetcdfProductDescriptions> descriptions) {
        for (NetcdfProductDescriptions descs : descriptions) {
            if (!(descs instanceof GoesrNetcdfProductDescriptions)) {
                throw new IllegalArgumentException(
                        GoesrNetcdfProductDescriptions.class.getSimpleName()
                                + " are required by this decoder.");
            }
        }
        super.setDescriptions(descriptions);
    }

    @Override
    protected void validateRecord(PersistablePluginDataObject record)
            throws NetcdfDecoderException {
        super.validateRecord(record);

        SatelliteRecord sRec = (SatelliteRecord) record;

        if (sRec.getMessageData() == null) {
            throw new NetcdfDecoderException("the data is missing.");
        }

        if (sRec.getCoverage() == null) {
            throw new NetcdfDecoderException("coverage is missing.");
        }

        if (sRec.getCreatingEntity() == null) {
            throw new NetcdfDecoderException("creatingEntity is missing.");
        }

        if (sRec.getSectorID() == null) {
            throw new NetcdfDecoderException("sectorID is missing.");
        }

        if (sRec.getSource() == null) {
            throw new NetcdfDecoderException("source is missing.");
        }

        if (sRec.getPhysicalElement() == null) {
            throw new NetcdfDecoderException("physicalElement is missing.");
        }
    }
}
