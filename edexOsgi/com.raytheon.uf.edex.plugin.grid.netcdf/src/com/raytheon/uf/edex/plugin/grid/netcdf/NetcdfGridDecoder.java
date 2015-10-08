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
package com.raytheon.uf.edex.plugin.grid.netcdf;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.JAXB;
import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ucar.ma2.Array;
import ucar.ma2.InvalidRangeException;
import ucar.ma2.Range;
import ucar.ma2.Section;
import ucar.nc2.Dimension;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.netcdf.decoder.exception.NetcdfDecoderException;
import com.raytheon.uf.edex.netcdf.decoder.util.NetcdfDecoderUtils;
import com.raytheon.uf.edex.netcdf.description.AbstractFieldDescription;
import com.raytheon.uf.edex.netcdf.description.LevelDescription;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.plugin.grid.netcdf.description.coverage.CoverageCoordinatesDescription;
import com.raytheon.uf.edex.plugin.grid.netcdf.description.product.NetcdfGridProductDescription;
import com.raytheon.uf.edex.plugin.grid.netcdf.description.product.NetcdfGridProductDescriptions;

/**
 * Base class for decoding NetCDF Grid files.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 03, 2015 4696       nabowle     Initial creation
 * Sep 10, 2015 4696       nabowle     Refactored. Renamed. No longer abstract.
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */
public class NetcdfGridDecoder {

    protected static final Logger logger = LoggerFactory
            .getLogger(NetcdfGridDecoder.class);

    protected static final GridRecord[] EMPTY_RECORDS = new GridRecord[0];

    private List<NetcdfGridProductDescriptions> descriptionsList = new ArrayList<>();

    /**
     * Constructor.
     */
    public NetcdfGridDecoder() {
        super();
    }

    /**
     * Decodes the provided file. Note: This will like be memory intensive. If
     * the files are expected to create many records, it's better to split the
     * file, and decode and store records individually.
     *
     * This method will decode all the records together and return them for
     * batch storage.
     *
     * @param f
     *            The data file.
     * @return The decoded grid records, or an empty array if no records could
     *         be decoded.
     */
    public GridRecord[] decode(File f) {

        Iterator<NetcdfGridRecordInfo> iter = split(f);

        List<GridRecord> records = new ArrayList<>();

        GridRecord[] recs;
        while (iter.hasNext()) {
            recs = decode(iter.next());

            for (GridRecord record : recs) {
                if (record != null) {
                    records.add(record);
                }
            }
        }

        return records.toArray(EMPTY_RECORDS);
    }

    public GridRecord[] decode(NetcdfGridRecordInfo recordInfo) {
        NetcdfGridProductDescription product = recordInfo.getDescription();
        File f = recordInfo.getFile();
        int levelIndex = recordInfo.getLevelIdx();
        NetcdfFile file = null;

        NetcdfGridProductDescriptions descriptions = null;
        for (NetcdfGridProductDescriptions descriptionFile : this.descriptionsList) {
            if (descriptionFile.matches(f.getName())) {
                descriptions = descriptionFile;
                break;
            }
        }

        if (descriptions == null) {
            logger.warn("No matching descriptions for file: {}", f.getName());
            return EMPTY_RECORDS;
        }

        try {
            file = NetcdfFile.open(f.getAbsolutePath());

            Variable dataVar = file.findVariable(product.getData().getName());
            if (dataVar == null) {
                logger.error("No {} {} data in file: {}", descriptions
                        .getDatasetId(), product.getData().getName(),
                        f.getName());
                return EMPTY_RECORDS;
            }

            Level level = descriptions.getLevel().getLevel(file,
                    LevelFactory.getInstance(), recordInfo.getLevelIdx());
            if (level == null) {
                logger.error("Cannot decode level data in file: {}",
                        f.getName());
                return EMPTY_RECORDS;
            }

            GridCoverage location = recordInfo.getLocation();

            GridRecord record = new GridRecord();
            record.setDatasetId(descriptions.getDatasetId());
            record.setLevel(level);
            record.setDataTime(recordInfo.getDataTime());
            record.setLocation(location);

            Parameter param = new Parameter(product.getParameter(),
                    product.getParameterName(), product.getUnits());
            record.setParameter(param);
            List<Dimension> dimensions = dataVar.getDimensions();
            List<Range> ranges = dataVar.getRanges();
            final String levelOneName = descriptions.getLevel()
                    .getLevelOneValue().getName();

            /*
             * Add the Ranges to the Section. The Range for the level variable,
             * if present, is limited to just the index for the current record,
             * while all others are left at their full Range.
             */
            Section sect = new Section();
            int latDimIdx = -1;
            int lonDimIdx = -1;
            Dimension dim;
            for (int i = 0; i < dimensions.size(); i++) {
                dim = dimensions.get(i);
                if (dim.getName().equals(levelOneName)) {
                    // start and end range values are inclusive.
                    sect.appendRange(new Range(levelIndex, levelIndex));
                } else {
                    if (recordInfo.getLatField().equals(dim.getName())) {
                        latDimIdx = i;
                    } else if (recordInfo.getLonField().equals(dim.getName())) {
                        lonDimIdx = i;
                    }
                    sect.appendRange(ranges.get(i));
                }
            }

            Array dataArr = dataVar.read(sect);

            float scaleFactor = NetcdfDecoderUtils.getScaleFactor(dataVar)
                    .floatValue();
            float addOffset = NetcdfDecoderUtils.getAddOffset(dataVar)
                    .floatValue();
            float noDataVal = NetcdfDecoderUtils.getNoDataValue(dataVar,
                    Float.NaN).floatValue();

            switch (location.getFirstGridPointCorner()) {
            case LowerLeft:
                dataArr = dataArr.flip(latDimIdx);
                break;
            case UpperRight:
                dataArr = dataArr.flip(lonDimIdx);
                break;
            case LowerRight:
                dataArr = dataArr.flip(latDimIdx).flip(lonDimIdx);
                break;
            case UpperLeft:
            default:
                // No-op
            }

            boolean dataPresent = false;
            float[] data = (float[]) dataArr.get1DJavaArray(float.class);
            float val;
            for (int i = 0; i < data.length; i++) {
                val = data[i];
                if (val == noDataVal) {
                    val = Float.NaN;
                } else {
                    val = val * scaleFactor + addOffset;
                }
                data[i] = val;
                dataPresent |= !Float.isNaN(val);
            }
            record.setMessageData(data);

            if (dataPresent) {
                return new GridRecord[] { record };
            } else {
                return EMPTY_RECORDS;
            }
        } catch (IOException | InvalidRangeException
                | InvalidDescriptionException e) {
            logger.error("Unable to decode {} {} from file: {}",
                    descriptions.getDatasetId(), product.getData().getName(),
                    f.getName(), e);
        } finally {
            if (file != null) {
                try {
                    file.close();
                } catch (IOException e) {
                    logger.warn("Error closing file: {}", f.getName());
                }
            }
        }

        return EMPTY_RECORDS;
    }

    /**
     * Does some preliminary decoding of common values and combines those with
     * the level index and a product description to specify individual records.
     *
     * @param f
     *            The file to split.
     * @return An iterator over the record descriptions.
     */
    public Iterator<NetcdfGridRecordInfo> split(File f) {
        List<NetcdfGridRecordInfo> infos = new ArrayList<>();
        NetcdfFile file = null;
        try {
            file = NetcdfFile.open(f.getAbsolutePath());

            NetcdfGridProductDescriptions descriptions = null;
            for (NetcdfGridProductDescriptions descriptionFile : this.descriptionsList) {
                if (descriptionFile.matches(f.getName())) {
                    descriptions = descriptionFile;
                    break;
                }
            }

            if (descriptions == null) {
                throw new NetcdfDecoderException("No matching descriptions.");
            }

            long numLevels = getNumLevels(file, descriptions);
            if (numLevels == 0) {
                throw new NetcdfDecoderException(
                        "Cannot determine the number of levels.");
            }

            DataTime dataTime = descriptions.getDataTime().getDataTime(file);
            if (dataTime == null) {
                throw new NetcdfDecoderException(
                        "Cannot decode the data time.");
            }

            GridCoverage location = getCoverage(file, descriptions);

            String latField = null;
            String lonField = null;
            CoverageCoordinatesDescription coordDesc = descriptions
                    .getCoverage().getCoordinatesDescription();
            if (coordDesc != null) {
                AbstractFieldDescription coordField = coordDesc.getLatitude();

                if (coordField != null) {
                    latField = coordField.getName();
                }
                coordField = coordDesc.getLongitude();

                if (coordField != null) {
                    lonField = coordField.getName();
                }
            }

            NetcdfGridRecordInfo info;
            for (NetcdfGridProductDescription prod : descriptions.getDescriptions()) {
                for (int i = 0; i < numLevels; i++) {
                    info = new NetcdfGridRecordInfo();
                    info.setDataTime(dataTime);
                    info.setLevelIdx(i);
                    info.setFile(f);
                    info.setDescription(prod);
                    info.setLocation(location);
                    if (latField != null) {
                        info.setLatField(latField);
                    }
                    if (lonField != null) {
                        info.setLonField(lonField);
                    }
                    infos.add(info);

                }
            }
        } catch (IOException | InvalidDescriptionException
                | NetcdfDecoderException e) {
            logger.error("Unable split file: {}", f.getName(), e);
        } finally {
            if (file != null) {
                try {
                    file.close();
                } catch (IOException e) {
                    logger.warn("Error closing file: {}", f.getName());
                }
            }
        }

        return infos.iterator();
    }

    /**
     * @param file
     * @return
     * @throws NetcdfDecoderException
     */
    protected long getNumLevels(NetcdfFile file,
            NetcdfGridProductDescriptions descriptions)
            throws NetcdfDecoderException {
        LevelDescription levelDesc = descriptions.getLevel();

        long numLevels = 0;
        AbstractFieldDescription afd = levelDesc.getMasterLevel();
        if (afd != null) {
            numLevels = afd.getLength(file);
        }

        afd = levelDesc.getLevelOneValue();
        if (afd != null) {
            numLevels = Math.max(numLevels, afd.getLength(file));
        }

        afd = levelDesc.getLevelTwoValue();
        if (afd != null) {
            numLevels = Math.max(numLevels, afd.getLength(file));
        }
        return numLevels;
    }

    /**
     * @param file
     * @return
     * @throws InvalidDescriptionException
     */
    protected GridCoverage getCoverage(NetcdfFile file,
            NetcdfGridProductDescriptions descriptions)
            throws InvalidDescriptionException, NetcdfDecoderException {
        return descriptions.getCoverage().getInitializedCoverage(file);
    }

    /**
     * The {@link IPathManager} is used to look up the configured description
     * file.
     *
     * @throws JAXBException
     * @throws SerializationException
     */
    public void setPathManager(IPathManager pathManager) {
        LocalizationFile[] files = pathManager.listStaticFiles("grid/netcdf",
                new String[] { ".xml" }, true, true);

        NetcdfGridProductDescriptions descriptions;
        for (LocalizationFile file : files) {
            logger.info("Loading Netcdf Grid description from "
                    + file.getName());
            try (InputStream inputStream = file.openInputStream()) {
                descriptions = JAXB.unmarshal(inputStream,
                        NetcdfGridProductDescriptions.class);
                this.descriptionsList.add(descriptions);
            } catch (LocalizationException | IOException e) {
                logger.error("Unable to load product descriptions from {}",
                        file.getName(), e);
            }
        }

        if (this.descriptionsList.isEmpty()) {
            logger.error("No descriptions were loaded. Nothing will be decoded.");
        }
    }

    public List<NetcdfGridProductDescriptions> getDescriptionsList() {
        return this.descriptionsList;
    }
}
