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
package com.raytheon.uf.edex.plugin.pointset.netcdf;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.LongBuffer;
import java.nio.ShortBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import javax.xml.bind.JAXB;

import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ucar.ma2.DataType;
import ucar.nc2.Attribute;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.pointset.PointSetData;
import com.raytheon.uf.common.dataplugin.pointset.PointSetLocation;
import com.raytheon.uf.common.dataplugin.pointset.PointSetRecord;
import com.raytheon.uf.common.dataplugin.pointset.triangulate.DelauneyTriangulator;
import com.raytheon.uf.common.dataplugin.pointset.triangulate.GridTriangulator;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.edex.netcdf.decoder.util.NetcdfDecoderUtils;
import com.raytheon.uf.edex.netcdf.description.VariableDescription;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.plugin.pointset.netcdf.description.PointSetProductDescriptions;
import com.raytheon.uf.edex.plugin.pointset.netcdf.description.ProductDescription;
import com.raytheon.uf.edex.plugin.pointset.netcdf.description.TriangulationDescription;
import com.raytheon.uf.edex.plugin.pointset.netcdf.description.TriangulationDescription.TriangulationType;

/**
 * 
 * Decoder which can read {@link NetcdfFile}s and extract out one or more
 * {@link PointSetRecord}s. The decode is controlled by
 * {@link PointSetProductDescriptions} which are loaded from an
 * {@link IPathManager}. Currently this decoder can handle any file which stores
 * the longitude,latitude and data in three distinct netcdf variable that have
 * the same shape and which stores the time in a global attribute. The variable
 * names and all metadata can be controlled from the config file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 11, 2015  4709     bsteffen  Initial creation
 * Jan 21, 2016  5208     bsteffen  Decode scale, offset, units, long_name when
 *                                  they are present
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class PointSetNetcdfDecoder {

    private static final Pattern LONGITUDE_COORDINATE_PATTERN = Pattern
            .compile("LON", Pattern.CASE_INSENSITIVE);

    private static final Pattern LATITUDE_COORDINATE_PATTERN = Pattern.compile(
            "LAT", Pattern.CASE_INSENSITIVE);

    private static final PointSetRecord[] EMPTY_POINTSET_ARRAY = new PointSetRecord[0];

    private static final Logger logger = LoggerFactory
            .getLogger(PointSetNetcdfDecoder.class);

    private PointSetProductDescriptions descriptions;

    private LevelFactory levelFactory;

    public PointSetRecord[] decode(File file) {
        if (levelFactory == null) {
            levelFactory = LevelFactory.getInstance();
        }
        try {
            NetcdfFile netcdfFile = NetcdfFile.open(file.getAbsolutePath());
            Map<String, String> locationCache = new HashMap<String, String>();
            List<PointSetRecord> records = new ArrayList<>();
            for (ProductDescription description : descriptions
                    .getDescriptions()) {
                PointSetRecord record = processDescription(netcdfFile,
                        description, locationCache);
                if (record != null) {
                    records.add(record);
                }
            }
            if (records.isEmpty()) {
                logger.warn("No valid pointsets were found in file: {}",
                        file.getName());
                return EMPTY_POINTSET_ARRAY;
            } else {
                return records.toArray(EMPTY_POINTSET_ARRAY);
            }
        } catch (InvalidDescriptionException | IOException | StorageException e) {
            logger.error("Unable to decode pointset from file: {}",
                    file.getName(), e);
        }
        return EMPTY_POINTSET_ARRAY;
    }

    private PointSetRecord processDescription(NetcdfFile file,
            ProductDescription description, Map<String, String> locationCache)
            throws InvalidDescriptionException, IOException, StorageException {
        String dataVarName = description.getDataVariable();
        boolean debug = description.isDebug();
        Variable dataVariable = file.findVariable(NetcdfFile
                .escapeName(dataVarName));
        if (dataVariable == null) {
            if (debug) {
                logger.debug(
                        "Product Description skipped because no data variable was found called {}",
                        dataVarName);
            }
            return null;
        }

        Variable lonVariable = null;
        String lonName = description.getLongitudeVariable();
        if (lonName != null) {
            lonVariable = file.findVariable(NetcdfFile.escapeName(lonName));
            if (lonVariable == null) {
                if (debug) {
                    logger.debug(
                            "Product Description skipped because no longitude variable was found called {}",
                            lonName);
                }
                return null;
            }
        }

        Variable latVariable = null;
        String latName = description.getLatitudeVariable();
        if (latName != null) {
            latVariable = file.findVariable(NetcdfFile.escapeName(latName));
            if (latVariable == null) {
                if (debug) {
                    logger.debug(
                            "Product Description skipped because no latitude variable was found called {}",
                            latName);
                }
                return null;
            }
        }

        if (lonVariable == null || latVariable == null) {
            Attribute coordinates = dataVariable.findAttribute("coordinates");
            if (coordinates == null) {
                if (debug) {
                    logger.debug(
                            "Product Description for data variable, {}, was skipped because the variable does not have coordinates and no longitude/latitude was provided.",
                            dataVarName);
                }
                return null;
            }
            int[] expectedShape = dataVariable.getShape();
            if (coordinates.isArray()) {
                for (int i = 0; i < coordinates.getLength(); i += 1) {
                    String coordinate = coordinates.getStringValue(i);
                    if (lonVariable == null) {
                        lonVariable = checkCoordinate(file, coordinate,
                                LONGITUDE_COORDINATE_PATTERN, expectedShape);
                    }
                    if (latVariable == null) {
                        latVariable = checkCoordinate(file, coordinate,
                                LATITUDE_COORDINATE_PATTERN, expectedShape);

                    }

                }
            } else if (coordinates.isString()) {
                for (String coordinate : coordinates.getStringValue().split(
                        "\\W")) {
                    if (lonVariable == null) {
                        lonVariable = checkCoordinate(file, coordinate,
                                LONGITUDE_COORDINATE_PATTERN, expectedShape);
                    }
                    if (latVariable == null) {
                        latVariable = checkCoordinate(file, coordinate,
                                LATITUDE_COORDINATE_PATTERN, expectedShape);

                    }
                }
            } else {
                if (debug) {
                    logger.debug(
                            "Product Description for data variable, {}, was skipped because the variable coordinates are not a compatible type.",
                            dataVarName);
                }
                return null;
            }
            if (lonVariable == null || latVariable == null) {
                if (debug) {
                    String type = "longitude or latidue";
                    if (lonVariable != null) {
                        type = "latidue";
                    } else if (latVariable != null) {
                        type = "longitude";
                    }
                    logger.debug(
                            "Product Description for data variable, {}, was skipped because no {} was found in the variable coordinates.",
                            dataVarName, type);
                }
                return null;
            }
        }

        PointSetRecord record = description.getRecord(file,
                levelFactory);
        if (record == null) {
            if (debug) {
                logger.debug(
                        "Product Description for data variable, {}, was skipped because the metadata was invalid.",
                        dataVarName);
            }
            return null;
        }
        Buffer numericData = null;
        DataType dataType = dataVariable.getDataType();
        Number fillValue = null;
        Attribute fillAttribute = dataVariable.findAttribute("_FillValue");
        if (fillAttribute != null) {
            fillValue = fillAttribute.getNumericValue();
        }
        switch (dataType) {
        case DOUBLE:
            /*
             * At this point in time(2015) there are no known cases of data
             * being sent as a double and actually needing the extra precision,
             * also not all pieces of CAVE currently support doubles so the
             * precision would not be needed anyway. AMSR-2 derived surface wind
             * speed is being needlessly sent as doubles. Because of all this,
             * just convert doubles to floats.
             */
        case FLOAT:
            float[] fdata = (float[]) dataVariable.read().get1DJavaArray(
                    float.class);
            if (fillValue != null) {
                float ffill = fillValue.floatValue();
                for (int i = 0; i < fdata.length; i += 1) {
                    if (fdata[i] == ffill) {
                        fdata[i] = Float.NaN;
                    }
                }
            }
            numericData = FloatBuffer.wrap(fdata);
            break;
        case BYTE:
            byte[] bdata = (byte[]) dataVariable.read().get1DJavaArray(
                    byte.class);
            numericData = ByteBuffer.wrap(bdata);
            break;
        case SHORT:
            short[] sdata = (short[]) dataVariable.read().get1DJavaArray(
                    short.class);
            numericData = ShortBuffer.wrap(sdata);
            break;
        case INT:
            int[] idata = (int[]) dataVariable.read().get1DJavaArray(int.class);
            numericData = IntBuffer.wrap(idata);
            break;
        case LONG:
            long[] ldata = (long[]) dataVariable.read().get1DJavaArray(
                    long.class);
            numericData = LongBuffer.wrap(ldata);
            break;
        default:
            logger.error(
                    "Unable to generate record for data variable, {}, because the dataType, {}, was not recognized",
                    dataVarName, dataType);
            return null;
        }

        Attribute longNameAttribute = dataVariable.findAttribute("long_name");
        if (longNameAttribute != null) {
            record.getParameter().setName(longNameAttribute.getStringValue());
        }
        Attribute unitsAttribute = dataVariable.findAttribute("units");
        if (unitsAttribute != null) {
            record.getParameter()
                    .setUnitString(unitsAttribute.getStringValue());
        }
        PointSetData data = new PointSetData(numericData);

        Number scale = NetcdfDecoderUtils.getScaleFactor(dataVariable);
        if (scale != NetcdfDecoderUtils.DEFAULT_SCALE_FACTOR) {
            data.setScale(scale.floatValue());
        }
        Number offset = NetcdfDecoderUtils.getAddOffset(dataVariable);
        if (offset != NetcdfDecoderUtils.DEFAULT_ADD_OFFSET) {
            data.setOffset(offset.floatValue());
        }
        record.setData(data);

        StringBuilder locationKeyBuilder = new StringBuilder();
        lonVariable.getNameAndDimensions(locationKeyBuilder);
        locationKeyBuilder.append("\n");
        latVariable.getNameAndDimensions(locationKeyBuilder);
        String locationKey = locationKeyBuilder.toString();
        if (locationCache.containsKey(locationKey)) {
            record.setLocationId(locationCache.get(locationKey));
        } else {
            float[] lonVals = (float[]) lonVariable.read().get1DJavaArray(
                    float.class);
            float[] latVals = (float[]) latVariable.read().get1DJavaArray(
                    float.class);
            PointSetLocation location = new PointSetLocation(lonVals, latVals);
            TriangulationDescription triangulation = description
                    .getTriangulation();
            if (triangulation != null) {
                TriangulationType type = triangulation.getType();
                if (type == TriangulationType.GRID) {
                    int[] shape = lonVariable.getShape();
                    if (shape.length != 2) {
                        logger.warn(
                                "Grid triangulation for {} failed because data is not 2 dimensional",
                                dataVarName);
                    }
                    location.setTriangles(new GridTriangulator().triangulate(
                            shape[1], shape[0]));
                } else if (type == TriangulationType.DELAUNEY) {
                    try {
                        location.setTriangles(new DelauneyTriangulator()
                                .triangulate(location));
                    } catch (FactoryException | TransformException e) {
                        logger.error(
                                "Delauney triangulation for {} failed because data is not 2 dimensional",
                                dataVarName, e);
                    }
                }
            }
            record.setLocationId(location.getId());
            location.save(record.getStoragePath().toFile());
        }
        return record;
    }

    private static Variable checkCoordinate(NetcdfFile file, String coordinate,
            Pattern pattern, int[] expectedShape) {
        if (pattern.matcher(coordinate).find()) {
            Variable variable = file.findVariable(coordinate);
            if (variable != null
                    && !Arrays.equals(expectedShape, variable.getShape())) {
                return null;
            } else {
                return variable;
            }
        }
        return null;
    }

    /**
     * The {@link IPathManager} is used to look up description files.
     */
    public void setPathManager(IPathManager pathManager) {
        LocalizationFile[] files = pathManager.listStaticFiles(
                "pointset/netcdf", new String[] { ".xml" }, true, true);
        PointSetProductDescriptions descriptions = new PointSetProductDescriptions();
        for (LocalizationFile file : files) {
            logger.info("Loading pointset data description from "
                    + file.getPath());
            try (InputStream inputStream = file.openInputStream()) {
                PointSetProductDescriptions unmarshalled = JAXB.unmarshal(
                        inputStream, PointSetProductDescriptions.class);
                for (ProductDescription description : unmarshalled
                        .getDescriptions()) {
                    if (validate(file.getPath(), description)) {
                        descriptions.addDescription(description);
                    }
                }
            } catch (LocalizationException | IOException e) {
                logger.error("Unable to load product descriptions from {}",
                        file.getPath(), e);
            }
        }
        this.descriptions = descriptions;
    }

    public void setDescriptions(PointSetProductDescriptions descriptions) {
        this.descriptions = descriptions;
    }

    public void setLevelFactory(LevelFactory levelFactory) {
        this.levelFactory = levelFactory;
    }

    protected boolean validate(String fileName, ProductDescription description) {
        VariableDescription data = description.getData();
        if (data == null) {
            logger.warn(
                    "Discarding data description from {} because no data element is present.",
                    fileName);
            return false;
        } else if (data.getName() == null) {
            logger.warn(
                    "Discarding data description from {} because data element has no name.",
                    fileName);
            return false;
        }
        return true;
    }
}
