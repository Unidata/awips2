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

import java.io.IOException;
import java.nio.Buffer;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import javax.xml.bind.JAXBException;

import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import ucar.nc2.Attribute;
import ucar.nc2.Dimension;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.dataplugin.pointset.PointSetData;
import com.raytheon.uf.common.dataplugin.pointset.PointSetLocation;
import com.raytheon.uf.common.dataplugin.pointset.PointSetRecord;
import com.raytheon.uf.common.dataplugin.pointset.triangulate.DelauneyTriangulator;
import com.raytheon.uf.common.dataplugin.pointset.triangulate.GridTriangulator;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.edex.netcdf.decoder.AbstractNetcdfDecoder;
import com.raytheon.uf.edex.netcdf.decoder.NetcdfRecordInfo;
import com.raytheon.uf.edex.netcdf.decoder.exception.NetcdfDecoderException;
import com.raytheon.uf.edex.netcdf.decoder.util.NetcdfDecoderUtils;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.direct.VariableDescription;
import com.raytheon.uf.edex.netcdf.description.product.NetcdfProductDescriptions;
import com.raytheon.uf.edex.plugin.pointset.netcdf.description.PointSetNetcdfProductDescription;
import com.raytheon.uf.edex.plugin.pointset.netcdf.description.PointSetNetcdfProductDescriptions;
import com.raytheon.uf.edex.plugin.pointset.netcdf.description.TriangulationDescription;
import com.raytheon.uf.edex.plugin.pointset.netcdf.description.TriangulationDescription.TriangulationType;

/**
 * Decoder which can read {@link NetcdfFile}s and extract out one or more
 * {@link PointSetRecord}s. The decode is controlled by
 * {@link PointSetNetcdfProductDescriptions} which are loaded from an
 * {@link IPathManager}. Currently this decoder can handle any file which stores
 * the longitude,latitude and data in three distinct netcdf variable that have
 * the same shape. The variable names and all metadata can be controlled from
 * the config file.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 11, 2015  4709     bsteffen  Initial creation
 * Jan 21, 2016  5208     bsteffen  Decode scale, offset, units, long_name when
 *                                  they are present and extra validation.
 * Mar 21, 2016  5450     nabowle   Add multi-level/multi-record support.
 *                                  Restructure to split files.
 * Apr 07, 2016  5450     nabowle   Remove decode(File). Add data masking.
 * Apr 19, 2016  5450     nabowle   Add multi-date support. Description patterns.
 * May 18, 2016  5452     bsteffen  Close files, enable configurable alpha shaping
 *                                  of delauney triangulation.
 * May 17, 2016  5584     nabowle   Refactor for consolidation.
 *
 * </pre>
 *
 * @author nabowle
 */
public class PointSetNetcdfDecoder extends AbstractNetcdfDecoder {

    public PointSetNetcdfDecoder(IPathManager pathManager) {
        this(pathManager, null);
    }

    public PointSetNetcdfDecoder(IPathManager pathManager,
            LevelFactory levelFactory) {
        this(getDefaultLocalizationPath(), pathManager, levelFactory);
    }

    public PointSetNetcdfDecoder(String localizationPath,
            IPathManager pathManager, LevelFactory levelFactory) {
        super(localizationPath, pathManager, levelFactory);
    }

    /**
     * Decodes and stores the location and triangulates the location if
     * configured to do so.
     */
    @Override
    protected List<NetcdfRecordInfo> processDeferredDescriptions(
            NetcdfFile netcdfFile, List<NetcdfRecordInfo> currentInfos)
            throws InvalidDescriptionException, NetcdfDecoderException {
        Map<String, PointSetLocation> locationCache = new HashMap<>();
        Map<String, Set<Path>> pathCache = new HashMap<>();

        List<NetcdfRecordInfo> recordInfos = super.processDeferredDescriptions(
                netcdfFile, currentInfos);

        Iterator<NetcdfRecordInfo> iter = recordInfos.iterator();
        NetcdfRecordInfo info;
        Variable dataVariable;
        while (iter.hasNext()) {
            info = iter.next();
            dataVariable = NetcdfDecoderUtils.getDataVariable(info,
                        netcdfFile);
            if (dataVariable == null) {
                logger.warn("Record discarded because the data variable could not be found.");
                iter.remove();
                continue;
            }
            String dataVarName = dataVariable.getFullNameEscaped();

            Variable latVariable = null;
            Variable lonVariable = null;

            VariableDescription latVariableDesc = (VariableDescription) info
                    .getDeferredDescription(PointSetNetcdfProductDescription.LATITUDE_KEY);
            if (latVariableDesc != null) {
                latVariable = netcdfFile
                        .findVariable(latVariableDesc.getName());
            }

            VariableDescription lonVariableDesc = (VariableDescription) info
                    .getDeferredDescription(PointSetNetcdfProductDescription.LONGITUDE_KEY);
            if (lonVariableDesc != null) {
                lonVariable = netcdfFile
                        .findVariable(lonVariableDesc.getName());
            }

            if (lonVariable == null || latVariable == null) {
                Attribute coordinates = dataVariable
                        .findAttribute("coordinates");
                if (coordinates == null) {
                    logger.warn(
                            "Record discarded for data variable, {}, because the variable does not have coordinates and no longitude/latitude was provided.",
                            dataVarName);
                    iter.remove();
                    continue;
                }

                int[] expectedShape;
                List<Dimension> dims = dataVariable.getDimensions();
                expectedShape = new int[dims.size()];
                Dimension dim;
                for (int i = 0; i < dims.size(); i++) {
                    dim = dims.get(i);
                    if (info.getDimensionIndexMap().containsKey(dim.getName())) {
                        expectedShape[i] = 1;
                    } else {
                        expectedShape[i] = dim.getLength();
                    }
                }
                expectedShape = NetcdfDecoderUtils.trimShape(expectedShape);

                if (coordinates.isArray()) {
                    for (int i = 0; i < coordinates.getLength(); i += 1) {
                        String coordinate = coordinates.getStringValue(i);
                        if (lonVariable == null) {
                            lonVariable = checkCoordinate(netcdfFile,
                                    coordinate,
                                    NetcdfDecoderUtils.LONGITUDE_COORDINATE_PATTERN,
                                    expectedShape);
                        }
                        if (latVariable == null) {
                            latVariable = checkCoordinate(netcdfFile,
                                    coordinate,
                                    NetcdfDecoderUtils.LATITUDE_COORDINATE_PATTERN,
                                    expectedShape);

                        }

                    }
                } else if (coordinates.isString()) {
                    for (String coordinate : coordinates.getStringValue()
                            .split("\\W")) {
                        if (lonVariable == null) {
                            lonVariable = checkCoordinate(netcdfFile,
                                    coordinate,
                                    NetcdfDecoderUtils.LONGITUDE_COORDINATE_PATTERN,
                                    expectedShape);
                        }
                        if (latVariable == null) {
                            latVariable = checkCoordinate(netcdfFile,
                                    coordinate,
                                    NetcdfDecoderUtils.LATITUDE_COORDINATE_PATTERN,
                                    expectedShape);

                        }
                    }
                } else {
                    logger.warn(
                            "Record discarded for data variable, {}, because the variable coordinates are not a compatible type.",
                            dataVarName);
                    iter.remove();
                    continue;
                }
                if (lonVariable == null || latVariable == null) {
                    String type = "longitude or latitude";
                    if (lonVariable != null) {
                        type = "latitude";
                    } else if (latVariable != null) {
                        type = "longitude";
                    }
                    logger.warn(
                            "Record discarded for data variable, {}, because no {} was found in the variable coordinates.",
                            dataVarName, type);
                    iter.remove();
                    continue;
                }
            }

            StringBuilder locationKeyBuilder = new StringBuilder();
            lonVariable.getNameAndDimensions(locationKeyBuilder);
            locationKeyBuilder.append("\n");
            latVariable.getNameAndDimensions(locationKeyBuilder);
            String locationKey = locationKeyBuilder.toString();

            PointSetLocation location = locationCache.get(locationKey);
            if (location == null) {
                float[] lonVals;
                try {
                    lonVals = (float[]) lonVariable.read().get1DJavaArray(
                            float.class);
                } catch (IOException e) {
                    throw new NetcdfDecoderException(
                            "Cannot read the longitude variable.", e);
                }
                float[] latVals;
                try {
                    latVals = (float[]) latVariable.read().get1DJavaArray(
                            float.class);
                } catch (IOException e) {
                    throw new NetcdfDecoderException(
                            "Cannot read the lat variable.", e);
                }

                location = new PointSetLocation(lonVals, latVals);

                TriangulationDescription triangulation = (TriangulationDescription) info
                        .getDeferredDescription(TriangulationDescription.TRIANGULATION_KEY);
                if (triangulation != null) {
                    TriangulationType type = triangulation.getType();
                    if (type != null) {
                        if (type == TriangulationType.GRID) {
                            int[] shape = lonVariable.getShape();
                            if (shape.length != 2) {
                                logger.warn(
                                        "Grid triangulation for {} failed because data is not 2 dimensional",
                                        dataVarName);
                            }
                            location.setTriangles(new GridTriangulator()
                                    .triangulate(shape[1], shape[0]));
                        } else if (type == TriangulationType.DELAUNEY) {
                            Double maxRadius = triangulation.getMaxRadius();
                            DelauneyTriangulator triangulator;
                            if (maxRadius == null) {
                                triangulator = new DelauneyTriangulator();
                            } else {
                                triangulator = new DelauneyTriangulator(maxRadius);
                            }
                            try {
                                location.setTriangles(triangulator
                                        .triangulate(location));
                            } catch (FactoryException | TransformException e) {
                                logger.error(
                                        "Delauney triangulation for {} failed",
                                        dataVarName, e);
                            }
                        }
                    }
                }
                locationCache.put(locationKey, location);
            }

            Set<Path> storedPaths = pathCache.get(locationKey);
            if (storedPaths == null) {
                storedPaths = new HashSet<>();
                pathCache.put(locationKey, storedPaths);
            }

            PointSetRecord record = (PointSetRecord) info.getBeanMap()
                    .getBean();

            Path path = record.getStoragePath();
            if (!storedPaths.contains(path)) {
                try {
                    location.save(path.toFile());
                } catch (StorageException e) {
                    iter.remove();
                    continue;
                }
                storedPaths.add(path);
            }
            record.setLocationId(location.getId());
        }

        return recordInfos;
    }

    private static Variable checkCoordinate(NetcdfFile file, String coordinate,
            Pattern pattern, int[] expectedShape) {
        if (pattern.matcher(coordinate).find()) {
            Variable variable = file.findVariable(coordinate);
            if (variable != null
                    && !Arrays.equals(expectedShape,
                            NetcdfDecoderUtils.trimShape(variable.getShape()))) {
                return null;
            } else {
                return variable;
            }
        }
        return null;
    }

    @Override
    protected void addDataToInfo(NetcdfRecordInfo info, Buffer numericData) {
        info.addField("data", new PointSetData(numericData), true);
    }

    protected static String getDefaultLocalizationPath() {
        return "pointset/netcdf";
    }

    @Override
    protected JAXBManager getJaxbManager() throws JAXBException {
        return new JAXBManager(PointSetNetcdfProductDescriptions.class);
    }

    @Override
    protected NetcdfRecordInfo newRecord() {
        return new NetcdfRecordInfo(new PointSetRecord());
    }

    @Override
    public void setDescriptions(List<NetcdfProductDescriptions> descriptions) {
        for (NetcdfProductDescriptions descs : descriptions) {
            if (!(descs instanceof PointSetNetcdfProductDescriptions)) {
                throw new IllegalArgumentException(
                        PointSetNetcdfProductDescriptions.class.getSimpleName()
                                + " are required by this decoder.");
            }
        }
        super.setDescriptions(descriptions);
    }

    @Override
    protected void validateRecord(PersistablePluginDataObject record)
            throws NetcdfDecoderException {
        super.validateRecord(record);

        PointSetRecord pRec = (PointSetRecord) record;

        if (pRec.getData() == null) {
            throw new NetcdfDecoderException("the data is missing.");
        }

        if (pRec.getDatasetId() == null) {
            throw new NetcdfDecoderException("datasetId is missing.");
        }

        if (pRec.getLevel() == null) {
            throw new NetcdfDecoderException("level is missing.");
        }

        if (pRec.getParameter() == null) {
            throw new NetcdfDecoderException("parameter is missing.");
        }
    }

}
