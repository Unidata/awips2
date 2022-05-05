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

import java.nio.Buffer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import javax.xml.bind.JAXBException;

import ucar.nc2.Dimension;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.lookup.GridCoverageLookup;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.util.BufferUtil;
import com.raytheon.uf.edex.netcdf.decoder.AbstractNetcdfDecoder;
import com.raytheon.uf.edex.netcdf.decoder.NetcdfRecordInfo;
import com.raytheon.uf.edex.netcdf.decoder.exception.NetcdfDecoderException;
import com.raytheon.uf.edex.netcdf.decoder.util.NetcdfDecoderUtils;
import com.raytheon.uf.edex.netcdf.description.data.DataDescription;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.product.NetcdfProductDescriptions;
import com.raytheon.uf.edex.plugin.grid.netcdf.description.coverage.GridCoverageDescription;
import com.raytheon.uf.edex.plugin.grid.netcdf.description.product.GridNetcdfProductDescriptions;

/**
 * Decoder NetCDF Grid files.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 03, 2015 4696       nabowle     Initial creation
 * Sep 10, 2015 4696       nabowle     Refactored. Renamed. No longer abstract.
 * Jan 27, 2016 5237       tgurney     Replace LocalizationFile with
 *                                     ILocalizationFile
 * Mar 21, 2016 5450       nabowle     Extracted getNumLevels to LevelDescription.
 * Apr 06, 2016 5450       nabowle     Remove decode(File)
 * Apr 07, 2016 5446       skorolev    Fixed flip error. Added valid_range testing.
 * May 23, 2016 5584       nabowle     Refactor and rename for consolidation.
 *
 * </pre>
 *
 * @author nabowle
 */

public class GridNetcdfDecoder extends AbstractNetcdfDecoder {
    protected static final Set<String> GRID_EXCLUDED_FIELDS;

    static {
        Set<String> fields = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);
        fields.addAll(AbstractNetcdfDecoder.DEFAULT_EXCLUDED_FIELDS);
        fields.add("info");
        GRID_EXCLUDED_FIELDS = Collections.unmodifiableSet(fields);
    }

    public GridNetcdfDecoder(IPathManager pathManager) {
        this(pathManager, null);
    }

    public GridNetcdfDecoder(IPathManager pathManager, LevelFactory levelFactory) {
        this(getDefaultLocalizationPath(), pathManager, levelFactory);
    }

    public GridNetcdfDecoder(String localizationPath, IPathManager pathManager,
            LevelFactory levelFactory) {
        super(localizationPath, pathManager, levelFactory);
    }

    @Override
    protected List<NetcdfRecordInfo> processDeferredDescriptions(NetcdfFile netcdfFile,
            List<NetcdfRecordInfo> currentInfos)
            throws InvalidDescriptionException, NetcdfDecoderException {

        Map<String, GridCoverage> coverageCache = new HashMap<>();
        List<NetcdfRecordInfo> recordInfos = new ArrayList<>(currentInfos);
        Iterator<NetcdfRecordInfo> iter = recordInfos.iterator();
        GridCoverageDescription coverageDesc;
        GridCoverage location;
        NetcdfRecordInfo info;
        Dimension latDim;
        Dimension lonDim;
        Variable dataVariable;
        while (iter.hasNext()) {
            info = iter.next();
            coverageDesc = (GridCoverageDescription) info
                    .getDeferredDescription(GridCoverageDescription.COVERAGE_KEY);
            if (coverageDesc == null) {
                logger.warn("Record discarded because no matching grid coverage description was found.");
                iter.remove();
                continue;
            }

            location = coverageCache.get(coverageDesc.getId());
            if (location == null) {
                location = coverageDesc.getInitializedCoverage(netcdfFile);
                if (location == null) {
                    logger.warn("Record discarded because the grid coverage could not be decoded.");
                    iter.remove();
                    continue;
                }

                location = GridCoverageLookup.getInstance().getCoverage(
                        location, true);
                coverageCache.put(coverageDesc.getId(), location);
            }

            info.addField(GridCoverageDescription.COVERAGE_KEY, location, true);

            dataVariable = NetcdfDecoderUtils.getDataVariable(info,
                        netcdfFile);
            if (dataVariable == null) {
                logger.warn("Record discarded because the data variable could not be found.");
                iter.remove();
                continue;
            }

            latDim = null;
            lonDim = null;
            switch (location.getFirstGridPointCorner()) {
            case LowerLeft:
                latDim = NetcdfDecoderUtils.findDimension(dataVariable,
                        coverageDesc.getLatitudeNames(),
                        NetcdfDecoderUtils.LATITUDE_COORDINATE_PATTERN);
                if (latDim == null) {
                    logger.warn("Record discarded because the latitude dimension could not be found.");
                    iter.remove();
                    continue;
                }
                break;
            case UpperRight:
                lonDim = NetcdfDecoderUtils.findDimension(dataVariable,
                        coverageDesc.getLongitudeNames(),
                        NetcdfDecoderUtils.LONGITUDE_COORDINATE_PATTERN);
                if (lonDim == null) {
                    logger.warn("Record discarded because the longitude dimension could not be found.");
                    iter.remove();
                    continue;
                }
                break;
            case LowerRight:
                latDim = NetcdfDecoderUtils.findDimension(dataVariable,
                        coverageDesc.getLatitudeNames(),
                        NetcdfDecoderUtils.LATITUDE_COORDINATE_PATTERN);
                lonDim = NetcdfDecoderUtils.findDimension(dataVariable,
                        coverageDesc.getLongitudeNames(),
                        NetcdfDecoderUtils.LONGITUDE_COORDINATE_PATTERN);
                if (latDim == null) {
                    logger.warn("Record discarded because the latitude dimension could not be found.");
                    iter.remove();
                    continue;
                }
                if (lonDim == null) {
                    logger.warn("Record discarded because the longitude dimension could not be found.");
                    iter.remove();
                    continue;
                }
                break;
            case UpperLeft:
            default:
                // No-op
            }
            if (latDim != null) {
                info.addFlipDimensionIndex(dataVariable.getDimensions()
                        .indexOf(latDim));
            }
            if (lonDim != null) {
                info.addFlipDimensionIndex(dataVariable.getDimensions()
                        .indexOf(lonDim));
            }
        }

        return super.processDeferredDescriptions(netcdfFile, recordInfos);
    }

    protected static String getDefaultLocalizationPath() {
        return "grid/netcdf";
    }

    /**
     * @param info
     * @param numericData
     */
    @Override
    protected void addDataToInfo(NetcdfRecordInfo info, Buffer numericData) {
        info.addField(DataDescription.DATA_KEY,
                BufferUtil.toFloatBuffer(numericData).array(), true);
    }

    @Override
    protected JAXBManager getJaxbManager() throws JAXBException {
        return new JAXBManager(GridNetcdfProductDescriptions.class);
    }

    @Override
    protected NetcdfRecordInfo newRecord() {
        return new NetcdfRecordInfo(new GridRecord());
    }

    @Override
    public void setDescriptions(List<NetcdfProductDescriptions> descriptions) {
        for (NetcdfProductDescriptions descs : descriptions) {
            if (!(descs instanceof GridNetcdfProductDescriptions)) {
                throw new IllegalArgumentException(
                        GridNetcdfProductDescriptions.class.getSimpleName()
                                + " are required by this decoder.");
            }
        }
        super.setDescriptions(descriptions);
    }

    @Override
    protected void validateRecord(PersistablePluginDataObject record)
            throws NetcdfDecoderException {
        super.validateRecord(record);

        GridRecord gRec = (GridRecord) record;

        if (gRec.getMessageData() == null) {
            throw new NetcdfDecoderException("the data is missing.");
        }

        if (gRec.getLocation() == null) {
            throw new NetcdfDecoderException("location is missing.");
        }

        if (gRec.getDatasetId() == null) {
            throw new NetcdfDecoderException("datasetId is missing.");
        }

        if (gRec.getLevel() == null) {
            throw new NetcdfDecoderException("level is missing.");
        }

        if (gRec.getParameter() == null) {
            throw new NetcdfDecoderException("parameter is missing.");
        }
    }

    @Override
    protected Set<String> getExcludedFields() {
        return GRID_EXCLUDED_FIELDS;
    }
}
