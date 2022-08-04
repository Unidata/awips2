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

package com.raytheon.edex.plugin.grib.decoderpostprocessors;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.edex.plugin.grib.spatial.GribSpatialCache;
import com.raytheon.edex.util.Util;
import com.raytheon.edex.util.grib.CompositeModel;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.plugin.grid.PartialGrid;

/**
 * The EnsembleGridAssembler class is part of the ingest process for grib data.
 * Some grib model come in as octants. This class will combine those octants
 * into a single grid
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 09, 2010  4638     bphillip  Initial Creation
 * Mar 14, 2013  1794     djohnson  FileUtil.listFiles now returns List.
 * Mar 27, 2013  1821     bsteffen  Reduce db and pypies requests in grid
 *                                  assembler.
 * Aug 30, 2013  2298     rjpeter   Make getPluginName abstract
 * Oct 15, 2013  2473     bsteffen  Remove deprecated method calls.
 * Nov 19, 2013  2478     rjpeter   Make update process update database also.
 * Dec 06, 2013  2170     rjpeter   Update to pass PluginDataObject[] to
 *                                  notification.
 * Apr 21, 2014  2060     njensen   Remove dependency on grid dataURI column
 * Jul 21, 2014  3373     bclement  JAXB manager api changes
 * Aug 18, 2014  4360     rferrel   Set secondaryId in {@link
 *                                  #createAssembledRecord(GridRecord,
 *                                  CompositeModel)}
 * Sep 09, 2015  4868     rjpeter   Updated to be stored in partial grids as
 *                                  part of normal route.
 * Oct 07, 2015  3756     nabowle   Extends DecoderPostProcessor.
 * Apr 11, 2016  5564     bsteffen  Move localization files to common_static
 * 
 * </pre>
 * 
 * @author bphillip
 */
public class EnsembleGridAssembler extends DecoderPostProcessor {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(EnsembleGridAssembler.class);

    /** The map of the models that come in sections */
    private static final Map<String, CompositeModel> thinnedModels = new HashMap<>();

    static {
        loadThinnedModels();
    }

    /**
     * Loads the models from the localization store and stores them in memory
     */
    private static void loadThinnedModels() {
        IPathManager pm = PathManagerFactory.getPathManager();
        ILocalizationFile[] thinnedModelFiles = pm.listStaticFiles(
                pm.getLocalSearchHierarchy(LocalizationType.COMMON_STATIC),
                "/grib/thinnedModels", new String[] { ".xml" }, true, true);

        SingleTypeJAXBManager<CompositeModel> jaxbManager;
        try {
            jaxbManager = new SingleTypeJAXBManager<>(true,
                    CompositeModel.class);
        } catch (JAXBException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to load thinned model files.", e);
            return;
        }

        for (ILocalizationFile file : thinnedModelFiles) {
            try (InputStream is = file.openInputStream()) {
                CompositeModel model = jaxbManager.unmarshalFromInputStream(is);
                thinnedModels.put(model.getModelName(), model);
            } catch (SerializationException | IOException
                    | LocalizationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error deserializing thinned model file", e);
            }
        }
    }

    @Override
    public GridRecord[] process(GridRecord rec) throws GribException {
        CompositeModel compositeModel = getCompositeModel(rec.getDatasetId());

        if (compositeModel != null) {
            GridRecord assembledRecord = createAssembledRecord(rec,
                    compositeModel);
            GridRecord wrapRecord = setPartialGrid(compositeModel,
                    assembledRecord, rec);
            if (wrapRecord == null) {
                return new GridRecord[] { assembledRecord };
            }

            return new GridRecord[] { assembledRecord, wrapRecord };
        }

        // wasn't a grid to be assembled
        return new GridRecord[] { rec };
    }

    /**
     * Gets the composite model for the provided model name0
     *
     * @param modelName
     *            The model name to determine the composite model name for
     * @return The composite model. Null if not found
     */
    private CompositeModel getCompositeModel(String modelName) {
        for (CompositeModel mod : thinnedModels.values()) {
            if (mod.getModelList().contains(modelName)) {
                return mod;
            }
        }
        return null;
    }

    private GridRecord createAssembledRecord(GridRecord record,
            CompositeModel thinned) {
        GridRecord newRecord = new GridRecord(record);

        GridCoverage coverage = GribSpatialCache.getInstance().getGridByName(
                thinned.getGrid());

        newRecord.setLocation(coverage);
        newRecord.setDatasetId(thinned.getModelName());
        newRecord.setOverwriteAllowed(true);

        return newRecord;
    }

    /**
     * Merges the data from a GridRecord into the composite GridRecord
     *
     * @param record
     *            The GridRecord containing the data to add
     * @param assembledRecord
     *            The composite GridRecord
     * @param thinned
     *            The composite model definition
     * @throws GribException
     */
    private GridRecord setPartialGrid(CompositeModel thinned,
            GridRecord assembledRecord, GridRecord recordToAssemble)
            throws GribException {
        PartialGrid pGrid = new PartialGrid();

        String modelName = recordToAssemble.getDatasetId();
        GridCoverage coverage = recordToAssemble.getLocation();

        int nx = coverage.getNx();
        int ny = coverage.getNy();
        pGrid.setNx(nx);
        pGrid.setNy(ny);

        /*
         * TODO: This should map the UL corner of recordToAssemble to
         * assembledRecord instead of relying on index in list
         */
        List<String> compModels = thinned.getModelList();
        int modIndex = compModels.indexOf(modelName);
        if (modIndex == -1) {
            /*
             * Shouldn't be possible since was how it was found in the first
             * place
             */
            throw new GribException(
                    "Error assembling grids.  Thinned grid definition does not contain "
                            + modelName);
        }

        pGrid.setxOffset((nx * modIndex) - modIndex);
        pGrid.setyOffset(0);
        assembledRecord.addExtraAttribute(PartialGrid.KEY, pGrid);
        assembledRecord.setMessageData(getMessageData(recordToAssemble));

        return checkWorldWrap(assembledRecord);
    }

    /**
     * Checks if assembledRecord's partial grid has extra columns that wrap
     * around. This is due to partial grids having 1 column of overlap between
     * each partial grid.
     *
     * @param assembledRecord
     * @return
     */
    private GridRecord checkWorldWrap(GridRecord assembledRecord) {
        GridCoverage assembledCoverage = assembledRecord.getLocation();
        int assembledNx = assembledCoverage.getNx();
        PartialGrid pGrid = (PartialGrid) assembledRecord
                .getExtraAttribute(PartialGrid.KEY);
        int xOffset = pGrid.getxOffset();
        int nx = pGrid.getNx();
        int ny = pGrid.getNy();

        // check world wrap due to overlapping columns
        if ((xOffset + nx) > assembledNx) {
            float[] messageData = (float[]) assembledRecord.getMessageData();
            float[][] data2D = Util.resizeDataTo2D(messageData, nx, ny);

            // cut off extra data from assembledRecord
            int newNx = assembledNx - xOffset;
            pGrid.setNx(newNx);
            assembledRecord.setMessageData(trimGridAndMake1D(data2D, 0, 0,
                    newNx, ny));

            // make a secondary record for the wrap amount
            GridRecord wrappedRecord = new GridRecord(assembledRecord);
            PartialGrid wrappedPartial = new PartialGrid();
            wrappedPartial.setxOffset(0);
            wrappedPartial.setyOffset(0);
            wrappedPartial.setNx(nx - newNx);
            wrappedPartial.setNy(ny);
            wrappedRecord.addExtraAttribute(PartialGrid.KEY, wrappedPartial);
            wrappedRecord.setMessageData(trimGridAndMake1D(data2D, newNx, 0,
                    wrappedPartial.getNx(), ny));
            wrappedRecord.setOverwriteAllowed(true);
            return wrappedRecord;
        }

        return null;
    }

    private float[] trimGridAndMake1D(float[][] data, int xOffset, int yOffset,
            int nx, int ny) {
        float[][] rval = new float[ny][nx];

        for (int row = 0; row < ny; row++) {
            for (int col = 0; col < nx; col++) {
                rval[row][col] = data[yOffset + row][xOffset + col];
            }
        }

        return Util.resizeDataTo1D(rval, ny, nx);
    }
}
