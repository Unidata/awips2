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
package com.raytheon.uf.edex.datadelivery.retrieval.metadata.adapters;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.datadelivery.registry.GriddedCoverage;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.edex.datadelivery.retrieval.util.ResponseProcessingUtilities;

/**
 * 
 * Convert RetrievalAttribute to GridRecords.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 19, 2012            bsteffen     Initial javadoc
 * 
 * </pre>
 * 
 * @author unknown
 * @version 1.0
 */
public class GridMetadataAdapter extends AbstractMetadataAdapter {

    public GridMetadataAdapter(RetrievalAttribute attXML)
            throws InstantiationException {

        loadPDOs(attXML);
    }

    private void loadPDOs(RetrievalAttribute attXML)
            throws InstantiationException {
        Level[] levels = getLevels(attXML);
        int size = levels.length;

        List<String> ensembles = null;
        if (attXML.getEnsemble() != null && attXML.getEnsemble().hasSelection()) {
            ensembles = attXML.getEnsemble().getSelectedMembers();
            size *= ensembles.size();
        } else {
            ensembles = Arrays.asList((String) null);
        }

        if (attXML.getTime().getSelectedTimeIndices() != null) {
            if (levels.length > 1
                    || attXML.getTime().getSelectedTimeIndices().size() > 1) {
                size *= attXML.getTime().getSelectedTimeIndices().size();
            }
        }

        pdos = new GridRecord[size];

        GridCoverage gridCoverage = ((GriddedCoverage) attXML.getCoverage())
                .getRequestGridCoverage();

        try {
            gridCoverage = getCoverageFromCache(gridCoverage);
        } catch (Exception e) {
            throw new InstantiationException(e.getMessage());
        }

        if (attXML.getTime().getSelectedTimeIndices() != null) {
            int bin = 0;
            for (String ensemble : ensembles) {
                for (int i = 0; i < attXML.getTime().getSelectedTimeIndices()
                        .size(); i++) {
                    for (int j = 0; j < levels.length; j++) {
                        pdos[bin++] = populateGridRecord(attXML.getSubName(),
                                attXML.getParameter(), levels[j], ensemble,
                                gridCoverage);
                    }
                }
            }
        } else {

            pdos[0] = populateGridRecord(attXML.getSubName(),
                    attXML.getParameter(), levels[0], ensembles.get(0),
                    gridCoverage);

        }
    }

    /**
     * Populate the grid record
     * 
     * @param parm
     * @param level
     * @param gridCoverage
     * @return
     */
    private GridRecord populateGridRecord(String name, Parameter parm,
            Level level, String ensembleId, GridCoverage gridCoverage) {
        return ResponseProcessingUtilities.getGridRecord(name, parm, level,
                ensembleId, gridCoverage);
    }

    /**
     * Try to find the coverage in the cache
     * 
     * @param coverage
     * @return
     */
    private GridCoverage getCoverageFromCache(GridCoverage coverage) {
        return ResponseProcessingUtilities.getCoverageFromCache(coverage);
    }

    private Level[] getLevels(RetrievalAttribute attXML) {
        ArrayList<Level> levels = ResponseProcessingUtilities
                .getOpenDAPGridLevels(attXML.getParameter().getLevels());
        return levels.toArray(new Level[levels.size()]);
    }

    /**
     * Flips the array in the y direction, needed because AWIPS display is
     * backward from NCEP data
     * 
     * @param nx
     * @param ny
     * @param origVals
     * @return
     */
    public static float[] adjustGrid(int nx, int ny, float[] origVals,
            float missingValue, boolean flip) {

        float[] returnVals = new float[(nx * ny)];

        for (int y = 0; y < ny; y++) {
            int revy;
            if (flip) {
                revy = (ny - 1) - y;
            } else {
                revy = y;
            }

            for (int x = 0; x < nx; x++) {
                float value = origVals[(nx * y) + x];
                if (value == missingValue) {
                    value = Util.GRID_FILL_VALUE;
                }
                returnVals[(nx * revy) + x] = value;
            }
        }

        return returnVals;
    }

    /**
     * pre-fill grid and pad when grid from provider is short on values
     * 
     * @param nx
     * @param ny
     * @param dnx
     * @param dny
     * @param vals
     * @return
     */
    public static float[] padUpGrid(int nx, int ny, int dnx, int dny,
            float[] subValues) {
        // first fill entire grid the size we want
        float[] vals = new float[(nx * ny)];
        for (int y = 0; y < ny; y++) {
            for (int x = 0; x < nx; x++) {
                vals[(nx * y) + x] = Util.GRID_FILL_VALUE;
            }
        }
        // writes in the values of the sub cut array actually received from
        // provider
        for (int y = 0; y < dny; y++) {
            for (int x = 0; x < dnx; x++) {
                vals[(dnx * y) + x] = subValues[(dnx * y) + x];
            }
        }

        return vals;
    }

    /**
     * pre-fill grid and pad when grid from provider is long on values
     * 
     * @param nx
     * @param ny
     * @param dnx
     * @param dny
     * @param vals
     * @return
     */
    public static float[] padDownGrid(int nx, int ny, int dnx, int dny,
            float[] subValues) {
        // fill in extra spaces not filled by provider
        int offset = (nx - dnx);
        float[] vals = new float[(nx * ny)];
        // fill it up
        for (int y = 0; y < ny; y++) {
            for (int x = 0; x < nx; x++) {
                vals[(nx * y) + x] = Util.GRID_FILL_VALUE;
            }
        }
        // writes in the values of the sub cut array
        for (int y = 0; y < dny; y++) {
            for (int x = 0; x < dnx; x++) {
                int bin = (dnx * y) + x;
                vals[bin] = subValues[bin];
                if (x == (dnx - 1)) {
                    for (int i = 0; i < offset; i++) {
                        vals[bin + i] = Util.GRID_FILL_VALUE;
                    }
                }
            }
        }

        return vals;
    }
}
