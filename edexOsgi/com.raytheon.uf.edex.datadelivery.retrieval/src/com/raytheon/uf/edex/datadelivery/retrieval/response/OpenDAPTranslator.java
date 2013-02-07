package com.raytheon.uf.edex.datadelivery.retrieval.response;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.GriddedCoverage;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.datadelivery.retrieval.metadata.adapters.GridMetadataAdapter;
import com.raytheon.uf.edex.datadelivery.retrieval.util.ResponseProcessingUtilities;

import dods.dap.BaseType;
import dods.dap.DArray;
import dods.dap.DArrayDimension;
import dods.dap.DGrid;
import dods.dap.DataDDS;
import dods.dap.PrimitiveVector;

/**
 * OPenDAP specific translation tools
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2011            dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class OpenDAPTranslator extends RetrievalTranslator {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(OpenDAPTranslator.class);

    public OpenDAPTranslator(RetrievalAttribute attXML)
            throws InstantiationException {
        super(attXML);
    }

    public PluginDataObject[] asPluginDataObjects(DataDDS dds) {
        PluginDataObject[] pdos = null;

        for (Enumeration<?> variables = dds.getVariables(); variables
                .hasMoreElements();) {

            BaseType bt = (BaseType) variables.nextElement();
            Class<?> dclass = bt.getClass();

            try {

                if (dclass == DGrid.class) {
                    pdos = translateGrid((DGrid) bt);
                } else if (dclass == DArray.class) {
                    pdos = translateArray((DArray) bt);
                }

            } catch (Exception e) {
                statusHandler.error("Unable to translate OpenDAP Response", e);
            }
        }

        return pdos;
    }

    private PluginDataObject[] translateGrid(DGrid dgrid) throws Exception {
        try {
            PluginDataObject[] record = null;

            Enumeration<?> e = dgrid.getVariables();
            // for (Enumeration e = dgrid.getVariables();
            // e.hasMoreElements();) {
            // testing out only one variable at this time
            if (e.hasMoreElements()) {
                record = translateArray((DArray) e.nextElement());
            }

            return record;
        } catch (Exception e) {
            throw e;
        }
    }

    private PluginDataObject[] translateArray(DArray darray) throws Exception {

        int nx = 0;
        int ny = 0;
        int dnx = 0;
        int dny = 0;
        int diffX = 0;
        int numTimes = getSubsetNumTimes();
        int numLevels = getSubsetNumLevels();
        ArrayList<DataTime> times = getTimes();

        for (Enumeration<?> e = darray.getDimensions(); e.hasMoreElements();) {
            DArrayDimension d = (DArrayDimension) e.nextElement();

            if (d.getName().equals("lat")) {
                dny = d.getSize();
            } else if (d.getName().equals("lon")) {
                dnx = d.getSize();
            }
        }

        // retrieve data
        if (attXML.getCoverage() instanceof GriddedCoverage) {
            GriddedCoverage gridCoverage = (GriddedCoverage) attXML
                    .getCoverage();

            nx = gridCoverage.getRequestGridCoverage().getNx();
            ny = gridCoverage.getRequestGridCoverage().getNy();

            if (dnx != nx || dny != ny) {
                statusHandler.info("GRID SIZE INCONSISTANTCY!!!!!!!!" + nx
                        + " DNX: " + dnx + " diffX: " + diffX);
            }
        }

        int gridSize = nx * ny;

        PrimitiveVector pm = darray.getPrimitiveVector();
        float[] values = (float[]) pm.getInternalStorage();

        List<String> ensembles = null;
        if (attXML.getEnsemble() != null && attXML.getEnsemble().hasSelection()) {
            ensembles = attXML.getEnsemble().getSelectedMembers();
        } else {
            ensembles = Arrays.asList((String) null);
        }

        // time dependencies
        int start = 0;
        PluginDataObject[] records = new PluginDataObject[numLevels * numTimes
                * ensembles.size()];

        int bin = 0;
        for (int i = 0; i < ensembles.size(); i++) {
            for (DataTime dataTime : times) {
                for (int j = 0; j < numLevels; j++) {
                    PluginDataObject record = getPdo(bin);
                    record.setDataTime(dataTime);
                    record.constructDataURI();

                    int end = start + gridSize;

                    float[] subValues = Arrays.copyOfRange(values, start, end);

                    subValues = GridMetadataAdapter.adjustGrid(nx, ny,
                            subValues, Float.parseFloat(attXML.getParameter()
                                    .getMissingValue()), true);

                    record.setMessageData(subValues);
                    record.setOverwriteAllowed(true);
                    records[bin++] = record;
                    statusHandler.info("Creating record: "
                            + record.getDataURI());
                    start = end;
                }
            }
        }

        return records;
    }

    /**
     * get # of subset times
     */
    protected int getSubsetNumTimes() {

        return ResponseProcessingUtilities.getOpenDAPGridNumTimes(attXML
                .getTime());
    }

    /**
     * get subset levels
     */
    protected int getSubsetNumLevels() {

        return ResponseProcessingUtilities.getOpenDAPGridNumLevels(attXML
                .getParameter());
    }

    /**
     * get list of data times from subset
     */
    protected ArrayList<DataTime> getTimes() {

        return ResponseProcessingUtilities.getOpenDAPGridDataTimes(attXML
                .getTime());
    }

}
