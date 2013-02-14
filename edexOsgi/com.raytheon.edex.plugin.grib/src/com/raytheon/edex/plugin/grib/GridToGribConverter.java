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
package com.raytheon.edex.plugin.grib;

import com.raytheon.edex.plugin.grib.util.GribModelCache;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grib.GribModel;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 4, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GridToGribConverter {

    private static GridToGribConverter instance = new GridToGribConverter();

    public static GridToGribConverter getInstance() {
        return instance;
    }

    private GridToGribConverter() {

    }

    public GribRecord[] convert(GridRecord[] records) throws PluginException,
            DataAccessLayerException {
        GribRecord[] result = new GribRecord[records.length];
        for (int i = 0; i < records.length; i += 1) {
            GridRecord grid = records[i];
            GribRecord grib = new GribRecord();
            GribModel model = new GribModel();
            grib.setPluginName("grib");
            grib.setDataTime(grid.getDataTime());
            model.setModelName(grid.getDatasetId());
            if (grid.getSecondaryId() != null
                    && grid.getSecondaryId().startsWith("Version")) {
                grib.setGridVersion(Integer.parseInt(grid.getSecondaryId()
                        .replace("Version", "")));
            }
            if ("ctl1".equals(grid.getEnsembleId())) {
                model.setPerturbationNumber(1);
                model.setTypeEnsemble(1);
            } else if ("ctl2".equals(grid.getEnsembleId())) {
                model.setPerturbationNumber(2);
                model.setTypeEnsemble(1);
            } else if ("n1".equals(grid.getEnsembleId())) {
                model.setPerturbationNumber(3);
                model.setTypeEnsemble(2);
            } else if ("p1".equals(grid.getEnsembleId())) {
                model.setPerturbationNumber(4);
                model.setTypeEnsemble(3);
            } else if ("n2".equals(grid.getEnsembleId())) {
                model.setPerturbationNumber(5);
                model.setTypeEnsemble(2);
            } else if ("p2".equals(grid.getEnsembleId())) {
                model.setPerturbationNumber(6);
                model.setTypeEnsemble(3);
            } else if ("n3".equals(grid.getEnsembleId())) {
                model.setPerturbationNumber(7);
                model.setTypeEnsemble(2);
            } else if ("p3".equals(grid.getEnsembleId())) {
                model.setPerturbationNumber(8);
                model.setTypeEnsemble(3);
            } else if ("n4".equals(grid.getEnsembleId())) {
                model.setPerturbationNumber(9);
                model.setTypeEnsemble(2);
            } else if ("p4".equals(grid.getEnsembleId())) {
                model.setPerturbationNumber(10);
                model.setTypeEnsemble(3);
            } else if ("n5".equals(grid.getEnsembleId())) {
                model.setPerturbationNumber(11);
                model.setTypeEnsemble(2);
            } else if ("p5".equals(grid.getEnsembleId())) {
                model.setPerturbationNumber(12);
                model.setTypeEnsemble(3);
            }
            Object centerid = grid.getExtraAttribute("centerid");
            if (centerid != null && centerid instanceof Integer) {
                model.setCenterid((Integer) centerid);
            }
            Object subcenterid = grid.getExtraAttribute("subcenterid");
            if (subcenterid != null && subcenterid instanceof Integer) {
                model.setSubcenterid((Integer) subcenterid);
            }
            Object genprocess = grid.getExtraAttribute("genprocess");
            if (genprocess != null && genprocess instanceof Integer) {
                model.setGenprocess((Integer) genprocess);
            }
            Object backGenprocess = grid.getExtraAttribute("backGenprocess");
            if (backGenprocess != null && backGenprocess instanceof Integer) {
                model.setBackGenprocess((Integer) backGenprocess);
            }
            Object pdsTemplate = grid.getExtraAttribute("pdsTemplate");
            if (pdsTemplate != null && pdsTemplate instanceof Integer) {
                model.setPdsTemplate((Integer) pdsTemplate);
            }
            Object gridid = grid.getExtraAttribute("gridid");
            if (gridid != null && gridid instanceof String) {
                model.setGridid((String) gridid);
            }
            Object numForecasts = grid.getExtraAttribute("numForecasts");
            if (numForecasts != null && numForecasts instanceof Integer) {
                model.setNumForecasts((Integer) numForecasts);
            }
            model.setLevel(grid.getLevel());
            model.setLocation(grid.getLocation());
            model.setParameterAbbreviation(grid.getParameter()
                    .getAbbreviation());
            model.setParameterName(grid.getParameter().getName());
            model.setParameterUnit(grid.getParameter().getUnitString());
            model = GribModelCache.getInstance().getModel(model);
            grib.setModelInfo(model);
            grib.constructDataURI();
            result[i] = grib;
        }
        return result;
    }
}
