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
package com.raytheon.uf.viz.d2d.gfe.rsc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.gfe.dataaccess.GFEDataAccessUtil;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.style.ParamLevelMatchCriteria;
import com.raytheon.uf.viz.core.style.level.Level;
import com.raytheon.uf.viz.core.style.level.SingleLevel;
import com.raytheon.viz.grid.rsc.general.AbstractGridResource;
import com.raytheon.viz.grid.rsc.general.GeneralGridData;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 9, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GFEGridResource extends AbstractGridResource<GFEGridResourceData> {

    private ParmID parmId;

    protected GFEGridResource(GFEGridResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.grid.rsc.general.GeneralGridResource#addDataObject(com
     * .raytheon.uf.common.dataplugin.PluginDataObject)
     */
    @Override
    protected void addDataObject(PluginDataObject pdo) {
        if (pdo instanceof GFERecord) {
            GFERecord gfeRecord = (GFERecord) pdo;
            if (parmId == null) {
                parmId = gfeRecord.getParmId();
            }
            super.addDataObject(pdo);
        }
    }

    private void populateGridParmInfo(GFERecord gfeRecord) {
        if (gfeRecord.getGridInfo() != null) {
            return;
        }
        try {
            gfeRecord
                    .setGridInfo(GFEDataAccessUtil.getGridParmInfo(gfeRecord.getParmId()));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public ParamLevelMatchCriteria getMatchCriteria() {
        return getMatchCriteria(parmId);
    }

    @Override
    public List<GeneralGridData> getData(DataTime time,
            List<PluginDataObject> pdos) throws VizException {
        if (pdos == null) {
            return null;
        }
        GFERecord gfeRecord = (GFERecord) pdos.get(0);
        IGridSlice slice = null;
        try {
            slice = GFEDataAccessUtil.getSlice(gfeRecord);
        } catch (Exception e) {
            throw new VizException(e);
        }
        populateGridParmInfo(gfeRecord);
        GridGeometry2D gridGeometry = MapUtil.getGridGeometry(gfeRecord
                .getGridInfo().getGridLoc());
        if (slice instanceof VectorGridSlice) {
            VectorGridSlice vSlice = (VectorGridSlice) slice;
            return Arrays.asList(GeneralGridData.createVectorData(gridGeometry,
                    vSlice.getMagGrid().getBuffer(), vSlice.getDirGrid()
                            .getBuffer(), slice.getGridInfo().getUnitObject()));
        } else if (slice instanceof ScalarGridSlice) {
            ScalarGridSlice sSlice = (ScalarGridSlice) slice;
            return Arrays.asList(GeneralGridData.createScalarData(gridGeometry,
                    sSlice.getScalarGrid().getBuffer(), slice.getGridInfo()
                            .getUnitObject()));
        } else if (slice == null) {
            throw new VizException("Unable to load GFE Slice Data");
        } else {
            throw new VizException("Unable GFE Slice of type "
                    + slice.getClass().getSimpleName());
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getName()
     */
    @Override
    public String getName() {
        if (resourceData.getLegendString() != null) {
            return resourceData.getLegendString();
        }
        String displayTypeString = DisplayType
                .getAbbreviation(getDisplayType());
        String siteId = "";
        String modelName = "";
        String parmName = "";
        String unitLabel = "?";
        if (parmId != null) {
            siteId = parmId.getDbId().getSiteId();
            modelName = parmId.getDbId().getModelName();
            parmName = parmId.getParmName();
        }
        if (stylePreferences != null) {
            unitLabel = stylePreferences.getDisplayUnitLabel();
        }

        return String.format("GFE(%s %s) %s %s (%s)  ", siteId, modelName,
                parmName, displayTypeString, unitLabel);
    }

    public static ParamLevelMatchCriteria getMatchCriteria(ParmID parmId) {
        ParamLevelMatchCriteria criteria = new ParamLevelMatchCriteria();
        criteria.setParameterName(new ArrayList<String>());
        criteria.setLevels(new ArrayList<Level>());
        criteria.setCreatingEntityNames(new ArrayList<String>());
        String parameter = "GFE:" + parmId.getParmName();
        SingleLevel level = new SingleLevel(Level.LevelType.SURFACE);
        String model = "GFE:" + parmId.getDbId().getModelName();
        if (!criteria.getParameterNames().contains(parameter)) {
            criteria.getParameterNames().add(parameter);
        }
        if (!criteria.getLevels().contains(level)) {
            criteria.getLevels().add(level);
        }
        if (!criteria.getCreatingEntityNames().contains(model)) {
            criteria.getCreatingEntityNames().add(model);
        }
        return criteria;
    }
}
