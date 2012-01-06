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
package com.raytheon.viz.grid.rsc;

import com.raytheon.uf.common.dataplugin.grib.GribModel;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.level.LevelMapping;
import com.raytheon.uf.viz.core.level.LevelMappingFactory;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

public class GridNameGenerator extends AbstractNameGenerator {

    public interface IGridNameResource {
        public LegendParameters getLegendParameters();
    }

    public static class LegendParameters {
        public GribModel model = null;

        public String type = "";

        public String unit = "";

        public DataTime dataTime;

        public boolean isPlaneLabelDisplayed = true;
    }

    private String planeLabelString;

    public GridNameGenerator() {

    }

    public GridNameGenerator(String planeLabelString) {
        this.planeLabelString = planeLabelString;
    }

    @Override
    public String getName(AbstractVizResource<?, ?> absResource) {
        LegendParameters legendParams = null;
        if (absResource instanceof IGridNameResource) {
            legendParams = ((IGridNameResource) absResource)
                    .getLegendParameters();
        }
        if (legendParams == null || legendParams.model == null) {
            return "Grid";
        }
        if (absResource.getResourceData() instanceof GridResourceData) {
            return getName(legendParams,
                    (GridResourceData) absResource.getResourceData());
        } else {
            return getLegendName(legendParams);
        }
    }

    public String getName(LegendParameters legendParameters,
            GridResourceData gridResourceData) {
        if (legendParameters == null || legendParameters.model == null) {
            return "Grid";
        }
        if (gridResourceData.getCustomLegend() != null) {
            String unit = "";

            if (legendParameters.unit != null) {
                unit = legendParameters.unit;
            }
            return gridResourceData.getCustomLegend() + " (" + unit + ")";
        } else {
            return getLegendName(legendParameters);
        }
    }

    private String getLegendName(LegendParameters legendParams) {
        String plane = this.planeLabelString == null ? lookupPlane(legendParams.model
                .getLevel()) : this.planeLabelString;
        String pert = "";
        if (legendParams.model.getPerturbationNumber() != null) {
            pert = "Perturbation " + legendParams.model.getPerturbationNumber();
        }
        if (legendParams.unit == null) {
            legendParams.unit = "";
        }
        String modelTitle = legendParams.model.getModelTitle();
        // Camel case single word lower case titles, like radar
        if (modelTitle.matches("[a-z]*")) {
            modelTitle = modelTitle.substring(0, 1).toUpperCase()
                    + modelTitle.substring(1);
        }
        if (legendParams.isPlaneLabelDisplayed) {
            return String.format("%s %s %s %s (%s) %s ", modelTitle, plane,
                    legendParams.model.getParameterName(), legendParams.type,
                    legendParams.unit, pert);
        } else {
            return String.format("%s %s %s (%s) %s ", modelTitle,
                    legendParams.model.getParameterName(), legendParams.type,
                    legendParams.unit, pert);
        }
    }

    private String lookupPlane(Level level) {
        LevelMapping mapping = LevelMappingFactory.getInstance()
                .getLevelMappingForLevel(level);
        if (mapping == null) {
            return level.getMasterLevel().getName();
        }
        return mapping.getDisplayName();
    }

    public void setPlaneLabelString(String planeLabelString) {
        this.planeLabelString = planeLabelString;
    }

}
