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

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizCommunicationException;
import com.raytheon.uf.viz.core.level.LevelMapping;
import com.raytheon.uf.viz.core.level.LevelMappingFactory;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

/**
 * 
 * Name generator that creates names for grids based off a standards set of
 * LegendParameters.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 20, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GridNameGenerator extends AbstractNameGenerator {

    public interface IGridNameResource {
        public LegendParameters getLegendParameters();
    }

    public static class LegendParameters {

        public String model = "";

        public String parameter = "";

        public Level level;

        public String type = "";

        public String unit = "";

        public DataTime dataTime;

        public String ensembleId;

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
        String plane = this.planeLabelString == null ? lookupPlane(legendParams.level)
                : this.planeLabelString;
        String pert = "";
        if (legendParams.ensembleId != null) {
            pert = "Perturbation " + legendParams.ensembleId;
        }
        if (legendParams.unit == null) {
            legendParams.unit = "";
        }
        String modelTitle = legendParams.model;
        // Camel case single word lower case titles, like radar
        if (modelTitle.matches("[a-z]*")) {
            modelTitle = modelTitle.substring(0, 1).toUpperCase()
                    + modelTitle.substring(1);
        }
        if (legendParams.isPlaneLabelDisplayed) {
            return String.format("%s %s %s %s (%s) %s ", modelTitle, plane,
                    legendParams.parameter, legendParams.type,
                    legendParams.unit, pert);
        } else {
            return String.format("%s %s %s (%s) %s ", modelTitle,
                    legendParams.parameter, legendParams.type,
                    legendParams.unit, pert);
        }
    }

    private String lookupPlane(Level level) {
        try {
            LevelMapping mapping = LevelMappingFactory.getInstance()
                    .getLevelMappingForLevel(level);
            if (mapping == null) {
                return level.getMasterLevel().getName();
            }
            return mapping.getDisplayName();
        } catch (VizCommunicationException e) {
            return level.getMasterLevel().getName();

        }
    }

    public void setPlaneLabelString(String planeLabelString) {
        this.planeLabelString = planeLabelString;
    }

}
