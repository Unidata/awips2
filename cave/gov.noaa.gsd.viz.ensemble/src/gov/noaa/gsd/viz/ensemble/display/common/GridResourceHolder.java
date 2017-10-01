package gov.noaa.gsd.viz.ensemble.display.common;

import gov.noaa.gsd.viz.ensemble.display.calculate.Calculation;
import gov.noaa.gsd.viz.ensemble.util.Utilities;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMapping;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMappingFactory;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.viz.grid.rsc.GridNameGenerator;
import com.raytheon.viz.grid.rsc.GridNameGenerator.IGridNameResource;
import com.raytheon.viz.grid.rsc.GridNameGenerator.LegendParameters;

/**
 * Concrete resolution of accessors of typical grid resource attributes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2014   5056       polster     Initial creation
 * 
 * </pre>
 * 
 * @author polster
 * @author jing
 * @version 1.0
 */
public class GridResourceHolder extends AbstractResourceHolder {

    GridNameGenerator.IGridNameResource currRsc = null;

    protected GridResourceHolder(AbstractVizResource<?, ?> rsc,
            boolean isSelected) {

        super(rsc, isSelected);
        currRsc = (GridNameGenerator.IGridNameResource) rsc;
    }

    @Override
    public String getModel() {
        String model;
        if (currRsc.getLegendParameters() == null) {
            model = "<model missing>";
        } else {
            model = currRsc.getLegendParameters().model;
        }
        return model;
    }

    @Override
    public String getLevel() {
        String level;
        if (currRsc.getLegendParameters() == null) {
            level = "<level missing>";
        } else {
            level = lookupPlane(currRsc.getLegendParameters().level);
        }

        return level;
    }

    @Override
    public String getParameter() {
        String parameter;
        if (currRsc.getLegendParameters() == null) {
            parameter = "<param missing>";
        } else {
            parameter = currRsc.getLegendParameters().parameter;
        }
        return parameter;
    }

    @Override
    public String getDataTime() {
        String datatime;
        if ((currRsc.getLegendParameters() == null)
                || (currRsc.getLegendParameters().dataTime == null)) {
            datatime = "<not loaded>";
        } else {
            LegendParameters legendParams = ((IGridNameResource) rsc)
                    .getLegendParameters();
            datatime = legendParams.dataTime.getLegendString();
        }
        return datatime;
    }

    @Override
    public String getUnits() {
        String units;
        if (currRsc.getLegendParameters() == null) {
            units = "<units missing>";
        } else {
            units = currRsc.getLegendParameters().unit;
        }
        return units;
    }

    @Override
    public String getType() {
        String type;
        if (currRsc.getLegendParameters() == null) {
            type = "<type missing>";
        } else {
            type = currRsc.getLegendParameters().type;
        }
        return type;
    }

    @Override
    public String getEnsembleId() {

        String ensId = "<ensemble id missing>";
        if ((currRsc.getLegendParameters() != null) && (getModel() != null)) {
            ensId = currRsc.getLegendParameters().ensembleId;
            if ((ensId != null) && (getModel().indexOf("SREF") >= 0)) {
                ensId = srefPerturbationPrettyfied(ensId);
            }
        }
        return ensId;
    }

    @Override
    public String getEnsembleIdRaw() {
        String ensId = "";
        if ((currRsc.getLegendParameters() != null) && (getModel() != null)) {
            ensId = currRsc.getLegendParameters().ensembleId;
        }
        return ensId;
    }

    private String lookupPlane(Level level) {
        LevelMapping mapping = LevelMappingFactory.getInstance(
                LevelMappingFactory.VOLUMEBROWSER_LEVEL_MAPPING_FILE)
                .getLevelMappingForLevel(level);
        if (mapping == null) {
            return level.getMasterLevel().getName();
        }
        return mapping.getDisplayName();
    }

    public String getGroupName() {

        String sb = String.format("%s %s %s %s", getModel(), getLevel(),
                getParameter(), getUnits() != null
                        && getUnits().equals("") == false ? "(" + getUnits()
                        + ")" : "");
        return sb;
    }

    public String getSpecificName() {

        String sb = String
                .format("%s %s %s %s %s",
                        getModel(),
                        getLevel(),
                        getParameter(),
                        getUnits() != null && getUnits().equals("") == false ? "("
                                + getUnits() + ")"
                                : "",
                        getEnsembleId() != null
                                && getEnsembleId().equals("") == false ? getEnsembleId()
                                : "");

        String nodeLabel = Utilities.removeExtraSpaces(sb.toString());
        return nodeLabel;
    }

    @Override
    public String getLocation() {
        return "";
    }

    @Override
    public String getStationId() {
        return "";
    }

    @Override
    public Calculation getCalculation() {
        return null;
    }

    @Override
    public boolean requiresLoadCheck() {
        return true;
    }

    @Override
    public String getGeneralName() {
        return getSpecificName();
    }

}
