package gov.noaa.gsd.viz.ensemble.display.common;

import gov.noaa.gsd.viz.ensemble.display.calculate.Calculation;
import gov.noaa.gsd.viz.ensemble.util.Utilities;

import com.raytheon.uf.common.style.level.SingleLevel;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.d2d.xy.adapters.timeseries.GridTimeSeriesAdapter;
import com.raytheon.uf.viz.xy.timeseries.adapter.AbstractTimeSeriesAdapter;
import com.raytheon.uf.viz.xy.timeseries.rsc.TimeSeriesResource;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Concrete resolution of accessors of time series resource attributes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2014   5056      polster     Initial creation
 * 
 * </pre>
 * 
 * @author polster
 * @author jing
 * @version 1.0
 */

public class TimeSeriesResourceHolder extends GenericResourceHolder {

    TimeSeriesResource currRsc = null;

    protected TimeSeriesResourceHolder(AbstractVizResource<?, ?> rsc,
            boolean isSelected) {

        super(rsc, isSelected);
        currRsc = (TimeSeriesResource) rsc;
    }

    @Override
    public String getModel() {
        String model;
        if ((currRsc.getResourceData() == null)
                || (currRsc.getResourceData().getSource() == null)) {
            model = "<Could not obtain model name>";
        } else {
            model = currRsc.getResourceData().getSource();
        }
        return model;
    }

    @Override
    public String getLocation() {

        String latlon = "";

        if ((currRsc.getResourceData() != null)
                && (currRsc.getResourceData().getCoordinate() != null)) {
            Coordinate c = currRsc.getResourceData().getCoordinate();
            String ns = c.y >= 0 ? "N" : "S";
            String ew = c.x >= 0 ? "E" : "W";
            latlon = String.format("pt%s %d%s %d%s", currRsc.getResourceData()
                    .getPointLetter(), Math.round(Math.abs(c.y)), ns, Math
                    .round(Math.abs(c.x)), ew);
        }

        return latlon;
    }

    @Override
    // don't return a level if this is a height time series -- see
    // TimeSeriesResource::getName()
    public String getLevel() {

        String levelKey = currRsc.getResourceData().getLevelKey();
        String levelUnit = levelKey.replaceAll("[^a-zA-Z]", "");
        boolean isHeight = levelUnit.equalsIgnoreCase("mb")
                || levelUnit.equalsIgnoreCase("agl")
                || levelUnit.contains("Agl");

        if (currRsc.getAdapter() != null) {
            if (!isHeight) {
                SingleLevel level = currRsc.getAdapter().getLevel();
                levelKey = (int) level.getValue() + level.getTypeString();
            } else {
                levelKey = "";
            }
        } else {
            levelKey = "";
        }
        return levelKey;
    }

    @Override
    public String getUnits() {

        String units = currRsc.getUnits();
        if (units == null) {
            units = "";
        }
        return units;
    }

    @Override
    public String getParameter() {

        String p = "";
        if (currRsc.getAdapter() != null) {
            p = currRsc.getAdapter().getParameterName();
            if (p == null)
                p = "";
            if (p.compareTo("Height") == 0) {
                p = "Hgt";
            }
        }
        return p;
    }

    @Override
    public String getEnsembleId() {
        String ensId = "<undef perturbation>";

        if (currRsc.getAdapter() instanceof GridTimeSeriesAdapter) {
            try {
                AbstractTimeSeriesAdapter<?> adapter = currRsc.getAdapter();
                if ((((GridTimeSeriesAdapter) adapter) != null)
                        && ((((GridTimeSeriesAdapter) adapter)
                                .getArbitraryRecord()) != null)
                        && ((((GridTimeSeriesAdapter) adapter)
                                .getArbitraryRecord().getInfo()) != null)) {

                    if (((GridTimeSeriesAdapter) adapter)
                            .getArbitraryRecord().getInfo()
                            .getEnsembleId() != null) {
                        ensId = ((GridTimeSeriesAdapter) adapter)
                                .getArbitraryRecord().getInfo()
                                .getEnsembleId();
                    } else {
                        ensId = currRsc.getName();
                    }

                    if ((getModel() != null)
                            && ((ensId != null) && (getModel().indexOf("SREF") >= 0))) {
                        ensId = srefPerturbationPrettyfied(ensId);
                    }
                }
            } catch (Exception e) {
                return ensId;
            }
        }

        return ensId;
    }

    @Override
    public String getEnsembleIdRaw() {
        return "";
    }

    @Override
    public String getStationId() {
        String stnID = "";
        if (currRsc.getResourceData().getMetadataMap()
                .get("location.stationId") != null) {
            stnID = currRsc.getResourceData().getMetadataMap()
                    .get("location.stationId").getConstraintValue();
        }
        return stnID;
    }

    public String getGroupName() {

        String sb = String.format("%s %s %s %s %s", getModel(), getLevel(),
                getParameter(), getLocation(), getUnits() != null
                        && getUnits().equals("") == false ? "(" + getUnits()
                        + ")" : "");
        String nodeLabel = Utilities.removeExtraSpaces(sb);
        return nodeLabel;

    }

    public String getUniqueName() {

        String sb = String
                .format("%s %s %s %s %s %s",
                        getModel(),
                        getLevel(),
                        getParameter(),
                        getLocation(),
                        getUnits() != null && getUnits().equals("") == false ? "("
                                + getUnits() + ")"
                                : "",
                        getEnsembleId() != null
                                && getEnsembleId().equals("") == false ? getEnsembleId()
                                : "");
        String nodeLabel = Utilities.removeExtraSpaces(sb);
        return nodeLabel;
    }

    @Override
    public String getDataTime() {
        return "";
    }

    @Override
    public String getType() {
        return "";
    }

    @Override
    public Calculation getCalculation() {
        return null;
    }

    @Override
    public int hashCode() {
        return getUniqueName().hashCode();
    }

}
