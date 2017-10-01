package gov.noaa.gsd.viz.ensemble.display.common;

import gov.noaa.gsd.viz.ensemble.display.calculate.Calculation;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

/**
 * This class is used to encapsulate visible resources in order to make it
 * easier for the developer to get access to the resource meta data such as that
 * data which is commonly displayed in the editor/map legend for a given
 * resource.
 * 
 * Used for encapsulating the more generic <code>AbstractVizResource</code>
 * instances that are not:
 * <ul>
 * <li>GeneratedEnsembleGridResource
 * <li>GeneratedTimeSeriesResource
 * <li>GeneratedTimeSeriesResourceHolder
 * <li>AbstractGridResource
 * <li>TimeSeriesResource
 * <li>HistogramResource
 * </ul>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 17, 2016    13211    polster     Renamed class from AbstractResourceHolder
 *                                      to GenericResourceHolder
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */

public class GenericResourceHolder extends AbstractResourceHolder {

    protected GenericResourceHolder(AbstractVizResource<?, ?> rsc,
            boolean isSelected) {

        super(rsc, isSelected);
        this.rsc = rsc;
    }

    @Override
    public boolean requiresLoadCheck() {
        return false;
    }

    @Override
    public String getGroupName() {
        return "";
    }

    @Override
    public String getGeneralName() {
        return getSpecificName();
    }

    @Override
    public String getSpecificName() {
        return rsc.getName();
    }

    @Override
    public String getModel() {
        return "";
    }

    @Override
    public String getLocation() {
        return "";
    }

    @Override
    public String getLevel() {
        return "";
    }

    @Override
    public String getParameter() {
        return "";
    }

    @Override
    public String getUnits() {
        return "";
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
    public String getEnsembleId() {
        return "";
    }

    @Override
    public String getEnsembleIdRaw() {
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

}
