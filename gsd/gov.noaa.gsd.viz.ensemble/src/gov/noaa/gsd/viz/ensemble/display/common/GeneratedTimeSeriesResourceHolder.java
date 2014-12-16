package gov.noaa.gsd.viz.ensemble.display.common;

import gov.noaa.gsd.viz.ensemble.display.calculate.Calculation;
import gov.noaa.gsd.viz.ensemble.display.rsc.timeseries.GeneratedTimeSeriesResource;
import gov.noaa.gsd.viz.ensemble.util.Utilities;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

/**
 * Concrete resolution of accessors of typical time series resource attributes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2014    5056     polster     Initial creation
 * 
 * </pre>
 * 
 * @author polster
 * @author jing
 * @version 1.0
 */
public class GeneratedTimeSeriesResourceHolder extends TimeSeriesResourceHolder {

    GeneratedTimeSeriesResource<?> currRsc = null;

    protected GeneratedTimeSeriesResourceHolder(AbstractVizResource<?, ?> rsc,
            boolean isSelected) {

        super(rsc, isSelected);
        currRsc = (GeneratedTimeSeriesResource<?>) rsc;
    }

    @Override
    public String getModel() {
        return "";
    }

    @Override
    public String getEnsembleId() {
        return "";
    }

    @Override
    public Calculation getCalculation() {
        return currRsc.getCalculation();
    }

    @Override
    public String getParameter() {

        String p = "";
        if (currRsc.getParameterName() != null) {
            p = currRsc.getParameterName();
            if (p.compareTo("Height") == 0) {
                p = "Hgt";
            }
        }
        return p;
    }

    @Override
    public String getUniqueName() {

        String s = currRsc.getName();
        String nodeLabel = Utilities.removeExtraSpaces(s);
        return nodeLabel;

    }
}
