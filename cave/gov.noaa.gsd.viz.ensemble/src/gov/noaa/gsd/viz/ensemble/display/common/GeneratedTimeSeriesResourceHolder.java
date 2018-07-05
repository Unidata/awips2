package gov.noaa.gsd.viz.ensemble.display.common;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

import gov.noaa.gsd.viz.ensemble.display.calculate.Calculation;
import gov.noaa.gsd.viz.ensemble.display.rsc.timeseries.GeneratedTimeSeriesResource;
import gov.noaa.gsd.viz.ensemble.util.Utilities;

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
public class GeneratedTimeSeriesResourceHolder
        extends TimeSeriesResourceHolder {

    GeneratedTimeSeriesResource<?> currRsc = null;

    public GeneratedTimeSeriesResourceHolder(AbstractVizResource<?, ?> rsc) {

        super(rsc);
        currRsc = (GeneratedTimeSeriesResource<?>) rsc;
        isGenerated = true;

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
    public String getSpecificName() {

        String s = currRsc.getName();
        String nodeLabel = Utilities.removeExtraSpaces(s);
        return nodeLabel;

    }

    /**
     * Boolean method to return true when two generated resources have the same
     * calculation, level, and unit.
     * 
     * @param tsrh
     *            the generated resource against which to compare similarity
     * @return true if this generated resource is similar to the given grh
     *         argument
     */
    public boolean isSimilarTo(GeneratedTimeSeriesResourceHolder tsrh) {
        boolean areSimilar = false;

        if (getCalculation().equals(tsrh.getCalculation())
                && getLevel().equals(tsrh.getLevel())
                && getUnits().equals(tsrh.getUnits())) {
            areSimilar = true;
        }
        return areSimilar;
    }

}
