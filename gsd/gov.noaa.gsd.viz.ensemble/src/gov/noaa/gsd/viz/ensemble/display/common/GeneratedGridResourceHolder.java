package gov.noaa.gsd.viz.ensemble.display.common;

import gov.noaa.gsd.viz.ensemble.display.calculate.Calculation;
import gov.noaa.gsd.viz.ensemble.display.calculate.ERFCalculator;
import gov.noaa.gsd.viz.ensemble.display.calculate.Range;
import gov.noaa.gsd.viz.ensemble.display.rsc.GeneratedEnsembleGridResource;
import gov.noaa.gsd.viz.ensemble.display.rsc.GeneratedEnsembleGridResourceData;
import gov.noaa.gsd.viz.ensemble.util.Utilities;

import java.util.StringTokenizer;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

/**
 * Concrete resolution of accessors of generated grid resource attributes.
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

public class GeneratedGridResourceHolder extends GenericResourceHolder {

    GeneratedEnsembleGridResource<?> currRsc = null;

    protected GeneratedGridResourceHolder(AbstractVizResource<?, ?> rsc,
            boolean isSelected) {

        super(rsc, isSelected);
        currRsc = (GeneratedEnsembleGridResource<?>) rsc;
    }

    @Override
    public String getModel() {
        return "";
    }

    @Override
    public String getLevel() {
        String level;

        if ((currRsc.getName() == null) || (currRsc.getName().length() == 0)) {
            level = "<level missing>";
        } else {
            StringTokenizer st = new StringTokenizer(currRsc.getName());
            level = st.nextToken();
        }
        return level;
    }

    @Override
    public String getParameter() {
        String parameter;
        if ((currRsc.getName() == null) || (currRsc.getName().length() == 0)) {
            parameter = "<parameter missing>";
        } else {
            StringTokenizer st = new StringTokenizer(currRsc.getName());
            st.nextToken();
            st.nextToken();
            parameter = st.nextToken();
        }
        return parameter;
    }

    @Override
    public String getDataTime() {
        return "";
    }

    @Override
    public String getUnits() {
        return "";
    }

    @Override
    public Calculation getCalculation() {
        return currRsc.getCalculation();
    }

    public String getGroupName() {

        return getUniqueName();
    }

    public String getUniqueName() {

        String s = currRsc.getName();
        String nodeLabel = Utilities.removeExtraSpaces(s);

        return nodeLabel;
    }

    @Override
    public String getLocation() {
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
    public int hashCode() {
        return getUniqueName().hashCode();
    }

    public Range getRange() {
        Range r = null;
        if (GeneratedEnsembleGridResourceData.class.isAssignableFrom(currRsc
                .getResourceData().getClass())) {
            GeneratedEnsembleGridResourceData grd = (GeneratedEnsembleGridResourceData) currRsc
                    .getResourceData();
            if (ERFCalculator.class.isAssignableFrom(grd.getCalculator()
                    .getClass())) {
                ERFCalculator erf = (ERFCalculator) grd.getCalculator();
                r = erf.getRange();
            }
            grd.getCalculator();
        }
        return r;
    }

}
