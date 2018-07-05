package gov.noaa.gsd.viz.ensemble.display.common;

import java.util.StringTokenizer;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

import gov.noaa.gsd.viz.ensemble.display.calculate.Calculation;
import gov.noaa.gsd.viz.ensemble.display.calculate.ERFCalculator;
import gov.noaa.gsd.viz.ensemble.display.calculate.Range;
import gov.noaa.gsd.viz.ensemble.display.rsc.GeneratedEnsembleGridResource;
import gov.noaa.gsd.viz.ensemble.display.rsc.GeneratedEnsembleGridResourceData;
import gov.noaa.gsd.viz.ensemble.util.Utilities;

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
 * Jun 27  2017   19325    jing         Fix isSimilarTo() method
 * 
 * </pre>
 * 
 * @author polster
 * @author jing
 * @version 1.0
 */

public class GeneratedGridResourceHolder extends AbstractResourceHolder {

    GeneratedEnsembleGridResource currRsc = null;

    public GeneratedGridResourceHolder(AbstractVizResource<?, ?> rsc) {

        super(rsc);
        currRsc = (GeneratedEnsembleGridResource) rsc;
        isGenerated = true;
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
    public String getGroupName() {
        return "";
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

    @Override
    public String getGeneralName() {
        return getSpecificName();
    }

    @Override
    public String getSpecificName() {

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

    public Range getRange() {
        Range r = null;
        if (GeneratedEnsembleGridResourceData.class
                .isAssignableFrom(currRsc.getResourceData().getClass())) {
            GeneratedEnsembleGridResourceData grd = (GeneratedEnsembleGridResourceData) currRsc
                    .getResourceData();
            if (ERFCalculator.class
                    .isAssignableFrom(grd.getCalculator().getClass())) {
                ERFCalculator erf = (ERFCalculator) grd.getCalculator();
                r = erf.getRange();
            }
            grd.getCalculator();
        }
        return r;
    }

    @Override
    public boolean requiresLoadCheck() {
        return false;
    }

    /**
     * Boolean method to return true when two generated resources have the same
     * calculation, level, and unit.
     * 
     * @param grh
     *            the generated resource against which to compare similarity
     * @return true if this generated resource is similar to the given grh
     *         argument
     */
    public boolean isSimilarTo(GeneratedGridResourceHolder grh) {
        boolean areSimilar = false;

        if (currRsc.getDisplayType() == grh.currRsc.getDisplayType()
                && (getCalculation().equals(grh.getCalculation())
                        && getRsc().getName().equals(grh.getRsc().getName())
                        && getLevel().equals(grh.getLevel())
                        && getUnits().equals(grh.getUnits()))) {
            areSimilar = true;
        }
        return areSimilar;
    }

    @Override
    public boolean isEnsembleGroup() {
        return false;
    }

    @Override
    public AbstractResourceHolder[] getChildren() {
        return null;
    }

    @Override
    public boolean hasChildren() {
        return false;
    }

}
