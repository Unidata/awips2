package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;

import gov.noaa.gsd.viz.ensemble.util.RequestableResourceMetadata;

/**
 * An element is defined as a field and plane pair.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 10, 2015  12372      polster     Initial creation
 * Nov 20, 2016  19443      polster     Rename method to include resource type
 * Dec 01, 2017  41520      polster     Added isCompatible method.
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */

public class FieldPlanePair {

    private String fieldAbbrev = null;

    private String fieldFullName = null;

    private String plane = null;

    private boolean isVisible = false;

    private DisplayType displayType = null;

    /* used for ui tree content provision */
    private String sourceParent = null;

    public FieldPlanePair(String field, String p, boolean v, DisplayType dt) {
        this.fieldAbbrev = field;
        fieldFullName = "";
        sourceParent = "";
        plane = p;
        displayType = dt;
        setResourceVisible(v);
    }

    public FieldPlanePair(String fieldAbbreviation, String fieldLongName,
            String p, boolean v, DisplayType dt) {
        fieldAbbrev = fieldAbbreviation;
        fieldFullName = fieldLongName;
        sourceParent = "";
        plane = p;
        displayType = dt;
        setResourceVisible(v);
    }

    public String getFieldAbbrev() {
        return fieldAbbrev;
    }

    public String getFieldLongName() {
        return fieldFullName;
    }

    public String getPlane() {
        return plane;
    }

    public String getShortName() {
        return plane + " " + fieldAbbrev;
    }

    public String getLongName() {
        return plane + " " + fieldFullName;
    }

    public String toString() {
        return plane + " " + fieldAbbrev;
    }

    public boolean isResourceVisible() {
        return isVisible;
    }

    public void setResourceVisible(boolean isVisible) {
        this.isVisible = isVisible;
    }

    public String getSourceParent() {
        return sourceParent;
    }

    public DisplayType getDisplayType() {
        return displayType;
    }

    public void setSourceParent(String sourceParent) {
        this.sourceParent = sourceParent;
    }

    /**
     * Is the formal argument the same product name and plane as this FPP?
     * 
     * @param rsc
     * @return true if the resource is kindred with this field plan pair
     */
    public boolean isCompatible(AbstractVizResource<?, ?> rsc) {
        boolean isCompatible = false;
        if (rsc.getResourceData() instanceof AbstractRequestableResourceData) {

            AbstractRequestableResourceData ard = (AbstractRequestableResourceData) rsc
                    .getResourceData();
            RequestableResourceMetadata rrd = new RequestableResourceMetadata(
                    ard);
            String fullName = rrd.getFieldFullName();
            String rscPlane = rrd.getPlane();
            if (fullName != null && rscPlane != null) {
                if (fullName.equals(getFieldLongName())
                        && rscPlane.equals(getPlane())) {
                    isCompatible = true;
                }
            }
        }
        return isCompatible;
    }

}
