package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

import com.raytheon.uf.viz.core.rsc.DisplayType;

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
        setVisible(v);
    }

    public FieldPlanePair(String fieldAbbreviation, String fieldLongName, String p,
            boolean v, DisplayType dt) {
        fieldAbbrev = fieldAbbreviation;
        fieldFullName = fieldLongName;
        sourceParent = "";
        plane = p;
        displayType = dt;
        setVisible(v);
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

    public boolean isVisible() {
        return isVisible;
    }

    public void setVisible(boolean isVisible) {
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

}
