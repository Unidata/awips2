package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

import java.util.ArrayList;
import java.util.List;

/**
 * The element set is a list of 1 to many elements (or field/plane pairs).
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

public class FieldPlanePairSet {

    public static final String emptyValue = "";

    private String modelFamilyName = null;

    private ModelFamily modelFamily = null;

    private ArrayList<FieldPlanePair> oneSet = null;

    public FieldPlanePairSet() {
        oneSet = new ArrayList<FieldPlanePair>();
        modelFamily = null;
    }

    public FieldPlanePairSet(ModelFamily family) {
        oneSet = new ArrayList<FieldPlanePair>();
        modelFamily = family;
    }

    public ModelFamily getModelFamily() {
        return modelFamily;
    }

    public void setModelFamily(ModelFamily modelFamily) {
        this.modelFamily = modelFamily;
    }

    public void add(FieldPlanePair n) {
        oneSet.add(n);
    }

    public int size() {
        return oneSet.size();
    }

    public List<FieldPlanePair> getNodes() {
        return oneSet;
    }

    public void clear() {
        oneSet.clear();
    }

    public String getName() {
        return modelFamilyName;
    }

    public void setFileName(String fileName) {
        if (fileName == null || fileName.length() == 0) {
            fileName = FieldPlanePairSet.emptyValue;
        }
        this.modelFamilyName = fileName;
    }

    public String toString() {
        StringBuffer retStrBuf = new StringBuffer();
        for (FieldPlanePair n : oneSet) {
            if (retStrBuf.length() > 0) {
                retStrBuf.append("; ");
            }
            retStrBuf.append(n.toString());
        }
        return retStrBuf.toString();
    }

    public FieldPlanePairSet clone() {
        FieldPlanePairSet set = new FieldPlanePairSet(modelFamily);
        for (FieldPlanePair e : oneSet) {
            set.add(new FieldPlanePair(e.getFieldAbbrev(), e.getFieldLongName(), e
                    .getPlane(), e.isVisible(), e.getDisplayType()));
        }
        return set;
    }
}
