package gov.noaa.gsd.viz.ensemble.util;

import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.derivparam.inv.AbstractInventory;
import com.raytheon.uf.common.derivparam.library.DerivedParameterGenerator;
import com.raytheon.uf.common.parameter.lookup.ParameterLookup;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.viz.core.rsc.BestResResourceData;
import com.raytheon.viz.grid.rsc.GridResourceData;

/**
 * This is the data model class which represents a model family.
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

public class RequestableResourceMetadata {

    private AbstractRequestableResourceData ard = null;

    private String fieldAbbrev = null;

    private String fieldFullName = null;

    private String plane = null;

    public RequestableResourceMetadata(AbstractRequestableResourceData ardata) {
        ard = ardata;
        extractMetadata(ard.getMetadataMap());
    }

    private void extractMetadata(Map<String, RequestConstraint> map) {

        String planeMasterLevel = null;
        String planeLevelOne = null;
        String planeLevelTwo = null;

        Set<String> keys = map.keySet();
        Iterator<String> iter = keys.iterator();
        String key = null;
        fieldAbbrev = null;
        fieldFullName = null;
        planeMasterLevel = null;
        planeLevelOne = null;
        planeLevelTwo = null;
        while (iter.hasNext()) {
            key = iter.next();
            RequestConstraint rc = map.get(key);
            if (key.equals(GridConstants.PARAMETER_ABBREVIATION)) {
                fieldAbbrev = rc.getConstraintValue();
                fieldFullName = getFullParameterName(ard, fieldAbbrev);
            } else if (key.equals(GridConstants.MASTER_LEVEL_NAME)) {
                planeMasterLevel = rc.getConstraintValue();
            } else if (key.equals(GridConstants.LEVEL_ONE)) {
                planeLevelOne = rc.getConstraintValue();
            } else if (key.equals(GridConstants.LEVEL_TWO)) {
                planeLevelTwo = rc.getConstraintValue();
            }
        }
        double levelTwo = 0;
        if (planeLevelOne != null && planeLevelTwo != null
                && planeMasterLevel != null) {
            planeLevelOne = removeDecimal(planeLevelOne);
            planeLevelTwo = removeDecimal(planeLevelTwo);
            try {
                levelTwo = Double.parseDouble(planeLevelTwo);
            } catch (NumberFormatException nfe) {
                levelTwo = Level.INVALID_VALUE;
            }
            if (levelTwo == Level.INVALID_VALUE) {
                if (planeLevelOne.equals("0")) {
                    plane = planeMasterLevel;
                } else {
                    plane = planeLevelOne + planeMasterLevel;
                }
            } else {
                plane = planeLevelOne + planeMasterLevel + "-" + planeLevelTwo
                        + planeMasterLevel;
            }
            if (fieldFullName == null) {
                fieldFullName = "";
            }
        }

    }

    /**
     * Given an parameter abbreviation, return the full parameter name. If this
     * method cannot find a full parameter name based on the given abbreviation,
     * then it returns the abbrebviation.
     */
    private String getFullParameterName(String abbrev) {

        String field = null;
        if (abbrev != null) {
            if (ParameterLookup.getInstance().getParameter(abbrev) != null) {
                field = ParameterLookup.getInstance().getParameter(abbrev)
                        .getName();
            }
            if (field == null) {
                AbstractInventory inventory = (AbstractInventory) DataCubeContainer
                        .getInventory("grid");
                field = inventory.getParameterName("NAM40", abbrev);
            }
            if (field == null) {
                if (DerivedParameterGenerator.getDerParLibrary().get(abbrev) != null)
                    field = DerivedParameterGenerator.getDerParLibrary()
                            .get(abbrev).getName();
            }
            if (field == null) {
                field = abbrev;
            }
        }
        return field;
    }

    private String getFullParameterName(AbstractRequestableResourceData ard,
            String abbrev) {

        String field = null;
        if (ard instanceof GridResourceData) {
            field = getFullParameterName(abbrev);
        } else if (ard instanceof BestResResourceData) {
            /*
             * TODO: Currently we either find a comma-separated list in the
             * abbreviation (e.g. "msl-P, msl-P2") or we just return the
             * abbreviation itself as the parameter name.
             */
            StringTokenizer tokenizer = new StringTokenizer(abbrev, ",");
            if (tokenizer == null || tokenizer.countTokens() == 1) {
                field = abbrev;
            } else {
                String nextAbbrev = null;
                String fullParameterName = null;
                while (tokenizer.hasMoreElements()) {
                    nextAbbrev = tokenizer.nextToken();
                    if (nextAbbrev == null) {
                        field = fullParameterName;
                    }
                    fullParameterName = getFullParameterName(nextAbbrev);
                    if (field == null) {
                        field = fullParameterName;
                    } else {
                        field = field.concat(", " + field);
                    }
                }
            }
        }
        return field;
    }

    /**
     * If the string argument ends with a decimal and a zero (".0") or the
     * string ends with just a decimal point then return only the value before
     * the decimal point.
     * 
     * @param numStr
     * @return
     */
    private String removeDecimal(String numStr) {

        String cleanStr = null;
        final String pointZero = ".0";
        final String point = ".";

        /* e.g. convert 500.0 to 500 */
        if (numStr.endsWith(pointZero)) {
            cleanStr = numStr.substring(0, numStr.indexOf(pointZero));
        }
        /* e.g. convert 500. to 500 */
        else if (numStr.endsWith(point)) {
            cleanStr = numStr.substring(0, numStr.indexOf(point));
        } else {
            cleanStr = numStr;
        }
        return cleanStr;

    }

    public String getPlane() {
        return plane;
    }

    public String getFieldAbbrev() {
        return fieldAbbrev;
    }

    public String getFieldFullName() {
        return fieldFullName;
    }

}
