package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

import gov.noaa.gsd.viz.ensemble.util.RequestableResourceMetadata;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.rsc.GridLoadProperties;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.viz.grid.rsc.GridResourceData;
import com.raytheon.viz.ui.BundleLoader;
import com.raytheon.viz.ui.BundleLoader.BundleInfoType;

/**
 * This is the data model class which represents a model family.
 * 
 * The model family has a definition (see <code>ModelFamilyDefinitions</code>, a
 * element set, and a variables map for resolution of variable components like
 * frame count, model source, and total precipitation.
 * 
 * To understand more about model families, raw model families can be found in
 * the <code>com.raytheon.viz.volumebrowser</code> under the directory
 * <code>localization/bundles/volume</code>. There you will find top-level
 * families like <code>WinterFamily.xml</code> and its related inner family
 * <code>ModelFamilyD.xml</code>.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 10, 2015  12302      polster     Initial creation
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */

public class ModelFamily {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ModelFamily.class);

    protected static final String bundlesLocation = "bundles/volume/";

    protected Bundle bundle = null;

    protected File currFile = null;

    protected Map<String, String> variablesMap = null;

    protected FieldPlanePairSet fieldPlanePairs = null;

    protected ModelFamilyDefinitions currFamilyDefinition = null;

    /**
     * Constructor.
     * 
     * @param givenFamilyDef
     *            the definition of the family
     * @throws IOException
     * @throws VizException
     */
    public ModelFamily(ModelFamilyDefinitions givenFamilyDef)
            throws IOException, VizException {

        currFamilyDefinition = givenFamilyDef;

        String filename = currFamilyDefinition.getFamilyFileName();
        if (filename == null) {
            filename = FieldPlanePairSet.emptyValue;
        }

        String xmlFileName = currFamilyDefinition.getFamilyFileName();
        if (xmlFileName == null || xmlFileName.length() == 0) {
            throw new IOException("Model family file name cannot be null.");
        }

        currFile = PathManagerFactory.getPathManager().getStaticFile(
                xmlFileName);
        if (currFile == null) {
            throw new IOException("No model family file found: " + xmlFileName);
        }

        variablesMap = new HashMap<String, String>();
        variablesMap = findVariables(currFile);

        bundle = BundleLoader.getBundle(xmlFileName, variablesMap,
                BundleInfoType.FILE_LOCATION);

        fieldPlanePairs = extractFieldPlanePairs();
        if (fieldPlanePairs == null || fieldPlanePairs.size() == 0) {
            throw new VizException(
                    "Invalid model family: There are no field/plane pairs.");
        }

    }

    /**
     * Copy constructor
     * 
     * @param givenFamily
     *            the family to copy from
     */
    public ModelFamily(ModelFamily givenFamily) {

        bundle = givenFamily.bundle;

        currFile = givenFamily.currFile;

        variablesMap = givenFamily.variablesMap;

        fieldPlanePairs = givenFamily.fieldPlanePairs;

        currFamilyDefinition = givenFamily.currFamilyDefinition;

    }

    /**
     * Creates a bundle from the raw model family XML file and then extracts the
     * field/plane pairs.
     * 
     * @return the set of field/plane pairs in the convenience class
     *         <code>ElementSet</code>
     */
    private FieldPlanePairSet extractFieldPlanePairs() {

        String fieldAbbrev = null;
        String fieldFullName = null;
        String plane = null;
        if (bundle == null)
            return null;
        else {
            fieldPlanePairs = new FieldPlanePairSet(this);
            fieldPlanePairs.setFileName(currFamilyDefinition
                    .getFamilyFileName());

            AbstractRenderableDisplay[] displays = bundle.getDisplays();
            for (AbstractRenderableDisplay display : displays) {
                ResourceList rscList = display.getDescriptor()
                        .getResourceList();
                for (int i = 0; i < rscList.size(); i++) {
                    ResourcePair rp = rscList.get(i);
                    if (rp != null && rp.getResourceData() != null) {
                        if (rp.getResourceData() instanceof AbstractRequestableResourceData) {

                            DisplayType displayType = getDisplayType(rp
                                    .getLoadProperties());

                            ResourceProperties rscProps = rp.getProperties();
                            AbstractRequestableResourceData ard = (AbstractRequestableResourceData) rp
                                    .getResourceData();
                            RequestableResourceMetadata rrd = new RequestableResourceMetadata(
                                    ard);
                            fieldAbbrev = rrd.getFieldAbbrev();
                            fieldFullName = rrd.getFieldFullName();
                            plane = rrd.getPlane();
                            if (fieldAbbrev != null && plane != null) {
                                fieldPlanePairs.add(new FieldPlanePair(
                                        fieldAbbrev, fieldFullName, plane,
                                        rscProps.isVisible(), displayType));
                            }
                        }
                    }
                }
            }
        }
        return fieldPlanePairs;
    }

    /**
     * Given a <code>LoadProperties</code> instance, return the display type.
     * 
     * @param lp
     *            a <code>LoadProperties</code> instance
     * @return a <code>DisplayType</code>
     */
    public static DisplayType getDisplayType(LoadProperties lp) {
        DisplayType displayType = DisplayType.CONTOUR;
        if (lp instanceof GridLoadProperties) {
            GridLoadProperties props = (GridLoadProperties) lp;
            displayType = props.getDisplayType();
        }
        return displayType;
    }

    /**
     * Extract the description from the beginning of the model family file which
     * contains the AWIPS1 virtual field equivalent of the model family.
     * 
     * TODO: This method is currently not used but may be asked for in the
     * future.
     */
    private String extractDescription() throws IOException {
        String descr = null;
        StringTokenizer stok = null;
        BufferedReader br = new BufferedReader(new FileReader(currFile));
        String thisLine = null;
        boolean descriptionLineFound = false;
        while (!descriptionLineFound && (thisLine = br.readLine()) != null) {
            stok = new StringTokenizer(thisLine, "|");
            if (stok.countTokens() > 4) {
                descriptionLineFound = true;
                stok.nextToken();
                stok.nextToken();
                stok.nextToken();
                descr = stok.nextToken();
            }
        }
        if (br != null) {
            br.close();
        }
        return descr;
    }

    /**
     * Given a raw model family file, extract the variable map
     * 
     * @param currFile
     *            the raw model family XML file.
     * @return tne variables map of name/value pairs
     * @throws IOException
     */
    private Map<String, String> findVariables(File currFile) throws IOException {

        /*
         * Search for string pattern: ${variable}
         */
        Pattern regexp = Pattern.compile("\\$\\{.*\\}");
        Matcher matcher = regexp.matcher("");
        BufferedReader br = new BufferedReader(new FileReader(currFile));
        String foundVariable = null;
        String thisLine = null;
        while ((thisLine = br.readLine()) != null) {
            matcher.reset(thisLine);
            if (matcher.find()) {
                foundVariable = thisLine.substring(matcher.start() + 2,
                        matcher.end() - 1);
                // TODO: Currently hard-coding the frameCount to 10
                if (foundVariable.equals("frameCount")) {
                    variablesMap.put(foundVariable, "10");
                } else {
                    variablesMap.put(foundVariable, foundVariable);
                }
            }
        }
        if (br != null) {
            br.close();
        }

        return variablesMap;
    }

    /**
     * Returns all the field/plane pairs associated with this model family.
     * 
     * @return the set of field/plane pairs.
     */
    public FieldPlanePairSet getFieldPlanePairs() {
        return fieldPlanePairs;
    }

    /**
     * Gets the model family definition for this model family.
     * 
     * @return the model family definition
     */
    public ModelFamilyDefinitions getCurrFamilyDefinition() {
        return currFamilyDefinition;
    }

    /**
     * Given a field/plane pair, return whether the model family defaults the
     * resolved resource to visible or not.
     * 
     * @param fieldPlanePair
     *            the Element contained within this model family
     * @return whether the default state for this model family pair is visible
     *         or not
     */
    public boolean isResourceVisibile(FieldPlanePair fieldPlanePair) {
        boolean isVisibile = false;
        if (fieldPlanePair != null) {
            isVisibile = isResourceVisibile(fieldPlanePair.getFieldAbbrev(),
                    fieldPlanePair.getPlane());
        }
        return isVisibile;
    }

    /**
     * Given a field name abbreviation and a plane, parse this model family's
     * bundle for the state of the visiblity flag.
     * 
     * @param fieldAbbrevArg
     *            the abbreviated field name of the field/plane pair
     * @param planeArg
     *            the plane name of the field/plane pair
     * @return whether the default state for this model family pair is visible
     *         or not
     */
    public boolean isResourceVisibile(String fieldAbbrevArg, String planeArg) {
        boolean isVisible = false;
        String targetField = null;
        String targetPlane = null;

        if (bundle != null) {
            AbstractRenderableDisplay[] displays = bundle.getDisplays();
            for (AbstractRenderableDisplay display : displays) {
                ResourceList rscList = display.getDescriptor()
                        .getResourceList();
                for (int i = 0; i < rscList.size(); i++) {
                    ResourcePair rp = rscList.get(i);
                    if (rp != null && rp.getResourceData() != null) {
                        if (rp.getResourceData() instanceof GridResourceData) {
                            ResourceProperties rscProps = rp.getProperties();
                            GridResourceData grd = (GridResourceData) rp
                                    .getResourceData();
                            HashMap<String, RequestConstraint> map = grd
                                    .getMetadataMap();
                            Set<String> keys = map.keySet();
                            Iterator<String> iter = keys.iterator();
                            String key = null;
                            while (iter.hasNext()) {
                                key = iter.next();
                                RequestConstraint rc = map.get(key);
                                if (key.equals(GridConstants.PARAMETER_ABBREVIATION)) {
                                    targetField = rc.getConstraintValue();
                                } else if (key
                                        .equals(GridConstants.MASTER_LEVEL_NAME)) {
                                    targetPlane = rc.getConstraintValue();
                                }
                            }
                            if (targetField != null && targetPlane != null) {
                                if (fieldAbbrevArg.equals(targetField)
                                        && (planeArg.equals(targetPlane))) {
                                    isVisible = rscProps.isVisible();
                                }
                            }
                        }
                    }
                }
            }
        }
        return isVisible;
    }

}
