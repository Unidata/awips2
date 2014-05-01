/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

package com.raytheon.uf.viz.core.catalog;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.NoDataAvailableException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.ResourceType;

/**
 * 
 * Stores the contents of a entry that will be used to create a layer within
 * CAVE.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	11/27/2006  #7         brockwoo    Initial creation.
 *  12/08/2006  #103       brockwoo    Changed contour values to doubles.
 *  6/26/2008   1135       grichard    Added throws VizException in method.
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
public class LayerProperty {

    private static final String TIME_FIELD = "dataTime";

    private String entryDataType;

    private Map<String, RequestConstraint> entryQueryParameters;

    private ResourceType desiredProduct;

    private DataTime[] entryTimes;

    private DataTime[] selectedEntryTime;

    private int numberOfImages = 1;

    /**
     * The constructor will set the various values to defaults.
     */
    public LayerProperty() {
    }

    /**
     * Returns the desired output type.
     * 
     * @return The desired output format
     */
    public ResourceType getDesiredProduct() {
        return desiredProduct;
    }

    /**
     * Sets the layer to the desired product, (i.e. Image, Contour, etc.).
     * 
     * @param desiredProduct
     *            The desired output format
     */
    public void setDesiredProduct(ResourceType desiredProduct) {
        this.desiredProduct = desiredProduct;
    }

    /**
     * Returns the type of product this layer is.
     * 
     * @return A string with the product type
     */
    public String getEntryDataType() {
        return entryDataType;
    }

    /**
     * Sets the data type of this product (i.e. Satellite, Radar, etc).
     * 
     * @param entryDataType
     *            The product type
     */
    public void setEntryDataType(String entryDataType) {
        this.entryDataType = entryDataType;
        if ("Radar".equalsIgnoreCase(entryDataType)
                || "Satellite".equalsIgnoreCase(entryDataType)
                || "Grid".equalsIgnoreCase(entryDataType)) {
            // desiredProduct = "Image";
        } else if ("Observations".equalsIgnoreCase(entryDataType)) {
            // desiredProduct = "Plot";
        }
        // TODO fix?
        desiredProduct = ResourceType.PLAN_VIEW;
    }

    /**
     * Will return the parameters defined for the layer. A boolean is included
     * to indicate if the selected time should be included with the query.
     * 
     * @param includeTime
     *            A boolean to indicate if time should be returned
     * @return A hashmap containing the requested parameter information
     */
    public HashMap<String, RequestConstraint> getEntryQueryParameters(
            boolean includeTime) {
        HashMap<String, RequestConstraint> querySet = new HashMap<String, RequestConstraint>();
        querySet.putAll(entryQueryParameters);
        if (includeTime && this.selectedEntryTime != null) {
            RequestConstraint c = new RequestConstraint();
            c.setConstraintType(ConstraintType.IN);
            c.setConstraintValueList(recast(this.selectedEntryTime));
            querySet.put(TIME_FIELD, c);
        }
        return querySet;
    }

    /**
     * Sets the parameters for this layer. Will then request all available times
     * for the layer.
     * 
     * @param entryQueryParameters
     *            the query parameters
     */
    public void setEntryQueryParameters(
            Map<String, RequestConstraint> entryQueryParameters)
            throws VizException {
        setEntryQueryParameters(entryQueryParameters, true);
    }

    /**
     * Sets the parameters for this layer.
     * 
     * Optional: Can request all available times for the layer.
     * 
     * @param entryQueryParameters
     *            the query parameters
     * @param updateTimes
     *            whether to fetch the available times
     */
    public void setEntryQueryParameters(
            Map<String, RequestConstraint> entryQueryParameters,
            boolean updateTimes) throws VizException {
        setEntryQueryParameters(entryQueryParameters, updateTimes, null);
    }

    public void setEntryQueryParameters(
            Map<String, RequestConstraint> entryQueryParameters,
            boolean updateTimes, BinOffset binOffset) throws VizException {
        this.entryQueryParameters = entryQueryParameters;

        if (updateTimes)
            requestEntryTimes(binOffset);
    }

    /**
     * Gets the entry times.
     * 
     * @return
     */
    public DataTime[] getEntryTimes() {
        return entryTimes;
    }

    /**
     * Sets the entry times.
     * 
     * @param entryTimes
     */
    public void setEntryTimes(DataTime[] entryTimes) {
        this.entryTimes = entryTimes;
    }

    /**
     * Returns the selected time for the current layer.
     * 
     * @return The selected time.
     */
    public DataTime[] getSelectedEntryTime() {
        return selectedEntryTime;
    }

    /**
     * Sets the selected valid time for the current layer.
     * 
     * @param selectedEntryTime
     *            A list of valid times (forecast hours for grid data)
     */
    public void setSelectedEntryTimes(DataTime[] selectedEntryTime) {
        this.selectedEntryTime = selectedEntryTime;
    }

    /**
     * Returns all the valid times for the current layer.
     * 
     * @return A string of the valid times (forecast hours for grid data)
     */
    public DataTime[] getAllEntryTimes() {
        return entryTimes;
    }

    public int getNumberOfImages() {
        return numberOfImages;
    }

    public void setNumberOfImages(int numberOfImages) {
        this.numberOfImages = numberOfImages;
    }

    private void requestEntryTimes(BinOffset binOffset) throws VizException {
        this.entryTimes = DataCubeContainer.performTimeQuery(
                entryQueryParameters, false, binOffset);

        if (this.entryTimes == null) {
            throw new NoDataAvailableException();
        }

        /*
         * this.entryTimes = new DataTime[results.length]; for (int i = 0; i <
         * results.length; i++) { entryTimes[i] = new DataTime(results[i]); }
         */

        Arrays.sort(entryTimes);
        if (selectedEntryTime == null && entryTimes.length > 0)
            selectedEntryTime = new DataTime[] { entryTimes[0] };
    }

    private String[] recast(Object[] objects) {
        if (objects == null)
            return new String[0];

        String[] stringArr = new String[objects.length];
        for (int i = 0; i < objects.length; i++) {
            if (objects[i] != null)
                stringArr[i] = objects[i].toString();
        }
        return stringArr;
    }

}
