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
package com.raytheon.uf.common.pointdata;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * A message for querying pointdata
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 14, 2010            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@DynamicSerialize
public class PointDataRequestMessage implements IServerRequest {

    @DynamicSerializeElement
    private String pluginName;

    @DynamicSerializeElement
    private boolean allLevels;

    @DynamicSerializeElement
    private String levelParameter;

    @DynamicSerializeElement
    private double[] levelValue;

    @DynamicSerializeElement
    private String[] parameters;

    @DynamicSerializeElement
    private PointDataRequestMessageConstraint[] constraints;

    /**
     * @return the pluginName
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * @param pluginName
     *            the pluginName to set
     */
    public void setPluginName(String pluginName) {
        this.pluginName = pluginName;
    }

    /**
     * @return the allLevels
     */
    public boolean isAllLevels() {
        return allLevels;
    }

    /**
     * @param allLevels
     *            the allLevels to set
     */
    public void setAllLevels(boolean allLevels) {
        this.allLevels = allLevels;
    }

    /**
     * @return the levelParameter
     */
    public String getLevelParameter() {
        return levelParameter;
    }

    /**
     * @param levelParameter
     *            the levelParameter to set
     */
    public void setLevelParameter(String levelParameter) {
        this.levelParameter = levelParameter;
    }

    /**
     * @return the levelValue
     */
    public double[] getLevelValue() {
        return levelValue;
    }

    /**
     * @param levelValue
     *            the levelValue to set
     */
    public void setLevelValue(double[] levelValue) {
        this.levelValue = levelValue;
    }

    /**
     * @return the parameters
     */
    public String[] getParameters() {
        return parameters;
    }

    /**
     * @param parameters
     *            the parameters to set
     */
    public void setParameters(String[] parameters) {
        this.parameters = parameters;
    }

    /**
     * @return the constraints
     */
    public PointDataRequestMessageConstraint[] getConstraints() {
        return constraints;
    }

    /**
     * @param constraints
     *            the constraints to set
     */
    public void setConstraints(PointDataRequestMessageConstraint[] constraints) {
        this.constraints = constraints;
    }

}
