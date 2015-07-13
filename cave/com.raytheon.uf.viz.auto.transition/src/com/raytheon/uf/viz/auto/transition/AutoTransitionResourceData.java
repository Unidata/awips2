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
package com.raytheon.uf.viz.auto.transition;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.core.rsc.BlendedResourceData;

/**
 * 
 * Serializable data for {@link AutoTransitionResource}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 09, 2015  4633     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class AutoTransitionResourceData extends BlendedResourceData {

    @XmlElement
    private int controlIndex = 1;

    @XmlElement
    private double thresholdValue = 30;

    @XmlElement
    private boolean automaticSelection = true;

    public AutoTransitionResourceData() {

    }

    public AutoTransitionResourceData(BlendedResourceData blended) {
        this.resourceList = new ResourceList();
        for (ResourcePair pair : blended.getResourceList()) {
            ResourcePair newPair = new ResourcePair();
            newPair.setResourceData(pair.getResourceData());
            newPair.setLoadProperties(pair.getLoadProperties());
            newPair.setProperties(pair.getProperties());
            this.resourceList.add(pair);
        }
    }

    @Override
    public AutoTransitionResource construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        return new AutoTransitionResource(this, loadProperties);
    }

    public int getControlIndex() {
        return controlIndex;
    }

    public void setControlIndex(int controlIndex) {
        this.controlIndex = controlIndex;
    }

    public double getThresholdValue() {
        return thresholdValue;
    }

    public void setThresholdValue(double thresholdValue) {
        this.thresholdValue = thresholdValue;
    }

    public boolean isAutomaticSelection() {
        return automaticSelection;
    }

    public void setAutomaticSelection(boolean automaticSelection) {
        this.automaticSelection = automaticSelection;
    }

}
