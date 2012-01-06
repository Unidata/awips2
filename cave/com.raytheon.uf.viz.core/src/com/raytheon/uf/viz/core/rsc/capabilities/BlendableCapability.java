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
package com.raytheon.uf.viz.core.rsc.capabilities;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.ResourceList;

/**
 * Adds the capability to blend between two resources.
 * 
 * Resource A will have k alpha and Resource B will have 1.0-k alpha.
 * 
 * Also has the capability to toggle between which resource has k, and which has
 * 1.0-k.
 * 
 * The alpha is stored as an integer fraction (i / BLEND_MAX).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 16, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class BlendableCapability extends AbstractCapability {

    public static final int BLEND_MAX = 15;

    @XmlAttribute
    protected int resourceIndex;

    @XmlAttribute
    protected int alphaStep;

    public BlendableCapability() {
        super();
        this.resourceIndex = 0;
        this.alphaStep = 8;
    }

    /**
     * @return the alpha step
     */
    public int getAlphaStep() {
        return alphaStep;
    }

    /**
     * @param alphaStep
     *            the alpha step to set
     */
    public void setAlphaStep(int alphaStep) {
        if (this.alphaStep != alphaStep) {
            ResourceList list = getResourceList();
            int otherIdx = 1 - resourceIndex;
            if (this.alphaStep == 0 && alphaStep > this.alphaStep) {
                // We are increasing
                if (otherIdx >= 0 && otherIdx < list.size()) {
                    getResourceList().get(otherIdx).getProperties().setVisible(
                            true);
                }
            } else if (this.alphaStep == BLEND_MAX
                    && alphaStep < this.alphaStep) {
                if ((resourceIndex) >= 0 && (resourceIndex) < list.size()) {
                    getResourceList().get(resourceIndex).getProperties()
                            .setVisible(true);
                }
            }
            this.alphaStep = alphaStep;
            if (this.alphaStep == 0) {
                // We have decreased to zero
                if ((otherIdx) >= 0 && (otherIdx) < list.size()) {
                    getResourceList().get(otherIdx).getProperties().setVisible(
                            false);
                }
            } else if (this.alphaStep == BLEND_MAX) {
                // We have decreased to zero
                if ((resourceIndex) >= 0 && (resourceIndex) < list.size()) {
                    getResourceList().get(resourceIndex).getProperties()
                            .setVisible(false);
                }
            }
            this.capabilityChanged();
        }
    }

    /**
     * @return the resourceIndex
     */
    public int getResourceIndex() {
        return resourceIndex;
    }

    /**
     * Perform a switch of which index is fully k alpha, and 1.0-k alpha.
     */
    public void toggle() {
        alphaStep = BLEND_MAX;
        ResourceList list = getResourceList();
        if (resourceIndex >= 0 && resourceIndex < list.size()) {
            getResourceList().get(resourceIndex).getProperties().setVisible(
                    true);
        }
        resourceIndex = 1 - resourceIndex;
        if (resourceIndex >= 0 && resourceIndex < list.size()) {
            getResourceList().get(resourceIndex).getProperties().setVisible(
                    false);
        }
    }

    public void toggle(ResourcePair pair) {
        ResourceList list = getResourceList();
        if (resourceIndex < list.size()
                && list.get(resourceIndex) == pair
                && pair.getProperties().isVisible()) {
            resourceIndex = 1 - resourceIndex;
        }
        toggle();
    }

    /**
     * Give the index to hide, show other and hide this one
     * 
     * @param indexToHide
     */
    public void toggle(int indexToHide) {
        if (resourceIndex != indexToHide) {
            toggle();
        } else {
            setAlphaStep(BLEND_MAX);
        }
    }

    public ResourceList getResourceList() {
        if (resourceData instanceof IResourceGroup) {
            return ((IResourceGroup) resourceData).getResourceList();
        } else {
            throw new IllegalStateException(
                    "The resource data of a resource with the BlendableCapability must implement IResourceList");
        }
    }

    @Override
    public AbstractCapability clone() {
        BlendableCapability bc = new BlendableCapability();
        bc.alphaStep = alphaStep;
        bc.resourceIndex = resourceIndex;
        return bc;
    }

}
