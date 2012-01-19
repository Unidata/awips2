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

import com.raytheon.uf.viz.core.drawables.ResourcePair;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 1, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class BlendedCapability extends AbstractCapability {

    protected transient ResourcePair blendableResource;

    protected transient int resourceIndex;

    public BlendedCapability() {

    }

    public ResourcePair getBlendableResource() {
        return blendableResource;
    }

    public void setBlendableResource(ResourcePair blendableResource) {
        if (blendableResource.getResource().hasCapability(
                BlendableCapability.class) == false) {
            throw new IllegalArgumentException(
                    "Resources with the BlendableCapability can only be set as the blendedResource on the BlendedCapability");
        }
        this.blendableResource = blendableResource;
    }

    public int getResourceIndex() {
        return resourceIndex;
    }

    public void setResourceIndex(int resourceIndex) {
        this.resourceIndex = resourceIndex;
    }

    @Override
    public AbstractCapability clone() {
        return new BlendedCapability();
    }
}
