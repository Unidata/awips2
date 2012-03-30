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
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IColorMapParametersListener;

/**
 * Capability for changing a colormap and/or its parameters
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 12, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class ColorMapCapability extends AbstractCapability implements
        IColorMapParametersListener {

    @XmlElement
    private ColorMapParameters colorMapParameters;

    /**
     * @return the colorMapParameters
     */
    public ColorMapParameters getColorMapParameters() {
        return colorMapParameters;
    }

    /**
     * @param colorMapParameters
     *            the colorMapParameters to set
     */
    public void setColorMapParameters(ColorMapParameters colorMapParameters) {
        setColorMapParameters(colorMapParameters, true);
    }

    /**
     * @param colorMapParameters
     *            the colorMapParameters to set
     * @param notify
     *            notify the resource of the change
     */
    public void setColorMapParameters(ColorMapParameters colorMapParameters,
            boolean notify) {
        if (this.colorMapParameters != colorMapParameters) {
            if (this.colorMapParameters != null) {
                this.colorMapParameters.removeListener(this);
            }
            this.colorMapParameters = colorMapParameters;
            if (notify) {
                capabilityChanged();
            }
        }

        if (this.colorMapParameters != null) {
            this.colorMapParameters.addListener(this);
        }
    }

    public void notifyResources() {
        capabilityChanged();
    }

    @Override
    public AbstractCapability clone() {
        ColorMapCapability cmc = new ColorMapCapability();
        cmc.colorMapParameters = colorMapParameters.clone();
        return cmc;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.core.drawables.IColorMapParametersListener#
     * colorMapChanged()
     */
    @Override
    public void colorMapChanged() {
        notifyResources();
    }

}
