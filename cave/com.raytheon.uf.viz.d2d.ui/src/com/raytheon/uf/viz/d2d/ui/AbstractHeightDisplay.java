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
package com.raytheon.uf.viz.d2d.ui;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource.ResourceStatus;
import com.raytheon.viz.core.slice.request.HeightScale;

/**
 * Abstract display for displaying data over multiple heights up through the
 * atmosphere
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 12, 2007            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public abstract class AbstractHeightDisplay extends AbstractNonMapDisplay {

    public AbstractHeightDisplay() {
        this(new PixelExtent(0, 1000, 0, 1000), null);
    }

    public AbstractHeightDisplay(PixelExtent aPixelExtent,
            IDescriptor descriptor) {
        super(aPixelExtent, descriptor);
    }

    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        super.paint(target, paintProps);
        for (ResourcePair rp : descriptor.getResourceList()) {
            if (rp.getResource() != null
                    && rp.getResource().getStatus() == ResourceStatus.NEW) {
                try {
                    rp.getResource().init(target);
                } catch (Throwable e) {
                    rp.getProperties().setVisible(false);
                    throw new VizException("Initialization error: "
                            + e.getMessage()
                            + ":: The resource has been disabled.", e);
                }
            }
        }
    }

    public abstract void setHeightScale(HeightScale aScale);

}
