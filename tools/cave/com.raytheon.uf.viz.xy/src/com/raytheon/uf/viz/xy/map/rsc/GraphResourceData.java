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
package com.raytheon.uf.viz.xy.map.rsc;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;

/**
 * Resource Data for the graph resource. All graphs will be constructed from
 * IGraphableResources so no need to persist existing graphs, only a graph name
 * is necessary for displaying on the legend
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 29, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class GraphResourceData extends AbstractResourceData {

    public enum OverlayMode {
        VERTICAL, OVERLAY;
    }

    @XmlAttribute
    protected OverlayMode overlayMode;

    /** The name of the graph for legend display */
    @XmlAttribute
    protected String name;

    public GraphResourceData() {

    }

    public GraphResourceData(String name) {
        super();
        this.name = name;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon
     * .uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public GraphResource construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        GraphResource rsc = new GraphResource(this, loadProperties);
        ColorableCapability cap = new ColorableCapability();
        cap.setColor(new RGB(155, 155, 155));
        rsc.getCapabilities().addCapability(cap);
        return rsc;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!(obj instanceof GraphResourceData))
            return false;
        GraphResourceData other = (GraphResourceData) obj;
        if (name == null) {
            if (other.name != null)
                return false;
        } else if (!name.equals(other.name))
            return false;
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractResourceData#getNameGenerator()
     */
    @Override
    public AbstractNameGenerator getNameGenerator() {
        if (this.nameGenerator == null) {
            this.nameGenerator = new AbstractNameGenerator() {
                @Override
                public String getName(AbstractVizResource<?, ?> resource) {
                    return GraphResourceData.this.name;
                }
            };
        }
        return super.getNameGenerator();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#update(java.lang.Object
     * )
     */
    @Override
    public void update(Object updateData) {

    }

    public String getName() {
        return name;
    }

    /**
     * @return the overlayMode
     */
    public OverlayMode getOverlayMode() {
        return overlayMode;
    }

    /**
     * @param overlayMode
     *            the overlayMode to set
     */
    public void setOverlayMode(OverlayMode overlayMode) {
        this.overlayMode = overlayMode;
    }

}
