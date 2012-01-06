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

package com.raytheon.uf.viz.core.rsc;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory.ResourceOrder;

/**
 * Defines the properties inherent to a resource
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------	----------	-----------	--------------------------
 *    7/1/06                    chammack    Initial Creation.
 *    3/5/08			2032	jsanchez	Initialized pdProps.
 * 
 * </pre>
 * 
 * @author chammack
 * 
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ResourceProperties implements ISerializableObject {

    /** is the layer isVisible */
    @XmlAttribute
    private boolean isVisible = true;

    /** Is hover turned on? */
    @XmlAttribute
    private boolean isHoverOn;

    /** Is the resource a map layer? */
    @XmlAttribute
    private boolean isMapLayer;

    /** Is the layer blinking? */
    @XmlAttribute
    private boolean isBlinking;

    /** Progressive Disclosure properties */
    @XmlElement
    private ProgressiveDisclosureProperties pdProps = new ProgressiveDisclosureProperties();

    /** Is the resource a system resource (does not show up in legend)? */
    @XmlAttribute
    private boolean isSystemResource;

    /** Default rendering order ID */
    @XmlAttribute
    private String renderingOrderId;

    /** Current rendering order */
    private int renderingOrder = RenderingOrderFactory.ResourceOrder.NOT_SET.value;

    private AbstractVizResource<?, ?> resource;

    public void setResource(AbstractVizResource<?, ?> rsc) {
        this.resource = rsc;
        if (rsc != null) {
            rsc.setProperties(this);
        }
    }

    private void notifyResource() {
        if (resource != null) {
            resource.propertiesChanged(this);
        }
    }

    /**
     * @return the isVisible
     */
    public boolean isVisible() {
        return isVisible;
    }

    /**
     * @param isVisible
     *            the isVisible to set
     */
    public void setVisible(boolean visible) {
        this.isVisible = visible;
        notifyResource();
    }

    /**
     * @return the isHoverOn
     */
    public boolean isHoverOn() {
        return isHoverOn;
    }

    /**
     * @param isHoverOn
     *            the isHoverOn to set
     */
    public void setHoverOn(boolean isHoverOn) {
        this.isHoverOn = isHoverOn;
        notifyResource();
    }

    /**
     * @return the isMapLayer
     */
    public boolean isMapLayer() {
        return isMapLayer;
    }

    /**
     * @param isMapLayer
     *            the isMapLayer to set
     */
    public void setMapLayer(boolean isMapLayer) {
        this.isMapLayer = isMapLayer;
    }

    /**
     * @return the pdProps
     */
    public ProgressiveDisclosureProperties getPdProps() {
        return pdProps;
    }

    /**
     * @param pdProps
     *            the pdProps to set
     */
    public void setPdProps(ProgressiveDisclosureProperties pdProps) {
        this.pdProps = pdProps;
    }

    /**
     * Returns the logical and of isVisible() and pdProps.isDisclosed()
     * 
     * @return
     */
    public boolean isDisplayable(int displayWidth) {
        return (isVisible && ((pdProps == null) || pdProps
                .isDisclosed(displayWidth)));
    }

    /**
     * @return the isBlinking
     */
    public boolean isBlinking() {
        return isBlinking;
    }

    /**
     * @param isBlinking
     *            the isBlinking to set
     */
    public void setBlinking(boolean isBlinking) {
        this.isBlinking = isBlinking;
        notifyResource();
    }

    /**
     * @return the isSystemResource
     */
    public boolean isSystemResource() {
        return isSystemResource;
    }

    /**
     * @param isSystemResource
     *            the isSystemResource to set
     */
    public void setSystemResource(boolean isSystemResource) {
        this.isSystemResource = isSystemResource;
    }

    public void setRenderingOrder(ResourceOrder order) {
        setRenderingOrderId(order.id);
        setRenderingOrder(order.value);
    }

    public String getRenderingOrderId() {
        return renderingOrderId;
    }

    public void setRenderingOrderId(String renderingOrderId) {
        this.renderingOrderId = renderingOrderId;
    }

    public int getRenderingOrder() {
        return renderingOrder;
    }

    public void setRenderingOrder(int renderingOrder) {
        this.renderingOrder = renderingOrder;
    }

    @Override
    public String toString() {
        StringBuffer s = new StringBuffer("[");
        s.append(isBlinking() ? "Blinking, " : "");
        s.append(isHoverOn() ? "HoverOn, " : "");
        s.append(isMapLayer() ? "MapLayer, " : "");
        s.append(isSystemResource() ? "System, " : "");
        s.append(isVisible() ? "Visible, " : "");
        s.append(getRenderingOrder() + ", ");
        s.append(getRenderingOrderId());
        s.append("]");
        return s.toString();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        } else if (obj == null || obj instanceof ResourceProperties == false) {
            return false;
        }

        ResourceProperties props = (ResourceProperties) obj;
        // blinking and visible don't matter when determining if they are equal
        // since they change
        return (this.isSystemResource == props.isSystemResource && this.isMapLayer == props.isMapLayer);
    }

    @Override
    public ResourceProperties clone() {
        ResourceProperties rp = new ResourceProperties();
        rp.isBlinking = isBlinking;
        rp.isHoverOn = isHoverOn;
        rp.isMapLayer = isMapLayer;
        rp.isSystemResource = isSystemResource;
        rp.isVisible = isVisible;
        rp.pdProps = pdProps;
        rp.renderingOrder = renderingOrder;
        rp.renderingOrderId = renderingOrderId;
        return rp;
    }
}
