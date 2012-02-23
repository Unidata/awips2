package gov.noaa.nws.ncep.viz.overlays.resources;

import gov.noaa.nws.ncep.viz.common.RGBColorAdapter;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NC-PgenXmlOverlayResourceData")
public class PgenXmlOverlayResourceData extends AbstractNatlCntrsResourceData 
                implements INatlCntrsResourceData { 

    @XmlElement(required = true)
    private String pgenProductName;

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB color = new RGB(155, 155, 155);

    @XmlElement
    private int lineWidth = 1;

    @XmlElement
    private LineStyle lineStyle = LineStyle.SOLID;

    public PgenXmlOverlayResourceData() {
        super();
        this.nameGenerator = new AbstractNameGenerator() {

            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                return pgenProductName;
            }

        };
    }

    @SuppressWarnings("unchecked")
	@Override
    public PgenXmlOverlayResource constructResource(LoadProperties loadProperties,
        IDescriptor descriptor) throws VizException {
    	return new PgenXmlOverlayResource(this, loadProperties);
    }

    public String getPgenProductName() {
    	return pgenProductName;
    }

    public void setPgenProductName(String pgenProductName) {
    	this.pgenProductName = pgenProductName;
    }

    public RGB getColor() {
    	return color;
    }

    public void setColor(RGB _color) {
            this.color = _color;
            this.legendColor = color;
    }

    public int getLineWidth() {
            return lineWidth;
    }

    public void setLineWidth(int lineWidth) {
            this.lineWidth = lineWidth;
    }

    public LineStyle getLineStyle() {
            return lineStyle;
    }
    public void setLineStyle(LineStyle lineStyle) {
        this.lineStyle = lineStyle;
    }

    @Override
    public boolean equals(Object obj) {
		if (!super.equals(obj)) {
			return false;
		}
        PgenXmlOverlayResourceData other = (PgenXmlOverlayResourceData) obj;

    	if (this.pgenProductName != null && other.pgenProductName == null) {
    		return false;
    	} else if (this.pgenProductName == null && other.pgenProductName != null) {
    		return false;
    	} else if (this.pgenProductName != null
    			&& this.pgenProductName.equals(other.pgenProductName) == false) {
    		return false;
    	}

    	return true;
    }

    @Override
    public String toString() {
    	return pgenProductName;
    }

}
