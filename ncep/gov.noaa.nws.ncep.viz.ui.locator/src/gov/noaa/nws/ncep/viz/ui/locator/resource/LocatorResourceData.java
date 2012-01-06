package gov.noaa.nws.ncep.viz.ui.locator.resource;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.graphics.RGB;

import gov.noaa.nws.ncep.viz.common.RGBColorAdapter;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NC-LocatorResourceData")
public class LocatorResourceData  extends AbstractNatlCntrsResourceData {
	
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB color = new RGB(0, 255, 0);
    
	public RGB getColor() {
		return color;
	}

	public void setColor(RGB _color) {
		color = _color; 
		this.legendColor = color;
	}

	public LocatorResourceData() {
		super();
		
		setResourceName(new ResourceName("OVERLAY","Locator",null));//super class final method	

		this.nameGenerator = new AbstractNameGenerator(){
			@Override
			public String getName(AbstractVizResource<?,?> resource ){
				return "Locator";
			}
		};setColor (new RGB(0,255,0));
	}


    @Override
    public String toString() {
    	return "Locator";  
    }    

	@Override
	public boolean equals(Object obj) {

		if (this == obj) {
			return true;
		}
		if (obj == null || obj instanceof LocatorResourceData == false) {
			return false;
		}
		LocatorResourceData other = (LocatorResourceData) obj;

		return true;
	}


	@Override
	public LocatorResource constructResource(
			LoadProperties loadProperties, IDescriptor descriptor)
			throws VizException {
		
		setResourceName(new ResourceName("OVERLAY","Locator",null));//super class' final method
		
		return new LocatorResource(this, loadProperties);
	}
	
	private LocatorResource lr = null;
	
	public void setLocatorResource(LocatorResource l){
		lr = l;
	}
	
	public LocatorResource getLocatorResource(){
		return lr;
	}

}
