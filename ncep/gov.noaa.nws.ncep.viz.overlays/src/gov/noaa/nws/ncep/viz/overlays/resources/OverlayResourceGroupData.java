
package gov.noaa.nws.ncep.viz.overlays.resources;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.common.RGBColorAdapter;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.ResourceList;

/**
 * ResourceData for Resource Groups
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/08/09                ghull       Created from MapResourceGroupData
 * Aug 06, 2009            ghull       construct() -> constructResource()
 * Nov 18, 2009            ghull       Incorporate to11d6 changes 
 * Apr 14  2010    #259    ghull       set legendColor from color
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NC-OverlayResourceGroupData")
public class OverlayResourceGroupData extends AbstractNatlCntrsResourceData implements
                   INatlCntrsResourceData, IResourceGroup {

    @XmlElement(name = "resource")
    private ResourceList resourceList;

    @XmlElement
    private String mapName;

    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB color = new RGB(155, 155, 155);

    @XmlElement
	private int lineWidth = 1;

    @XmlElement
    private LineStyle lineStyle = LineStyle.SOLID;


    public OverlayResourceGroupData() {
        this.resourceList = new ResourceList();
        this.nameGenerator = new AbstractNameGenerator() {

            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                return mapName;
            }

        };
    }

    @Override
    public OverlayResourceGroup constructResource(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        return new OverlayResourceGroup(this, loadProperties);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.IResourceGroup#getResourceList()
     */
    @Override
    public ResourceList getResourceList() {
        return resourceList;
    }

    /**
     * @return the mapName
     */
    public String getMapName() {
        return mapName;
    }

    /**
     * @param mapName
     *            the mapName to set
     */
    public void setMapName(String mapName) {
        this.mapName = mapName;
    }
	
	@SuppressWarnings("unchecked")
	@Override
	public boolean setRscAttrSet( ResourceAttrSet newRscAttrSet ) {
		super.setRscAttrSet( newRscAttrSet );
		boolean retVal = true;
		
        for( ResourcePair rp : resourceList ) {
        	if( rp.getResourceData() instanceof INatlCntrsResourceData ) {
        		INatlCntrsResourceData rscData = (INatlCntrsResourceData)rp.getResourceData();        	
        		retVal = (retVal && rscData.setRscAttrSet( newRscAttrSet ) );
        		// call resourceAttrsModified on the resource?
        	}
        }
        return retVal;
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
	
	public void setResourceName( ResourceName rscName ) {		
		super.setResourceName(rscName);
		
        for( ResourcePair rp : resourceList ) {
        	if( rp.getResourceData() instanceof INatlCntrsResourceData ) {
        		INatlCntrsResourceData rscData = (INatlCntrsResourceData)rp.getResourceData();        	
        		rscData.setResourceName( rscName );        		
        	}
        }
	}

    @Override
    public boolean equals(Object obj) {
		if (!super.equals(obj)) {
			return false;
		}
        OverlayResourceGroupData other = (OverlayResourceGroupData) obj;

        if (this.resourceList != null && other.resourceList == null) {
            return false;
        } else if (this.resourceList == null && other.resourceList != null) {
            return false;
        } else if (this.resourceList != null
                && this.resourceList.equals(other.resourceList) == false) {
            return false;
        }

        if (this.mapName != null && other.mapName == null) {
            return false;
        } else if (this.mapName == null && other.mapName != null) {
            return false;
        } else if (this.mapName != null
                && this.mapName.equals(other.mapName) == false) {
            return false;
        }

        return true;
    }
}
