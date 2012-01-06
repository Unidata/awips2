/*
 * gov.noaa.nws.ncep.viz.idft.rsc.IDFTResourceData
 * 
 * September 2009
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.rsc.idft.rsc;


import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter; 

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

import gov.noaa.nws.ncep.viz.common.RGBColorAdapter;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;

/**
 * Resource data for IDFT data.
 * 
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 09/11/2009   154        Gang Zhang  Initial creation
 * 10/25/2010   307        Greg Hull   rm IdftParam for the forecast day and
 *                                     modify to work for cycle time.
 *  
 * </pre>
 * 
 * @author gzhang
 * @version 1.0
 */


@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NC-IDFTResourceData")
public class IDFTResourceData extends AbstractNatlCntrsRequestableResourceData 
				implements INatlCntrsResourceData {
	
    @XmlElement
    private String symbolName = "ASTERISK";    
        
	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	private RGB distanceColor = new RGB(255,215,0);
	
    @XmlElement
    private Double pointSize = 1.65;
    
	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	private RGB pointColor = new RGB(255,0,0);
	
	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	private RGB arrowColor = new RGB(255,215,0);
	
	@XmlElement
	private Double arrowLength = 1.25;
	
	@XmlElement 
	private Double arrowLineWidth = 1.25;
    
	public IDFTResourceData(){
		super();
		this.nameGenerator = new AbstractNameGenerator(){
			@Override
			public String getName(AbstractVizResource<?,?> resource ){
				return "IDFT ";
			}
		};
	}

    public RGB getDistanceColor() {
		return distanceColor;
	}

	public void setDistanceColor(RGB distanceColor) {
		this.distanceColor = distanceColor;
	}

	public void setSymbolName(String snStr ) {
        this.symbolName = snStr;
    }

    public String getSymbolName() {
        return this.symbolName;
    }
	
	@Override
	protected AbstractVizResource<?, ?> constructResource(
			LoadProperties loadProperties, PluginDataObject[] objects)
			throws VizException {
		return new IDFTResource(this,loadProperties);
	}

	public Double getPointSize() {
		return pointSize;
	}

	public void setPointSize(Double pointSize) {
		this.pointSize = pointSize;
	}

	public RGB getPointColor() {
		return pointColor;
	}

	public void setPointColor(RGB pointColor) {
		this.pointColor = pointColor;
	}


	public RGB getArrowColor() {
		return arrowColor;
	}

	public void setArrowColor(RGB arrowColor) {
		this.arrowColor = arrowColor;
		this.legendColor = arrowColor;
	}
	
	// override to use the arrow color
	@Override
	public RGB getLegendColor() {
		return getArrowColor();
	}

	public Double getArrowLength() {
		return arrowLength;
	}

	public void setArrowLength(Double arrowLength) {
		this.arrowLength = arrowLength;
	}

	public Double getArrowLineWidth() {
		return arrowLineWidth;
	}

	public void setArrowLineWidth(Double arrowLineWidth) {
		this.arrowLineWidth = arrowLineWidth;
	}
	
	
}
