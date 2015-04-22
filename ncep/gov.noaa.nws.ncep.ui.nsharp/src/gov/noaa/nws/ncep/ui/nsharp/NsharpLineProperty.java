/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.NsharpLineProperty
 * 
 * This java class performs the surface station locator functions.
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/13/2012   			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp;

import gov.noaa.nws.ncep.viz.common.RGBColorAdapter;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;

@XmlRootElement(name = "NsharpLineProperty")
@XmlAccessorType(XmlAccessType.NONE)
public class NsharpLineProperty implements ISerializableObject{
	//@XmlAttribute
	//private String lineName="";
	@XmlAttribute
	private LineStyle lineStyle;
	@XmlAttribute
    private int lineWidth;
	@XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB lineColor;
    
	public NsharpLineProperty() {
		super();
		lineWidth = 1;
		lineColor = NsharpConstants.color_white;
		lineStyle = LineStyle.SOLID;
	}
	
	public NsharpLineProperty(LineStyle lineStyle, int lineWidth, RGB lineColor) {
		super();
		this.lineStyle = lineStyle;
		this.lineWidth = lineWidth;
		this.lineColor = lineColor;
	}

	public LineStyle getLineStyle() {
		return lineStyle;
	}
	public void setLineStyle(LineStyle lineStyle) {
		this.lineStyle = lineStyle;
	}
	public int getLineWidth() {
		return lineWidth;
	}
	public void setLineWidth(int lineWidth) {
		this.lineWidth = lineWidth;
	}
	public RGB getLineColor() {
		return lineColor;
	}
	public void setLineColor(RGB lineColor) {
		this.lineColor = lineColor;
	}
    
}
