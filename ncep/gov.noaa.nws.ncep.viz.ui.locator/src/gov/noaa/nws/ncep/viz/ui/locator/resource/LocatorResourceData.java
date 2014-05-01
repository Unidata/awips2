package gov.noaa.nws.ncep.viz.ui.locator.resource;

import gov.noaa.nws.ncep.viz.common.RGBColorAdapter;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResourceData;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *
 * ???????????  ???         ???         Created
 * 12/14/1212   #903        Greg Hull   fontSize, fontName, and color
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 * 
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="LocatorResourceData")
public class LocatorResourceData extends AbstractNatlCntrsResourceData {
	
	public static final int MAX_NUM_SOURCES = 5;

    @XmlElement
	private Integer fontSize;

    @XmlElement
	private String fontName;

    @XmlElement
	private String pos1LocatorSource = ""; 
 
    @XmlElement
	private Integer pos1RoundToNearest=1;
    
    @XmlElement
	private String pos1DisplayUnit="";
    
    @XmlElement
	private String pos1DirectionUnit="";

    // Position 2
    @XmlElement
	private String pos2LocatorSource = "";

    @XmlElement
	private Integer pos2RoundToNearest=1;
    
    @XmlElement
	private String pos2DisplayUnit="";
    
    @XmlElement
	private String pos2DirectionUnit="";

    // Position 3
    @XmlElement
	private String pos3LocatorSource = "";

    @XmlElement
	private Integer pos3RoundToNearest=1;
    
    @XmlElement
	private String pos3DisplayUnit = "";
    
    @XmlElement
	private String pos3DirectionUnit = "";

    // Position 4
    @XmlElement
	private String pos4LocatorSource = "";

    @XmlElement
	private Integer pos4RoundToNearest=1;
    
    @XmlElement
	private String pos4DisplayUnit = "";
    
    @XmlElement
	private String pos4DirectionUnit = "";

    // Position 5
    @XmlElement
	private String pos5LocatorSource = "";

    @XmlElement
	private Integer pos5RoundToNearest=1;
    
    @XmlElement
	private String pos5DisplayUnit = "";
    
    @XmlElement
	private String pos5DirectionUnit = "";


    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB color = null;
    
    private RGB dfltColor = new RGB(255, 255, 255);
    	
    private String legendStr = null;
    
	public LocatorResourceData() {
		super();
		
		this.nameGenerator = new AbstractNameGenerator(){
			@Override
			public String getName(AbstractVizResource<?,?> resource ){
				if( legendStr == null ) {
					createLegend();
				}
				return legendStr;
			}
		};		
	}

	private void createLegend() {
		legendStr = "Locator (";
		if( getPos1Enabled() ) {
			legendStr += " "+getPos1LocatorSource();
		}
		if( getPos2Enabled() ) {
			legendStr += ", "+getPos2LocatorSource();
		}
		if( getPos3Enabled() ) {
			legendStr += ", "+getPos3LocatorSource();
		}
		if( getPos4Enabled() ) {
			legendStr += ", "+getPos4LocatorSource();
		}
		if( getPos5Enabled() ) {
			legendStr += ", "+getPos5LocatorSource();
		}
		legendStr += ")";
	}

    @Override
    public String toString() {
    	return "Locator/"+getResourceName().getRscAttrSetName();  
    }    

	@Override
	public AbstractVizResource<?, ?> constructResource(
			LoadProperties loadProperties, IDescriptor descriptor)
			throws VizException {
		return new LocatorResource(this, loadProperties);
	}

	private Boolean isValidDataSourceName( String srcName ) {
		return ( srcName != null && 
				!srcName.isEmpty() &&
				!srcName.equalsIgnoreCase("omit") &&
				!srcName.equalsIgnoreCase("none") );
	}
	
	public String getPos1LocatorSource() {
		return pos1LocatorSource;
	}

	public void setPos1LocatorSource(String pos1LocatorSource) {
		this.pos1LocatorSource = pos1LocatorSource;
		createLegend();
	}

	public Boolean getPos1Enabled() {
		return isValidDataSourceName( pos1LocatorSource );
	}

	public Integer getPos1RoundToNearest() {
		return pos1RoundToNearest;
	}

	public void setPos1RoundToNearest(Integer pos1RoundToNearest) {
		this.pos1RoundToNearest = pos1RoundToNearest;
	}

	public String getPos1DisplayUnit() {
		return pos1DisplayUnit;
	}

	public void setPos1DisplayUnit(String pos1DisplayUnit) {
		this.pos1DisplayUnit = pos1DisplayUnit;
	}

	public String getPos1DirectionUnit() {
		return pos1DirectionUnit;
	}

	public void setPos1DirectionUnit(String pos1DirectionUnit) {
		this.pos1DirectionUnit = pos1DirectionUnit;
	}

	public String getPos2LocatorSource() {
		return pos2LocatorSource;
	}

	public void setPos2LocatorSource(String pos2LocatorSource) {
		this.pos2LocatorSource = pos2LocatorSource;
		createLegend();
	}

	public Boolean getPos2Enabled() {
		return isValidDataSourceName( pos2LocatorSource );
	}

	public Integer getPos2RoundToNearest() {
		return pos2RoundToNearest;
	}

	public void setPos2RoundToNearest(Integer pos2RoundToNearest) {
		this.pos2RoundToNearest = pos2RoundToNearest;
	}

	public String getPos2DisplayUnit() {
		return pos2DisplayUnit;
	}

	public void setPos2DisplayUnit(String pos2DisplayUnit) {
		this.pos2DisplayUnit = pos2DisplayUnit;
	}

	public String getPos2DirectionUnit() {
		return pos2DirectionUnit;
	}

	public void setPos2DirectionUnit(String pos2DirectionUnit) {
		this.pos2DirectionUnit = pos2DirectionUnit;
	}

	public String getPos3LocatorSource() {
		return pos3LocatorSource;
	}

	public void setPos3LocatorSource(String pos3LocatorSource) {
		this.pos3LocatorSource = pos3LocatorSource;
		createLegend();
	}

	public Boolean getPos3Enabled() {
		return isValidDataSourceName( pos3LocatorSource );
	}

	public Integer getPos3RoundToNearest() {
		return pos3RoundToNearest;
	}

	public void setPos3RoundToNearest(Integer pos3RoundToNearest) {
		this.pos3RoundToNearest = pos3RoundToNearest;
	}

	public String getPos3DisplayUnit() {
		return pos3DisplayUnit;
	}

	public void setPos3DisplayUnit(String pos3DisplayUnit) {
		this.pos3DisplayUnit = pos3DisplayUnit;
	}

	public String getPos3DirectionUnit() {
		return pos3DirectionUnit;
	}

	public void setPos3DirectionUnit(String pos3DirectionUnit) {
		this.pos3DirectionUnit = pos3DirectionUnit;
	}

	public String getPos4LocatorSource() {
		return pos4LocatorSource;
	}

	public void setPos4LocatorSource(String pos4LocatorSource) {
		this.pos4LocatorSource = pos4LocatorSource;
		createLegend();
	}

	public Boolean getPos4Enabled() {
		return isValidDataSourceName( pos4LocatorSource );
	}

	public Integer getPos4RoundToNearest() {
		return pos4RoundToNearest;
	}

	public void setPos4RoundToNearest(Integer pos4RoundToNearest) {
		this.pos4RoundToNearest = pos4RoundToNearest;
	}

	public String getPos4DisplayUnit() {
		return pos4DisplayUnit;
	}

	public void setPos4DisplayUnit(String pos4DisplayUnit) {
		this.pos4DisplayUnit = pos4DisplayUnit;
	}

	public String getPos4DirectionUnit() {
		return pos4DirectionUnit;
	}

	public void setPos4DirectionUnit(String pos4DirectionUnit) {
		this.pos4DirectionUnit = pos4DirectionUnit;
	}

	public String getPos5LocatorSource() {
		return pos5LocatorSource;
	}

	public void setPos5LocatorSource(String pos5LocatorSource) {
		this.pos5LocatorSource = pos5LocatorSource;
		createLegend();
	}

	public Boolean getPos5Enabled() {
		return isValidDataSourceName( pos5LocatorSource );
	}

	public Integer getPos5RoundToNearest() {
		return pos5RoundToNearest;
	}

	public void setPos5RoundToNearest(Integer pos5RoundToNearest) {
		this.pos5RoundToNearest = pos5RoundToNearest;
	}

	public String getPos5DisplayUnit() {
		return pos5DisplayUnit;
	}

	public void setPos5DisplayUnit(String pos5DisplayUnit) {
		this.pos5DisplayUnit = pos5DisplayUnit;
	}

	public String getPos5DirectionUnit() {
		return pos5DirectionUnit;
	}

	public void setPos5DirectionUnit(String pos5DirectionUnit) {
		this.pos5DirectionUnit = pos5DirectionUnit;
	}

	public RGB getColor() {
		return (color==null?dfltColor:color);
	}

	public void setColor(RGB _color) {
		color = _color; 
		this.legendColor = color;
	}

	public String getFontName() {
		return fontName;
	}

	public void setFontName(String fontName) {
		this.fontName = fontName;
	}

	public Integer getFontSize() {
		return (fontSize == null ? 12 : fontSize);
	}

	public void setFontSize(Integer fs) {
		this.fontSize = fs;
	}

	public LocatorDisplayAttributes getDisplayAttributesForPosition( int pos ) {
		
		switch( pos ) {
		case 0 : 
			return ( !getPos1Enabled() ? null : 
						new LocatorDisplayAttributes( pos1LocatorSource, pos1RoundToNearest, 
											pos1DisplayUnit, pos1DirectionUnit ) );
		case 1 : 
			return ( !getPos2Enabled() ? null : 
						new LocatorDisplayAttributes( pos2LocatorSource, pos2RoundToNearest, 
											pos2DisplayUnit, pos2DirectionUnit ) );
		case 2 : 
			return ( !getPos3Enabled() ? null : 
						new LocatorDisplayAttributes( pos3LocatorSource, pos3RoundToNearest, 
											pos3DisplayUnit, pos3DirectionUnit ) );
		case 3 : 
			return ( !getPos4Enabled() ? null : 
						new LocatorDisplayAttributes( pos4LocatorSource, pos4RoundToNearest, 
											pos4DisplayUnit, pos4DirectionUnit ) );
		case 4 : 
			return ( !getPos5Enabled() ? null : 
						new LocatorDisplayAttributes( pos5LocatorSource, pos5RoundToNearest, 
											pos5DisplayUnit, pos5DirectionUnit ) );
		}
		return null;
	}

	@Override
	public boolean equals( Object obj ) {
		if (!super.equals(obj)) {
			return false;
		}

		if (obj instanceof LocatorResourceData == false) {
			return false;
		}

		LocatorResourceData other = (LocatorResourceData) obj;

		return true;
	}
}
