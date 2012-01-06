package gov.noaa.nws.ncep.viz.rsc.ncscat.rsc;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.RGBColorAdapter;
import gov.noaa.nws.ncep.viz.ui.display.ColorBar;
import gov.noaa.nws.ncep.ui.pgen.display.IVector.VectorType;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;


/**
 * NcscatResourceData - Class for display of all types of satellite
 * scatterometer/radiometer data to showing ocean surface winds.
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 30 Oct 2009  176        B. Hebbard  Initial creation (as QuikSCAT).
 * 14 Feb 2010  235A       B. Hebbard  Convert from QuikSCAT (@D3) to NCSCAT (@D6)
 * 11 Jun 2010  235B       B. Hebbard  Expand for all Ocean Winds data types
 * 14 Jul 2010  235C       B. Hebbard  Use common NC ColorBar
 * 14 Jan 2011  235D       B. Hebbard  Add densityValue (for progressive disclosure)
 * 03 Feb 2011  235E       B. Hebbard  Add support for ambiguity variants
 * 
 * </pre>
 * 
 * @author bhebbard 
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NC-NcscatResourceData")
public class NcscatResourceData extends AbstractNatlCntrsRequestableResourceData
                                  implements INatlCntrsResourceData { 

    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB color;  //  resource legend color only

    @XmlElement
    protected boolean skipEnable;
    @XmlElement
    protected int     skipValue;
    @XmlElement
    protected int     densityValue;
    @XmlElement
    protected boolean timeStampEnable;
    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     timeStampColor;
    @XmlElement
    protected int     timeStampInterval;
    @XmlElement
    protected int     timeStampLineWidth;

    public static enum ArrowStyle {
        //                  menu display name    PGEN VectorType       PGEN directionOnly    pgenType
        DIRECTIONAL_ARROW ("Directional Arrows", VectorType.ARROW,     true,                 "Directional"),
        REGULAR_ARROW     ("Regular Arrows",     VectorType.ARROW,     false,                "Arrow"),
        WIND_BARB         ("Wind Barbs",         VectorType.WIND_BARB, false,                "Barb");
        private String displayName;
        private VectorType vectorType;
        private boolean directionOnly;
        private String pgenType;
        private ArrowStyle (String displayName, VectorType vectorType, boolean directionOnly, String pgenType) {
            this.displayName = displayName;
            this.vectorType = vectorType;
            this.directionOnly = directionOnly;
            this.pgenType = pgenType;
        }
        public String getDisplayName() {
            return displayName;
        }
        public VectorType getVectorType() {
            return vectorType;
        }
        public boolean getDirectionOnly() {
            return directionOnly;
        }
        public String getPgenType() {
            return pgenType;
        }
    }
    
    @XmlElement
    protected ArrowStyle arrowStyle;
    @XmlElement
    protected int arrowWidth;
    @XmlElement
    protected int arrowSize;
    @XmlElement
    protected int headSize;

    @XmlElement
    protected ColorBar colorBar1;
    @XmlElement
    protected ColorBar colorBar2;
    
    @XmlElement
    protected boolean highWindSpeedEnable;
    @XmlElement
    protected boolean lowWindSpeedEnable;
    @XmlElement
    protected boolean rainFlagEnable;
    @XmlElement
    protected boolean availabilityFlagEnable;
    @XmlElement
    protected boolean use2ndColorForRainEnable;
    @XmlElement
    protected boolean plotCirclesForRainEnable;
    
    //  ------------------------------------------------------------

    private static final RGB    RESOURCE_LEGEND_COLOR = new RGB(155, 155, 155);

    private NcscatMode ncscatMode = NcscatMode.UNKNOWN;
    private int ambigNumber = 0;  // if >0, ambiguity variant number
    
    /**
     * Create a NCSCAT resource.
     * 
     * @throws VizException
     */
    public NcscatResourceData() throws VizException {
        super();
        this.nameGenerator = new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
            	String s = ncscatMode.toString();
            	if (ambigNumber > 0) {
            		s = s.concat(" Ambiguity #" + ambigNumber);
            	}
            	return s; 
            }
        };
        color = RESOURCE_LEGEND_COLOR;
    }

    public NcscatMode getNcscatMode() {
        return ncscatMode;
    }

    public void setNcscatMode() {
    	//  Determine which data format we're using -- and the
    	//  ambiguity variant number, if any -- from the reportType
    	//  being requested from the database
    	String s = metadataMap.get("reportType").getConstraintValue();
    	//  If there's an ambiguity suffix, remove it before lookup,
    	//  but remember the ambiguity number
    	Pattern p = Pattern.compile("-ambig([1-9])$");
    	Matcher m = p.matcher(s);
    	if (m.find()) {
    		String n = m.group(1);
    		ambigNumber = Integer.parseInt(n);
    		s = m.replaceAll("");
    	}
    	else {
    		ambigNumber = 0;
    	}

    	//  Determine mode (data format) based on (sub)string of reportType
        try {
            this.ncscatMode = NcscatMode.stringToMode(s);
        }
        catch (Exception e) {
            this.ncscatMode = NcscatMode.UNKNOWN;
        }
    }

    public RGB getColor() {
        return color;
    }

    public void setColor(RGB color) {
        this.color = color;
    }

    public boolean getSkipEnable() {
		return skipEnable;
	}

	public void setSkipEnable(boolean skipEnable) {
		this.skipEnable = skipEnable;
	}

	public int getSkipValue() {
		return skipValue;
	}

	public void setSkipValue(int skipValue) {
		this.skipValue = skipValue;
	}

	public int getDensityValue() {
		return densityValue;
	}

	public void setDensityValue(int densityValue) {
		this.densityValue = densityValue;
	}

	public boolean getTimeStampEnable() {
		return timeStampEnable;
	}

	public void setTimeStampEnable(boolean timeStampEnable) {
		this.timeStampEnable = timeStampEnable;
	}

	public RGB getTimeStampColor() {
		return timeStampColor;
	}

	public void setTimeStampColor(RGB timeStampColor) {
		this.timeStampColor = timeStampColor;
	}

	public int getTimeStampInterval() {
		return timeStampInterval;
	}

	public void setTimeStampInterval(int timeStampInterval) {
		this.timeStampInterval = timeStampInterval;
	}

	public int getTimeStampLineWidth() {
		return timeStampLineWidth;
	}

	public void setTimeStampLineWidth(int timeStampLineWidth) {
		this.timeStampLineWidth = timeStampLineWidth;
	}

	public ArrowStyle getArrowStyle() {
        return arrowStyle;
    }

    public void setArrowStyle(ArrowStyle arrowStyle) {
        this.arrowStyle = arrowStyle;
    }

	public void setArrowWidth(int arrowWidth) {
		this.arrowWidth = arrowWidth;
	}

	public Integer getArrowWidth() {
		return arrowWidth;
	}

	public void setArrowWidth(Integer arrowWidth) {
		this.arrowWidth = arrowWidth;
	}

	public int getArrowSize() {
		return arrowSize;
	}

	public void setArrowSize(int arrowSize) {
		this.arrowSize = arrowSize;
	}

	public int getHeadSize() {
		return headSize;
	}

	public void setHeadSize(int headSize) {
		this.headSize = headSize;
	}
    
    public ColorBar getColorBar1() {
        return colorBar1;
    }

    public void setColorBar1(ColorBar colorBar1) {
        this.colorBar1 = colorBar1;
    }

    public ColorBar getColorBar2() {
        return colorBar2;
    }

    public void setColorBar2(ColorBar colorBar2) {
        this.colorBar2 = colorBar2;
    }

	public boolean getHighWindSpeedEnable() {
		return highWindSpeedEnable;
	}

	public void setHighWindSpeedEnable(boolean highWindSpeedEnable) {
		this.highWindSpeedEnable = highWindSpeedEnable;
	}

	public boolean getLowWindSpeedEnable() {
		return lowWindSpeedEnable;
	}

	public void setLowWindSpeedEnable(boolean lowWindSpeedEnable) {
		this.lowWindSpeedEnable = lowWindSpeedEnable;
	}

	public boolean getRainFlagEnable() {
		return rainFlagEnable;
	}

	public void setRainFlagEnable(boolean rainFlagEnable) {
		this.rainFlagEnable = rainFlagEnable;
	}

	public boolean getAvailabilityFlagEnable() {
		return availabilityFlagEnable;
	}

	public void setAvailabilityFlagEnable(boolean availabilityFlagEnable) {
		this.availabilityFlagEnable = availabilityFlagEnable;
	}

	public boolean getUse2ndColorForRainEnable() {
		return use2ndColorForRainEnable;
	}

	public void setUse2ndColorForRainEnable(boolean use2ndColorForRainEnable) {
		this.use2ndColorForRainEnable = use2ndColorForRainEnable;
	}

	public boolean getPlotCirclesForRainEnable() {
		return plotCirclesForRainEnable;
	}

	public void setPlotCirclesForRainEnable(boolean plotCirclesForRainEnable) {
		this.plotCirclesForRainEnable = plotCirclesForRainEnable;
	}

	@Override
	protected AbstractVizResource<?, ?> constructResource(
			LoadProperties loadProperties,
			com.raytheon.uf.common.dataplugin.PluginDataObject[] objects)
			throws VizException {
		return new NcscatResource( this, loadProperties );
	}

}