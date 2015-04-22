package gov.noaa.nws.ncep.viz.overlays.resources;

import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.viz.common.RGBColorAdapter;
import gov.noaa.nws.ncep.viz.common.ui.Markers.MarkerType;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResourceData;

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

/**
 * Resource to draw day/night terminator line, sub-solar point marker and
 * midnight meridian line
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date           Ticket#    Engineer    Description
 * ------------   ---------- ----------- --------------------------
 * 04/24/2014     1130       S. Gurung   Initial creation
 * 
 * </pre>
 * 
 * 
 * @author sgurung
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "NC-DayNightTerminatorOverlayResourceData")
public class DayNightTerminatorOverlayResourceData extends
        AbstractNatlCntrsResourceData {

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB termLineColor = new RGB(0, 0, 255);

    @XmlElement
    private int termLineWidth = 1;

    @XmlElement
    private LineStyle termLineStyle;

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB sunMarkerColor = new RGB(255, 165, 79);

    @XmlElement
    private MarkerType sunMarkerType = MarkerType.ASTERISK;

    @XmlElement
    private Float sunMarkerSize = 1.3f;

    @XmlElement
    private Integer sunMarkerWidth = 2;

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB dayShadeColor = new RGB(255, 165, 0);

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB nightShadeColor = new RGB(30, 30, 163);

    @XmlElement
    private Float shadeAlpha = 0.5f;

    @XmlElement
    private String shadePattern;

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB midnightMeridianLineColor = new RGB(127, 127, 127);

    @XmlElement
    private int midnightMeridianLineWidth = 1;

    @XmlElement
    private LineStyle midnightMeridianLineStyle;

    @XmlElement
    private boolean displaySun = true;

    @XmlElement
    private boolean applyShading = false;

    @XmlElement
    private boolean displayMidnightMeridian = false;

    public DayNightTerminatorOverlayResourceData() {
        super();
        this.nameGenerator = new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                return "Day/Night Terminator";
            }
        };
    }

    @Override
    public DayNightTerminatorOverlayResource constructResource(
            LoadProperties loadProperties, IDescriptor descriptor)
            throws VizException {
        DayNightTerminatorOverlayResource resource = new DayNightTerminatorOverlayResource(
                this, loadProperties);
        return resource;
    }

    public int getTermLineWidth() {
        return termLineWidth;
    }

    public void setTermLineWidth(int lineWidth) {
        this.termLineWidth = lineWidth;
    }

    public RGB getTermLineColor() {
        return termLineColor;
    }

    public void setTermLineColor(RGB color) {
        this.termLineColor = color;
        this.legendColor = termLineColor;
    }

    public LineStyle getTermLineStyle() {
        return termLineStyle;
    }

    public void setTermLineStyle(LineStyle termLineStyle) {
        this.termLineStyle = termLineStyle;
    }

    public RGB getSunMarkerColor() {
        return sunMarkerColor;
    }

    public void setSunMarkerColor(RGB sunColor) {
        this.sunMarkerColor = sunColor;
    }

    public MarkerType getSunMarkerType() {
        return sunMarkerType;
    }

    public void setSunMarkerType(MarkerType sunMarkerType) {
        this.sunMarkerType = sunMarkerType;
    }

    public Float getSunMarkerSize() {
        return sunMarkerSize;
    }

    public void setSunMarkerSize(Float sunMarkerSize) {
        this.sunMarkerSize = sunMarkerSize;
    }

    public Integer getSunMarkerWidth() {
        return sunMarkerWidth;
    }

    public void setSunMarkerWidth(Integer sunMarkerWidth) {
        this.sunMarkerWidth = sunMarkerWidth;
    }

    public RGB getDayShadeColor() {
        return dayShadeColor;
    }

    public void setDayShadeColor(RGB dayShadeColor) {
        this.dayShadeColor = dayShadeColor;
    }

    public RGB getNightShadeColor() {
        return nightShadeColor;
    }

    public void setNightShadeColor(RGB nightShadeColor) {
        this.nightShadeColor = nightShadeColor;
    }

    public RGB getMidnightMeridianLineColor() {
        return midnightMeridianLineColor;
    }

    public void setMidnightMeridianLineColor(RGB midnightMeridianLineColor) {
        this.midnightMeridianLineColor = midnightMeridianLineColor;
    }

    public int getMidnightMeridianLineWidth() {
        return midnightMeridianLineWidth;
    }

    public void setMidnightMeridianLineWidth(int midnightMeridianLineWidth) {
        this.midnightMeridianLineWidth = midnightMeridianLineWidth;
    }

    public LineStyle getMidnightMeridianLineStyle() {
        return midnightMeridianLineStyle;
    }

    public void setMidnightMeridianLineStyle(LineStyle midnightMeridianLineStyle) {
        this.midnightMeridianLineStyle = midnightMeridianLineStyle;
    }

    public boolean getDisplaySun() {
        return displaySun;
    }

    public void setDisplaySun(boolean displaySun) {
        this.displaySun = displaySun;
    }

    public boolean getApplyShading() {
        return applyShading;
    }

    public void setApplyShading(boolean applyShading) {
        this.applyShading = applyShading;
    }

    public boolean getDisplayMidnightMeridian() {
        return displayMidnightMeridian;
    }

    public void setDisplayMidnightMeridian(boolean displayMidnightMeridian) {
        this.displayMidnightMeridian = displayMidnightMeridian;
    }

    public Float getShadeAlpha() {
        return shadeAlpha;
    }

    public void setShadeAlpha(Float shadeAlpha) {
        this.shadeAlpha = shadeAlpha;
    }

    public String getShadePattern() {
        return shadePattern;
    }

    public void setShadePattern(String shadePattern) {
        this.shadePattern = shadePattern;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        DayNightTerminatorOverlayResourceData other = (DayNightTerminatorOverlayResourceData) obj;
        if (termLineColor == null) {
            if (other.termLineColor != null)
                return false;
        } else if (!termLineColor.equals(other.termLineColor))
            return false;
        if (termLineWidth != other.termLineWidth)
            return false;
        if (termLineStyle == null) {
            if (other.termLineStyle != null)
                return false;
        } else if (!termLineStyle.equals(other.termLineStyle))
            return false;
        if (sunMarkerColor == null) {
            if (other.sunMarkerColor != null)
                return false;
        } else if (!sunMarkerColor.equals(other.sunMarkerColor))
            return false;

        if (sunMarkerType == null) {
            if (other.sunMarkerType != null)
                return false;
        } else if (!sunMarkerType.equals(other.sunMarkerType))
            return false;
        if (sunMarkerSize != other.sunMarkerSize)
            return false;
        if (sunMarkerWidth != other.sunMarkerWidth)
            return false;

        if (dayShadeColor == null) {
            if (other.dayShadeColor != null)
                return false;
        } else if (!dayShadeColor.equals(other.dayShadeColor))
            return false;
        if (nightShadeColor == null) {
            if (other.nightShadeColor != null)
                return false;
        } else if (!nightShadeColor.equals(other.nightShadeColor))
            return false;
        if (shadeAlpha != other.shadeAlpha)
            return false;
        if (shadePattern != other.shadePattern)
            return false;

        if (midnightMeridianLineColor == null) {
            if (other.termLineColor != null)
                return false;
        } else if (!midnightMeridianLineColor
                .equals(other.midnightMeridianLineColor))
            return false;
        if (midnightMeridianLineWidth != other.midnightMeridianLineWidth)
            return false;
        if (midnightMeridianLineStyle == null) {
            if (other.midnightMeridianLineStyle != null)
                return false;
        } else if (!midnightMeridianLineStyle
                .equals(other.midnightMeridianLineStyle))
            return false;

        if (displaySun != other.displaySun)
            return false;

        if (applyShading != other.applyShading)
            return false;

        if (displayMidnightMeridian != other.displayMidnightMeridian)
            return false;

        return true;
    }

    /**
     * Returns the fill pattern given a string.
     */
    public FillPattern getFillPattern(String fillPattern) {

        return FillPattern.valueOf(fillPattern);
    }

    @Override
    public String toString() {
        return "DayNightTerminatorResource: \n Terminator Line: color= "
                + termLineColor.toString() + " width= " + termLineWidth
                + " style= " + termLineStyle.toString()
                + "\n Sub-solar Point: color=  " + sunMarkerColor.toString()
                + " width=  " + sunMarkerWidth + " marker=  "
                + sunMarkerType.toString()
                + "\n Midnight Meridian Line: color= "
                + midnightMeridianLineColor.toString() + " width= "
                + midnightMeridianLineWidth + " style= "
                + midnightMeridianLineStyle.toString()
                + "\n Shading Color: day= " + dayShadeColor.toString()
                + " night= " + nightShadeColor.toString() + " alpha= "
                + shadeAlpha + " pattern=" + shadePattern.toString()
                + "\n Display Sub-solar Point: " + displaySun
                + "\n Apply Shading: " + applyShading
                + "\n Display Midnight Meridian: " + displayMidnightMeridian;
    }

}
