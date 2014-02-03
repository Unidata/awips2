package gov.noaa.nws.ncep.viz.rsc.solarimage.rsc;

import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.ui.display.ColorBarFromColormap;

import java.text.ParseException;
import java.text.ParsePosition;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * The resource data class for Solar Image.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer         Description
 * ------------ ---------- -----------      --------------------------
 * 02/21/2013    958       qzhou            Initial creation
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "SolarImageResourceData")
public class SolarImageResourceData extends
        AbstractNatlCntrsRequestableResourceData {

    private static final String instrumentParam = "instrument";

    private static final String wavelengthParam = "wavelength";

    private static final String intTimeParam = "intTime";

    @XmlElement
    private Float alpha;

    @XmlElement
    private Float brightness;

    @XmlElement
    private Float contrast;

    @XmlElement
    private Integer cylindrical;

    @XmlElement
    private String colorMapName;

    @XmlElement
    private ColorBarFromColormap colorBar;

    @XmlElement
    private String displayUnitStr;

    @XmlElement
    private String imageFunction;

    private Unit<?> displayUnit;

    public SolarImageResourceData() {

        super();

        // called by AbstractVizResource.getName()
        // and we delegate back to the resource
        this.nameGenerator = new AbstractNameGenerator() {

            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                return ((SolarImageResource) resource).getLegendStr();
            }
        };
    }

    @Override
    public NcDisplayType[] getSupportedDisplayTypes() {
        return new NcDisplayType[] { NcDisplayType.SOLAR_DISPLAY };
    }

    // The following methods assume that the constraints in the metadata map
    // are are for 'exact' matches. ie. that the one constraint value is the
    // value
    // that will be queried.
    public String getInstrument() {
        return (metadataMap.containsKey(instrumentParam) ? metadataMap.get(
                instrumentParam).getConstraintValue() : "");
    }

    public String getWavelength() {
        return (metadataMap.containsKey(wavelengthParam) ? metadataMap.get(
                wavelengthParam).getConstraintValue() : "");
    }

    public String getIntTime() {
        return (metadataMap.containsKey(intTimeParam) ? metadataMap.get(
                intTimeParam).getConstraintValue() : "");
    }

    public String getImageFunction() {

        return imageFunction;
    }

    public void setImageFunction(String imageFunction) {
        this.imageFunction = imageFunction;
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        return new SolarImageResource(this, loadProperties);
    }

    public Unit<?> getDisplayUnit() {
        if (displayUnit == null) {
            setDisplayUnitStr(displayUnitStr);
        }
        return displayUnit;
    }

    public String getDisplayUnitStr() {
        return displayUnitStr;
    }

    public void setDisplayUnitStr(String dispUnitStr) {
        this.displayUnitStr = dispUnitStr;

        if (displayUnit == null) {
            if (displayUnitStr != null) {
                try {
                    displayUnit = UnitFormat.getUCUMInstance().parseSingleUnit(
                            displayUnitStr, new ParsePosition(0));
                } catch (ParseException e) {
                    System.out.println("Unable parse display units : "
                            + displayUnitStr);
                }
            }
        }
    }

    public String getColorMapName() {
        return colorMapName;
    }

    public void setColorMapName(String cmapName) {
        colorMapName = cmapName;
    }

    public ColorBarFromColormap getColorBar() {
        return colorBar;
    }

    public void setColorBar(ColorBarFromColormap cBar) {
        this.colorBar = cBar;
    }

    public Float getAlpha() {
        return alpha;
    }

    public void setAlpha(Float alpha) {
        this.alpha = alpha;
    }

    public Float getBrightness() {
        return brightness;
    }

    public void setBrightness(Float brightness) {
        this.brightness = brightness;
    }

    public Float getContrast() {
        return contrast;
    }

    public void setContrast(Float contrast) {
        this.contrast = contrast;
    }

    public Integer getCylindrical() {
        return cylindrical;
    }

    public void setCylindrical(Integer cylindrical) {
        this.cylindrical = cylindrical;
    }

    @Override
    public boolean equals(Object obj) {
        if (!super.equals(obj)) {
            return false;
        }
        if (obj instanceof SolarImageResourceData == false) {
            return false;
        }

        SolarImageResourceData other = (SolarImageResourceData) obj;

        if (this.colorMapName != null && other.colorMapName == null) {
            return false;
        } else if (this.colorMapName == null && other.colorMapName != null) {
            return false;
        } else if (this.colorMapName != null
                && this.colorMapName.equals(other.colorMapName) == false) {
            return false;
        }

        if (this.displayUnitStr != null && other.displayUnitStr == null) {
            return false;
        } else if (this.displayUnitStr == null && other.displayUnitStr != null) {
            return false;
        } else if (this.displayUnitStr != null
                && this.displayUnitStr.equals(other.displayUnitStr) == false) {
            return false;
        }

        if ((this.alpha != null && other.alpha == null)
                || (this.alpha == null && other.alpha != null)
                || (this.alpha != null && this.alpha.equals(other.alpha) == false)) {
            return false;

        }

        if ((this.brightness != null && other.brightness == null)
                || (this.brightness == null && other.brightness != null)
                || (this.brightness != null && this.brightness
                        .equals(other.brightness) == false)) {
            return false;

        }

        if ((this.contrast != null && other.contrast == null)
                || (this.contrast == null && other.contrast != null)
                || (this.contrast != null && this.contrast
                        .equals(other.contrast) == false)) {
            return false;

        }

        return true;
    }
}
