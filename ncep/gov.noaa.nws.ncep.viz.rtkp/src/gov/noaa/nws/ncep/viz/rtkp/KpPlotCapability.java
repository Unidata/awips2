/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
package gov.noaa.nws.ncep.viz.rtkp;

import gov.noaa.nws.ncep.viz.common.RGBColorAdapter;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;

/**
 * Implementation for describing persistable capabilities for the Kp Plot
 * attributes
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- -----------   --------------------------
 * 09-03-2014   R4078       sgurung       Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class KpPlotCapability extends AbstractCapability {

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB plotColor = new RGB(0, 238, 238);

    @XmlAttribute
    protected String textSize = "14";

    @XmlAttribute
    protected String textFont = "Times";

    @XmlAttribute
    protected String textStyle = "Bold";

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB textColor = new RGB(255, 255, 255);

    @XmlAttribute
    private String pointStyle = "POINT";

    @XmlAttribute
    private String pointSize = "1.75";

    public KpPlotCapability() {

    }

    public KpPlotCapability(RGB plotColor, String textSize, String textFont,
            String textStyle, RGB textColor, String pointStyle, String pointSize) {

        this.plotColor = plotColor;
        this.textSize = textSize;
        this.textFont = textFont;
        this.textStyle = textStyle;
        this.textColor = textColor;
        this.pointStyle = pointStyle;
        this.pointSize = pointSize;
    }

    public RGB getPlotColor() {
        return plotColor;
    }

    public void setPlotColor(RGB plotColor) {
        this.plotColor = plotColor;
    }

    public String getTextSize() {
        return textSize;
    }

    public void setTextSize(String textSize) {
        this.textSize = textSize;
    }

    public String getTextFont() {
        return textFont;
    }

    public void setTextFont(String textFont) {
        this.textFont = textFont;
    }

    public String getTextStyle() {
        return textStyle;
    }

    public void setTextStyle(String textStyle) {
        this.textStyle = textStyle;
    }

    public RGB getTextColor() {
        return textColor;
    }

    public void setTextColor(RGB textColor) {
        this.textColor = textColor;
    }

    public String getPointStyle() {
        return pointStyle;
    }

    public void setPointStyle(String pointStyle) {
        this.pointStyle = pointStyle;
    }

    public String getPointSize() {
        return pointSize;
    }

    public void setPointSize(String pointSize) {
        this.pointSize = pointSize;
    }

    @Override
    public AbstractCapability clone() {
        KpPlotCapability kpc = new KpPlotCapability();
        kpc.plotColor = plotColor;
        kpc.textSize = textSize;
        kpc.textFont = textFont;
        kpc.textStyle = textStyle;
        kpc.textColor = textColor;
        kpc.pointStyle = pointStyle;
        kpc.pointSize = pointSize;

        return kpc;
    }

}
