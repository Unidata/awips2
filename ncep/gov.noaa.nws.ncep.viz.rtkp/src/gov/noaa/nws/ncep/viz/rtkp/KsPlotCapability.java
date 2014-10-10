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
 * Implementation for describing persistable capabilities for the Ks Plot
 * attributes.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- -----------   --------------------------
 * May 30, 2014 1122       sgurung       Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class KsPlotCapability extends AbstractCapability {

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB plotColorStn0 = new RGB(0, 0, 0);

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB plotColorStn1 = new RGB(0, 255, 0);

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB plotColorStn2 = new RGB(78, 146, 88);

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB plotColorStn3 = new RGB(255, 255, 0);

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB plotColorStn4 = new RGB(255, 136, 0);

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB plotColorStn5 = new RGB(255, 0, 0);

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB plotColorStn6 = new RGB(136, 136, 136);

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB plotColorStn7 = new RGB(68, 68, 68);

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB plotColorStn8 = new RGB(255, 255, 255);

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

    public KsPlotCapability() {

    }

    public KsPlotCapability(RGB[] plotColorStns, String textSize,
            String textFont, String textStyle, RGB textColor,
            String pointStyle, String pointSize) {
        if (plotColorStns != null && plotColorStns.length == 9) {
            this.plotColorStn0 = plotColorStns[0];
            this.plotColorStn1 = plotColorStns[1];
            this.plotColorStn2 = plotColorStns[2];
            this.plotColorStn3 = plotColorStns[3];
            this.plotColorStn4 = plotColorStns[4];
            this.plotColorStn5 = plotColorStns[5];
            this.plotColorStn6 = plotColorStns[6];
            this.plotColorStn7 = plotColorStns[7];
            this.plotColorStn8 = plotColorStns[8];
        }

        this.textSize = textSize;
        this.textFont = textFont;
        this.textStyle = textStyle;
        this.textColor = textColor;
        this.pointStyle = pointStyle;
        this.pointSize = pointSize;
    }

    public RGB getPlotColorStn0() {
        return plotColorStn0;
    }

    public void setPlotColorStn0(RGB plotColorStn0) {
        if ((this.plotColorStn0 == null) && (plotColorStn0 == null)) {
            return;
        }
        if ((this.plotColorStn0 == null)
                || !this.plotColorStn0.equals(plotColorStn0)) {
            this.plotColorStn0 = plotColorStn0;
            this.capabilityChanged();
        }
    }

    public RGB getPlotColorStn1() {
        return plotColorStn1;
    }

    public void setPlotColorStn1(RGB plotColorStn1) {
        if ((this.plotColorStn1 == null) && (plotColorStn1 == null)) {
            return;
        }
        if ((this.plotColorStn1 == null)
                || !this.plotColorStn1.equals(plotColorStn1)) {
            this.plotColorStn1 = plotColorStn1;
            this.capabilityChanged();
        }
    }

    public RGB getPlotColorStn2() {
        return plotColorStn2;
    }

    public void setPlotColorStn2(RGB plotColorStn2) {
        if ((this.plotColorStn2 == null) && (plotColorStn2 == null)) {
            return;
        }
        if ((this.plotColorStn2 == null)
                || !this.plotColorStn2.equals(plotColorStn2)) {
            this.plotColorStn2 = plotColorStn2;
            this.capabilityChanged();
        }
    }

    public RGB getPlotColorStn3() {
        return plotColorStn3;
    }

    public void setPlotColorStn3(RGB plotColorStn3) {
        if ((this.plotColorStn3 == null) && (plotColorStn3 == null)) {
            return;
        }
        if ((this.plotColorStn3 == null)
                || !this.plotColorStn3.equals(plotColorStn3)) {
            this.plotColorStn3 = plotColorStn3;
            this.capabilityChanged();
        }
    }

    public RGB getPlotColorStn4() {
        return plotColorStn4;
    }

    public void setPlotColorStn4(RGB plotColorStn4) {
        if ((this.plotColorStn4 == null) && (plotColorStn4 == null)) {
            return;
        }
        if ((this.plotColorStn4 == null)
                || !this.plotColorStn4.equals(plotColorStn4)) {
            this.plotColorStn4 = plotColorStn4;
            this.capabilityChanged();
        }
    }

    public RGB getPlotColorStn5() {
        return plotColorStn5;
    }

    public void setPlotColorStn5(RGB plotColorStn5) {
        if ((this.plotColorStn5 == null) && (plotColorStn5 == null)) {
            return;
        }
        if ((this.plotColorStn5 == null)
                || !this.plotColorStn5.equals(plotColorStn5)) {
            this.plotColorStn5 = plotColorStn5;
            this.capabilityChanged();
        }
    }

    public RGB getPlotColorStn6() {
        return plotColorStn6;
    }

    public void setPlotColorStn6(RGB plotColorStn6) {
        if ((this.plotColorStn6 == null) && (plotColorStn6 == null)) {
            return;
        }
        if ((this.plotColorStn6 == null)
                || !this.plotColorStn6.equals(plotColorStn6)) {
            this.plotColorStn6 = plotColorStn6;
            this.capabilityChanged();
        }
    }

    public RGB getPlotColorStn7() {
        return plotColorStn7;
    }

    public void setPlotColorStn7(RGB plotColorStn7) {
        if ((this.plotColorStn7 == null) && (plotColorStn7 == null)) {
            return;
        }
        if ((this.plotColorStn7 == null)
                || !this.plotColorStn7.equals(plotColorStn7)) {
            this.plotColorStn7 = plotColorStn7;
            this.capabilityChanged();
        }
    }

    public RGB getPlotColorStn8() {
        return plotColorStn8;
    }

    public void setPlotColorStn8(RGB plotColorStn8) {
        if ((this.plotColorStn8 == null) && (plotColorStn8 == null)) {
            return;
        }
        if ((this.plotColorStn8 == null)
                || !this.plotColorStn8.equals(plotColorStn8)) {
            this.plotColorStn8 = plotColorStn8;
            this.capabilityChanged();
        }
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

    public RGB[] getPlotColors() {

        RGB[] plotColorStns = new RGB[9];
        plotColorStns[0] = plotColorStn0;
        plotColorStns[1] = plotColorStn1;
        plotColorStns[2] = plotColorStn2;
        plotColorStns[3] = plotColorStn3;
        plotColorStns[4] = plotColorStn4;
        plotColorStns[5] = plotColorStn5;
        plotColorStns[6] = plotColorStn6;
        plotColorStns[7] = plotColorStn7;
        plotColorStns[8] = plotColorStn8;

        return plotColorStns;
    }

    public void setPlotColors(RGB[] plotColorStns) {
        if (plotColorStns != null && plotColorStns.length == 9) {
            plotColorStn0 = plotColorStns[0];
            plotColorStn1 = plotColorStns[1];
            plotColorStn2 = plotColorStns[2];
            plotColorStn3 = plotColorStns[3];
            plotColorStn4 = plotColorStns[4];
            plotColorStn5 = plotColorStns[5];
            plotColorStn6 = plotColorStns[6];
            plotColorStn7 = plotColorStns[7];
            plotColorStn8 = plotColorStns[8];
        }
    }

    @Override
    public AbstractCapability clone() {
        KsPlotCapability ksc = new KsPlotCapability();
        ksc.plotColorStn0 = plotColorStn0;
        ksc.plotColorStn1 = plotColorStn1;
        ksc.plotColorStn2 = plotColorStn2;
        ksc.plotColorStn3 = plotColorStn3;
        ksc.plotColorStn4 = plotColorStn4;
        ksc.plotColorStn5 = plotColorStn5;
        ksc.plotColorStn6 = plotColorStn6;
        ksc.plotColorStn7 = plotColorStn7;
        ksc.plotColorStn8 = plotColorStn8;
        ksc.textSize = textSize;
        ksc.textFont = textFont;
        ksc.textStyle = textStyle;
        ksc.textColor = textColor;
        ksc.pointStyle = pointStyle;
        ksc.pointSize = pointSize;

        return ksc;
    }

}
