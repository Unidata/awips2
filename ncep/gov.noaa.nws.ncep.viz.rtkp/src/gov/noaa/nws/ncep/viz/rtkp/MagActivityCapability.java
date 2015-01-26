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
import gov.noaa.nws.ncep.viz.common.ui.Markers.MarkerTextSize;

import java.awt.Color;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;

/**
 * Implementation for describing persistable capabilities for the attributes
 * used in the world-wide mag activity map (k-indices circle colors, marker
 * color, marker size, text size etc.)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- -----------   --------------------------
 * June 3, 2014 1122       sgurung       Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class MagActivityCapability extends AbstractCapability {

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB quiet = new RGB(0, 128, 0);

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB unsettled = new RGB(100, 228, 100);

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB active = new RGB(9, 135, 205);

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB minorstorm = new RGB(142, 53, 239);;

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB moderatestorm = new RGB(205, 0, 205);

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB strongstorm = new RGB(255, 255, 0);

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB severestorm = new RGB(215, 125, 0);

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB extremestorm = new RGB(255, 0, 0);

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB nonNetwrkStnColor = new RGB(157, 114, 50);

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB[] kIndicesTextColors = { new RGB(255, 255, 255),
            new RGB(255, 255, 255), new RGB(255, 255, 255), new RGB(0, 0, 0),
            new RGB(255, 255, 255), new RGB(255, 255, 255),
            new RGB(255, 255, 255), new RGB(0, 0, 0), new RGB(255, 255, 255),
            new RGB(255, 255, 255) };

    @XmlElement
    private Float markerSize = 1.3f;

    @XmlElement
    private MarkerTextSize markerTextSize = MarkerTextSize.MEDIUM;

    public MagActivityCapability() {

    }

    public MagActivityCapability(RGB[] kIndicesColors,
            RGB[] kIndicesTextColors, RGB nonNetwrkStnColor, Float markerSize,
            MarkerTextSize markerTextSize) {
        if (kIndicesColors != null && kIndicesColors.length == 8) {
            this.quiet = kIndicesColors[0];
            this.unsettled = kIndicesColors[1];
            this.active = kIndicesColors[2];
            this.minorstorm = kIndicesColors[3];
            this.moderatestorm = kIndicesColors[4];
            this.strongstorm = kIndicesColors[5];
            this.severestorm = kIndicesColors[6];
            this.extremestorm = kIndicesColors[7];
        }
        this.kIndicesTextColors = kIndicesTextColors;
        this.nonNetwrkStnColor = nonNetwrkStnColor;
        this.markerSize = markerSize;
        this.markerTextSize = markerTextSize;
    }

    public RGB getQuiet() {
        return quiet;
    }

    public void setQuiet(RGB quiet) {
        if ((this.quiet == null) && (quiet == null)) {
            return;
        }
        if ((this.quiet == null) || !this.quiet.equals(quiet)) {
            this.quiet = quiet;
            this.capabilityChanged();
        }
    }

    public RGB getUnsettled() {
        return unsettled;
    }

    public void setUnsettled(RGB unsettled) {
        if ((this.unsettled == null) && (unsettled == null)) {
            return;
        }
        if ((this.unsettled == null) || !this.unsettled.equals(unsettled)) {
            this.unsettled = unsettled;
            this.capabilityChanged();
        }
    }

    public RGB getActive() {
        return active;
    }

    public void setActive(RGB active) {
        if ((this.active == null) && (active == null)) {
            return;
        }
        if ((this.active == null) || !this.active.equals(active)) {
            this.active = active;
            this.capabilityChanged();
        }
    }

    public RGB getMinorstorm() {
        return minorstorm;
    }

    public void setMinorstorm(RGB minorstorm) {
        if ((this.minorstorm == null) && (minorstorm == null)) {
            return;
        }
        if ((this.minorstorm == null) || !this.minorstorm.equals(minorstorm)) {
            this.minorstorm = minorstorm;
            this.capabilityChanged();
        }
    }

    public RGB getModeratestorm() {
        return moderatestorm;
    }

    public void setModeratestorm(RGB moderatestorm) {
        if ((this.moderatestorm == null) && (moderatestorm == null)) {
            return;
        }
        if ((this.moderatestorm == null)
                || !this.moderatestorm.equals(moderatestorm)) {
            this.moderatestorm = moderatestorm;
            this.capabilityChanged();
        }
    }

    public RGB getStrongstorm() {
        return strongstorm;
    }

    public void setStrongstorm(RGB strongstorm) {
        if ((this.strongstorm == null) && (strongstorm == null)) {
            return;
        }
        if ((this.strongstorm == null) || !this.strongstorm.equals(strongstorm)) {
            this.strongstorm = strongstorm;
            this.capabilityChanged();
        }
    }

    public RGB getSeverestorm() {
        return severestorm;
    }

    public void setSeverestorm(RGB severestorm) {
        if ((this.severestorm == null) && (severestorm == null)) {
            return;
        }
        if ((this.severestorm == null) || !this.severestorm.equals(severestorm)) {
            this.severestorm = severestorm;
            this.capabilityChanged();
        }
    }

    public RGB getExtremestorm() {
        return extremestorm;
    }

    public void setExtremestorm(RGB extremestorm) {
        if ((this.extremestorm == null) && (extremestorm == null)) {
            return;
        }
        if ((this.extremestorm == null)
                || !this.extremestorm.equals(extremestorm)) {
            this.extremestorm = extremestorm;
            this.capabilityChanged();
        }
    }

    public Float getMarkerSize() {
        return markerSize;
    }

    public void setMarkerSize(Float markerSize) {
        if ((this.markerSize == null) && (markerSize == null)) {
            return;
        }
        this.markerSize = markerSize;
        this.capabilityChanged();
    }

    public MarkerTextSize getMarkerTextSize() {
        return markerTextSize;
    }

    public void setMarkerTextSize(MarkerTextSize markerTextSize) {
        if ((this.markerTextSize == null) && (markerTextSize == null)) {
            return;
        }
        this.markerTextSize = markerTextSize;
        this.capabilityChanged();
    }

    public RGB getNonNetwrkStnColor() {
        return nonNetwrkStnColor;
    }

    public void setNonNetwrkStnColor(RGB nonNetwrkStnColor) {
        if ((this.nonNetwrkStnColor == null) && (nonNetwrkStnColor == null)) {
            return;
        }
        this.nonNetwrkStnColor = nonNetwrkStnColor;
        this.capabilityChanged();
    }

    public RGB[] getkIndicesTextColors() {
        return kIndicesTextColors;
    }

    public void setkIndicesTextColors(RGB[] kIndicesTextColors) {
        if ((this.kIndicesTextColors == null) && (kIndicesTextColors == null)) {
            return;
        }
        this.kIndicesTextColors = kIndicesTextColors;
        this.capabilityChanged();
    }

    public RGB[] getColors() {

        RGB[] colors = new RGB[8];
        colors[0] = this.quiet;
        colors[1] = this.unsettled;
        colors[2] = this.active;
        colors[3] = this.minorstorm;
        colors[4] = this.moderatestorm;
        colors[5] = this.strongstorm;
        colors[6] = this.severestorm;
        colors[7] = this.extremestorm;

        return colors;
    }

    public void setColors(RGB[] colors) {
        if (colors != null && colors.length == 8) {
            this.quiet = colors[0];
            this.unsettled = colors[1];
            this.active = colors[2];
            this.minorstorm = colors[3];
            this.moderatestorm = colors[4];
            this.strongstorm = colors[5];
            this.severestorm = colors[6];
            this.extremestorm = colors[7];
        }
    }

    public Color[] getNOAAScaleColors() {
        Color[] colors = new Color[10];
        colors[0] = new Color(this.quiet.red, this.quiet.green, this.quiet.blue);
        colors[1] = colors[0];
        colors[2] = colors[0];
        colors[3] = new Color(this.unsettled.red, this.unsettled.green,
                this.unsettled.blue);
        colors[4] = new Color(this.active.red, this.active.green,
                this.active.blue);
        colors[5] = new Color(this.minorstorm.red, this.minorstorm.green,
                this.minorstorm.blue);
        colors[6] = new Color(this.moderatestorm.red, this.moderatestorm.green,
                this.moderatestorm.blue);
        colors[7] = new Color(this.strongstorm.red, this.strongstorm.green,
                this.strongstorm.blue);
        colors[8] = new Color(this.severestorm.red, this.severestorm.green,
                this.severestorm.blue);
        colors[9] = new Color(this.extremestorm.red, this.extremestorm.green,
                this.extremestorm.blue);

        return colors;
    }

    public RGB[] getNOAAScaleRGBColors() {
        RGB[] colors = new RGB[10];
        colors[0] = new RGB(this.quiet.red, this.quiet.green, this.quiet.blue);
        colors[1] = colors[0];
        colors[2] = colors[0];
        colors[3] = new RGB(this.unsettled.red, this.unsettled.green,
                this.unsettled.blue);
        colors[4] = new RGB(this.active.red, this.active.green,
                this.active.blue);
        colors[5] = new RGB(this.minorstorm.red, this.minorstorm.green,
                this.minorstorm.blue);
        colors[6] = new RGB(this.moderatestorm.red, this.moderatestorm.green,
                this.moderatestorm.blue);
        colors[7] = new RGB(this.strongstorm.red, this.strongstorm.green,
                this.strongstorm.blue);
        colors[8] = new RGB(this.severestorm.red, this.severestorm.green,
                this.severestorm.blue);
        colors[9] = new RGB(this.extremestorm.red, this.extremestorm.green,
                this.extremestorm.blue);

        return colors;
    }

    public Color getNonNetWorkStationMarkerColor() {
        return new Color(this.nonNetwrkStnColor.red,
                this.nonNetwrkStnColor.green, this.nonNetwrkStnColor.blue);
    }

    @Override
    public AbstractCapability clone() {
        MagActivityCapability mac = new MagActivityCapability();
        mac.quiet = this.quiet;
        mac.unsettled = this.unsettled;
        mac.active = this.active;
        mac.minorstorm = this.minorstorm;
        mac.moderatestorm = this.moderatestorm;
        mac.strongstorm = this.strongstorm;
        mac.severestorm = this.severestorm;
        mac.extremestorm = this.extremestorm;
        mac.kIndicesTextColors = this.kIndicesTextColors;
        mac.nonNetwrkStnColor = this.nonNetwrkStnColor;
        mac.markerSize = this.markerSize;
        mac.markerTextSize = this.markerTextSize;
        return mac;
    }

}
