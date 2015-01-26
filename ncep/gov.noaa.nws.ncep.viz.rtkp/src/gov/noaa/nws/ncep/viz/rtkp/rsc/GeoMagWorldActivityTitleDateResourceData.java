/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
package gov.noaa.nws.ncep.viz.rtkp.rsc;

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
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 08/26/14     #4078       Shova Gurung Initial Creation.
 * 
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "geoMagWorldActivityTitleDateResourceData")
public class GeoMagWorldActivityTitleDateResourceData extends
        AbstractNatlCntrsResourceData {

    @XmlElement
    private Integer fontSize;

    @XmlElement
    private String fontName;

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB color = null;

    private RGB dfltColor = new RGB(255, 255, 255);

    private String title = null;

    @Override
    public AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, IDescriptor descriptor)
            throws VizException {
        return new GeoMagWorldActivityTitleDateResource(this, loadProperties);
    }

    public RGB getColor() {
        return (color == null ? dfltColor : color);
    }

    public void setColor(RGB _color) {
        color = _color;
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

    // default no-arg required to serialize
    public GeoMagWorldActivityTitleDateResourceData() {
        title = "Geomagnetic Activity Map";
    }

    public GeoMagWorldActivityTitleDateResourceData(String title) {
        this.title = title;
    }

    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        return new GeoMagWorldActivityTitleDateResource(this, loadProperties);
    }

    @Override
    public void update(Object updateData) {
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    @Override
    public boolean equals(Object obj) {
        if (!super.equals(obj)) {
            return false;
        }

        if (obj instanceof GeoMagWorldActivityTitleDateResourceData == false) {
            return false;
        }

        GeoMagWorldActivityTitleDateResourceData other = (GeoMagWorldActivityTitleDateResourceData) obj;
        if (this.title == other.getTitle()
                && this.getFontName() == other.getFontName()
                && this.getFontSize() == other.getFontSize()
                && this.getColor() == other.getColor()) {
            return true;

        }

        return false;
    }
}
