/**
 * This software was modified from Raytheon's taf plugin by
 * NOAA/NWS/NCEP/NCO in order to output point data in HDF5.
 **/

package gov.noaa.nws.ncep.common.dataplugin.nctaf;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import org.apache.commons.lang.builder.HashCodeBuilder;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * A turbulence layer found in a taf message.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 09/09/2011   458			sgurung	    Initial Creation from Raytheon's taf plugin
 * 09/23/2011   458			sgurung	    Converted to HDF5
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NcTafTurbulenceLayer extends PersistableDataObject implements
        ISerializableObject {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue
    private int id;

    @ManyToOne
    @JoinColumn(name = "parentID", nullable = false)
    private NcTafChangeGroup parentID;

    /** The intensity of the turbulence */
    @DynamicSerializeElement
    @XmlElement
    private Integer turbulence_intensity;

    /** The minimum altitude for the turbulence */
    @DynamicSerializeElement
    @XmlElement
    private Integer turbulence_min_alt_ft_agl;

    /** The maximum altitude for the turbulence */
    @DynamicSerializeElement
    @XmlElement
    private Integer turbulence_max_alt_ft_agl;

    public NcTafTurbulenceLayer() {
    }

    /**
     * Constructor for new TurbulenceLayer
     * 
     * @param intensityVal
     *            The intensity of the turbulence
     * @param baseLayer
     *            The lower altitude of the turbulence
     * @param thicknessIce
     *            The thickness of the turbulence
     */
    public NcTafTurbulenceLayer(NcTafChangeGroup parentID, String intensityVal,
            String baseLayer, String thicknessIce) {
        this.parentID = parentID;
        this.turbulence_intensity = Integer.parseInt(intensityVal);
        this.turbulence_min_alt_ft_agl = Integer.parseInt(baseLayer + "00");
        this.turbulence_max_alt_ft_agl = turbulence_min_alt_ft_agl
                + Integer.parseInt(thicknessIce) * 1000;
    }

    /**
     * Converts a TurbulenceLayer object to a String
     */
    @Override
    public String toString() {
        return "Turbulence Layer->Int: " + turbulence_intensity + "ft Base: "
                + turbulence_min_alt_ft_agl + " Top: "
                + turbulence_max_alt_ft_agl + " ft.";
    }

    public Integer getTurbulence_intensity() {
        return turbulence_intensity;
    }

    public void setTurbulence_intensity(Integer turbulence_intensity) {
        this.turbulence_intensity = turbulence_intensity;
    }

    public Integer getTurbulence_min_alt_ft_agl() {
        return turbulence_min_alt_ft_agl;
    }

    public void setTurbulence_min_alt_ft_agl(Integer turbulence_min_alt_ft_agl) {
        this.turbulence_min_alt_ft_agl = turbulence_min_alt_ft_agl;
    }

    public Integer getTurbulence_max_alt_ft_agl() {
        return turbulence_max_alt_ft_agl;
    }

    public void setTurbulence_max_alt_ft_agl(Integer turbulence_max_alt_ft_agl) {
        this.turbulence_max_alt_ft_agl = turbulence_max_alt_ft_agl;
    }

    public NcTafChangeGroup getParentID() {
        return parentID;
    }

    public void setParentID(NcTafChangeGroup parentID) {
        this.parentID = parentID;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof NcTafTurbulenceLayer) {
            NcTafTurbulenceLayer layer = (NcTafTurbulenceLayer) obj;

            if (this.parentID != layer.parentID) {
                return false;
            }

            if (!(this.turbulence_intensity == null ? layer
                    .getTurbulence_intensity() == null
                    : this.turbulence_intensity.equals(layer
                            .getTurbulence_intensity()))) {
                return false;
            }

            if (!(this.turbulence_min_alt_ft_agl == null ? layer
                    .getTurbulence_min_alt_ft_agl() == null
                    : this.turbulence_min_alt_ft_agl.equals(layer
                            .getTurbulence_min_alt_ft_agl()))) {
                return false;
            }

            if (!(this.turbulence_max_alt_ft_agl == null ? layer
                    .getTurbulence_max_alt_ft_agl() == null
                    : this.turbulence_max_alt_ft_agl.equals(layer
                            .getTurbulence_max_alt_ft_agl()))) {
                return false;
            }

            return true;

        } else {
            return false;
        }

    }

    @Override
    public int hashCode() {

        return new HashCodeBuilder(17, 37).append(parentID).append(
                this.turbulence_intensity).append(
                this.turbulence_min_alt_ft_agl).append(
                this.turbulence_max_alt_ft_agl).toHashCode();
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }
}
