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
 * An icing layer found in a TAF message.
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
public class NcTafIcingLayer extends PersistableDataObject implements
        ISerializableObject {

    @Id
    @GeneratedValue
    private int id;

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    @ManyToOne
    @JoinColumn(name = "parentID", nullable = false)
    private NcTafChangeGroup parentID;

    /** The icing intensity */
    @DynamicSerializeElement
    @XmlElement
    private Integer icing_intensity;

    /** The minimum altitude for the icing */
    @DynamicSerializeElement
    @XmlElement
     private Integer icing_min_alt_ft_agl;

    /** The maximum altitude for the icing */
    @DynamicSerializeElement
    @XmlElement
    private Integer icing_max_alt_ft_agl;

    public NcTafIcingLayer() {

    }

    /**
     * Constructor for IcingLayer
     * 
     * @param intensityVal
     *            The intensity of the icing layer
     * @param baseLayer
     *            The base altitude of the icing layer
     * @param thicknessIce
     *            The thickness of the icing layer
     */
    public NcTafIcingLayer(NcTafChangeGroup parentid, String intensityVal,
            String baseLayer, String thicknessIce) {
        this.parentID = parentid;
        this.icing_intensity = Integer.parseInt(intensityVal);
        this.icing_min_alt_ft_agl = Integer.parseInt(baseLayer + "00");
        this.icing_max_alt_ft_agl = icing_min_alt_ft_agl
                + Integer.parseInt(thicknessIce) * 1000;
    }

    /**
     * Converts an IcingLayer object to a String
     */
    @Override
    public String toString() {

        return "\nIcing Layer ->Intensity: " + icing_intensity + " Min: "
                + icing_min_alt_ft_agl + " ft. Max: " + icing_max_alt_ft_agl
                + " ft.\n";

    }

    public Integer getIcing_intensity() {
        return icing_intensity;
    }

    public void setIcing_intensity(Integer icing_intensity) {
        this.icing_intensity = icing_intensity;
    }

    public Integer getIcing_min_alt_ft_agl() {
        return icing_min_alt_ft_agl;
    }

    public void setIcing_min_alt_ft_agl(Integer icing_min_alt_ft_agl) {
        this.icing_min_alt_ft_agl = icing_min_alt_ft_agl;
    }

    public Integer getIcing_max_alt_ft_agl() {
        return icing_max_alt_ft_agl;
    }

    public void setIcing_max_alt_ft_agl(Integer icing_max_alt_ft_agl) {
        this.icing_max_alt_ft_agl = icing_max_alt_ft_agl;
    }

    public NcTafChangeGroup getParentID() {
        return parentID;
    }

    public void setParentID(NcTafChangeGroup parentID) {
        this.parentID = parentID;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof NcTafIcingLayer) {
            NcTafIcingLayer layer = (NcTafIcingLayer) obj;

            if (this.parentID != layer.parentID) {
                return false;
            }

            if (!(this.icing_intensity == null ? layer.getIcing_intensity() == null
                    : this.icing_intensity.equals(layer.getIcing_intensity()))) {
                return false;
            }

            if (!(this.icing_min_alt_ft_agl == null ? layer
                    .getIcing_min_alt_ft_agl() == null
                    : this.icing_min_alt_ft_agl.equals(layer
                            .getIcing_min_alt_ft_agl()))) {
                return false;
            }

            if (!(this.icing_max_alt_ft_agl == null ? layer
                    .getIcing_max_alt_ft_agl() == null
                    : this.icing_max_alt_ft_agl.equals(layer
                            .getIcing_max_alt_ft_agl()))) {
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
                this.icing_intensity).append(this.icing_min_alt_ft_agl).append(
                this.icing_max_alt_ft_agl).toHashCode();
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

}
