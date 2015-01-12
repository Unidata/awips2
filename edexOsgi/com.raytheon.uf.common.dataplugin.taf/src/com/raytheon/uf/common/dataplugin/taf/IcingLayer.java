/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

package com.raytheon.uf.common.dataplugin.taf;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.apache.commons.lang.builder.HashCodeBuilder;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
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
 * 8/30/06                  bphillip    Initial Creation
 * 6/21/07      180         bphillip    Updated for use with plugin persistance pattern
 * Nov 01, 2013 2361        njensen     Remove XML annotations
 * May 15, 2014 3002        bgonzale    Moved to com.raytheon.uf.common.dataplugin.taf.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Entity
@Table(name = "taf_icing_layers")
@DynamicSerialize
public class IcingLayer extends PersistableDataObject {

    @Id
    @GeneratedValue
    private int id;

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    @ManyToOne
    @JoinColumn(name = "parentID", nullable = false)
    private ChangeGroup parentID;

    /** The icing intensity */
    @DynamicSerializeElement
    @Column
    private Integer icing_intensity;

    /** The minimum altitude for the icing */
    @DynamicSerializeElement
    @Column
    private Integer icing_min_alt_ft_agl;

    /** The maximum altitude for the icing */
    @DynamicSerializeElement
    @Column
    private Integer icing_max_alt_ft_agl;

    public IcingLayer() {

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
    public IcingLayer(ChangeGroup parentid, String intensityVal,
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

    public ChangeGroup getParentID() {
        return parentID;
    }

    public void setParentID(ChangeGroup parentID) {
        this.parentID = parentID;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IcingLayer) {
            IcingLayer layer = (IcingLayer) obj;

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
