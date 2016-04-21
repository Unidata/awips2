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
 * A turbulence layer found in a taf message.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 8/30/2006                bphillip    Initial Creation    
 * 6/21/2007                bphillip    Updated for use with plugin persistance pattern
 * Nov 01, 2013 2361        njensen     Remove XML annotations
 * May 15, 2014 3002        bgonzale    Moved to com.raytheon.uf.common.dataplugin.taf.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Entity
@Table(name = "taf_turbulence_layers")
@DynamicSerialize
public class TurbulenceLayer extends PersistableDataObject {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue
    private int id;

    @ManyToOne
    @JoinColumn(name = "parentID", nullable = false)
    private ChangeGroup parentID;

    /** The intensity of the turbulence */
    @DynamicSerializeElement
    @Column
    private Integer turbulence_intensity;

    /** The minimum altitude for the turbulence */
    @DynamicSerializeElement
    @Column
    private Integer turbulence_min_alt_ft_agl;

    /** The maximum altitude for the turbulence */
    @DynamicSerializeElement
    @Column
    private Integer turbulence_max_alt_ft_agl;

    public TurbulenceLayer() {
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
    public TurbulenceLayer(ChangeGroup parentID, String intensityVal,
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

    public ChangeGroup getParentID() {
        return parentID;
    }

    public void setParentID(ChangeGroup parentID) {
        this.parentID = parentID;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof TurbulenceLayer) {
            TurbulenceLayer layer = (TurbulenceLayer) obj;

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
