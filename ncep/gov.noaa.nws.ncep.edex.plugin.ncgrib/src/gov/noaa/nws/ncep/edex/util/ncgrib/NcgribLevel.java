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

package gov.noaa.nws.ncep.edex.util.ncgrib;

import javax.measure.unit.Unit;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import org.apache.commons.lang.builder.HashCodeBuilder;
import org.hibernate.annotations.Type;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Represents a grib level
 * <p>
 * Refer to the files ncgrib1levels.xml and ncgrib2levels.xml. This class maps to
 * the levels defined in those files.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 9/26/07      381         bphillip    Initial creation
 * 
 * 
 * </pre>
 */
@Entity
@Table(name = "ncgrib_levels")
public class NcgribLevel extends PersistableDataObject implements
        ISerializableObject {

    private static final long serialVersionUID = 1L;

    @Id
    private Integer id;

    @Column
    private Integer layerNumber;

    @Column
    private Integer edition;

    /** The name of the level */
    @Column(length = 127)
    private String name;

    /** The unit associated with the level */
    @Column
    @Type(type = "gov.noaa.nws.ncep.edex.objects.hibernate.ncgrib.UnitType")
    private Unit<?> unit;

    /** The number of surfaces described by this level */
    @Column(name = "layers")
    private int numLayers;

    public NcgribLevel() {
    }

    /**
     * Constructor
     * 
     * @param name
     *            The name of the level
     * @param unit
     *            The unit associated with the level
     * @param numLayers
     *            The number of surfaces described by this level
     */
    public NcgribLevel(String name, Unit<?> unit, int numLayers) {
        this.name = name;
        this.unit = unit;
        this.numLayers = numLayers;
    }

    /**
     * Gets the name
     * 
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the name
     * 
     * @param name
     *            The name
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Gets the unit
     * 
     * @return The unit
     */
    public Unit<?> getUnit() {
        return unit;
    }

    /**
     * Sets the unit
     * 
     * @param unit
     *            The unit
     */
    public void setUnit(Unit<?> unit) {
        this.unit = unit;
    }

    /**
     * Gets the number of layers
     * 
     * @return the number of layers
     */
    public int getNumLayers() {
        return numLayers;
    }

    /**
     * Sets the number of layers
     * 
     * @param numLayers
     *            the number of layers
     */
    public void setNumLayers(int numLayers) {
        this.numLayers = numLayers;
    }

    public Integer getEdition() {
        return edition;
    }

    public void setEdition(Integer edition) {
        this.edition = edition;
    }

    public Integer getNumber() {
        return layerNumber;
    }

    public void setNumber(Integer number) {
        this.layerNumber = number;
    }

    public boolean equals(Object obj) {
        if (obj instanceof NcgribLevel) {

            NcgribLevel level = (NcgribLevel) obj;
            if (this.layerNumber == level.layerNumber
                    && this.edition == level.edition
                    && this.unit.equals(level.unit)
                    && this.name.equals(level.name)
                    && this.numLayers == level.numLayers) {
                return true;

            } else {
                return false;
            }
        } else {
            return false;
        }
    }

    public int hashCode() {
        return new HashCodeBuilder(17, 37).append(layerNumber).append(edition)
                .append(name).append(unit).append(numLayers).toHashCode();
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public Integer getLayerNumber() {
        return layerNumber;
    }

    public void setLayerNumber(Integer layerNumber) {
        this.layerNumber = layerNumber;
    }
}
