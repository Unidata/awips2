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

package gov.noaa.nws.ncep.common.dataplugin.ncgrib;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Class encapsulating information about the generating process extracted from
 * the PDS section of the grib file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/7/09       1994        bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Entity
@Table(name = "ncgrib_genprocess")
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NcGenProcess implements ISerializableObject {

    /**
     * A generated identification number to uniquely identify this generating
     * process
     */
    @Id
    private int id;

    /** The originating center which defined this generating process */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private int center;

    /** The originating subcenter which defined this generating process */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private int subcenter;

    /** The number of the generating process */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private int number;

    /** The short name of this generating process */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private String name;

    /** Description of the generating process */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private String description;

    /**
     * Creates an empty GenProcess object
     */
    public NcGenProcess() {

    }

    /**
     * Creates a GenProcess object with provided information
     * 
     * @param center
     *            The center associated with this generating process
     * @param number
     *            The number of the generating process
     * @param name
     *            The short name of the generating process
     * @param description
     *            The description of the generating process
     */
    public NcGenProcess(int center, int subcenter, int number, String name, String description) {
        this.center = center;
        this.subcenter = subcenter;
        this.number = number;
        this.name = name;
        this.description = description;
        id = hashCode();
    }

    public String toString() {
        StringBuffer sb = new StringBuffer();
        sb.append("         id: ").append(id).append("\n");
        sb.append("     center: ").append(center).append("\n");
        sb.append("     number: ").append(number).append("\n");
        sb.append("       name: ").append(name).append("\n");
        sb.append("description: ").append(description).append("\n");
        return sb.toString();
    }

    public int hashCode() {
        final int PRIME = 31;
        int result = 1;
        result = PRIME * result + center;
        result = PRIME * result + number;
        result = PRIME * result + ((name == null) ? 0 : name.hashCode());
        result = PRIME * result
                + ((description == null) ? 0 : description.hashCode());
        return result;
    }

    /**
     * Gets the center
     * 
     * @return The center id
     */
    public int getCenter() {
        return center;
    }

    /**
     * Sets the center id
     * 
     * @param center
     *            The center id
     */
    public void setCenter(int center) {
        this.center = center;
    }

    /**
     * Gets the generating process number
     * 
     * @return The generating process number
     */
    public int getNumber() {
        return number;
    }

    /**
     * Sets the generating process number
     * 
     * @param number
     *            The generating process number
     */
    public void setNumber(int number) {
        this.number = number;
    }

    /**
     * Gets the short name of the generating process
     * 
     * @return The name
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the short name of the generating process
     * 
     * @param name
     *            The name
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Gets the description of the generating process
     * 
     * @return The description
     */
    public String getDescription() {
        return description;
    }

    /**
     * Sets the description of the generating process
     * 
     * @param description
     *            The description
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * Gets the id
     * 
     * @return The id
     */
    public int getId() {
        return id;
    }

    /**
     * Sets the id
     * 
     * @param id
     *            The id
     */
    public void setId(int id) {
        this.id = id;
    }

    /**
     * Gets the subcenter id
     * 
     * @return The subcenter id
     */
    public int getSubcenter() {
        return subcenter;
    }

    /**
     * Sets the subcenter id
     * 
     * @param subcenter
     *            The subcenter id
     */
    public void setSubcenter(int subcenter) {
        this.subcenter = subcenter;
    }

}
