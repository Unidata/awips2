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

package oasis.names.tc.ebxml.regrep.xsd.rim.v4;

import java.util.List;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlSeeAlso;
import jakarta.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 *
 * Common base type for all types that have unique identity and support
 * extensibility via slots.
 *
 *
 * <p>
 * Java class for IdentifiableType complex type.
 *
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 *
 * <pre>
 * &lt;complexType name="IdentifiableType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ExtensibleObjectType">
 *       &lt;attribute name="id" use="required" type="{http://www.w3.org/2001/XMLSchema}string" />
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10/27/2020    8170     ksunil    Removed empty tables. New class to support non JPA activity.
 * </pre>
 *
 * @author ksunil
 */
@XmlRootElement(name = "Identifiable")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "IdentifiableTypeNonJPA")
@XmlSeeAlso({ RegistryObjectType.class })
@DynamicSerialize
public abstract class IdentifiableTypeNonJPA
        extends ExtensibleObjectTypeNonJPA {

    private static final long serialVersionUID = -4589394594867000988L;

    protected IdentifiableTypeNonJPA() {
        super();
    }

    protected IdentifiableTypeNonJPA(List<SlotType> slots, String id) {
        super(id, slots);
    }

    /**
     * Gets the value of the id property.
     *
     * @return possible object is {@link String }
     *
     */
    @Override
    @XmlAttribute(required = true)
    public String getId() {
        return id;
    }

    @Override
    public void setId(String id) {
        super.setId(id);
    }

}
