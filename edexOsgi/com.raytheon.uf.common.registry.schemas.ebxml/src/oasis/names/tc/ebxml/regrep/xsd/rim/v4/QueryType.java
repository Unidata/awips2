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

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.raytheon.uf.common.registry.RegrepUtil;
import com.raytheon.uf.common.registry.schemas.ebxml.util.annotations.RegistryObjectReference;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Represents invocation of a parameterized query.
 * 
 * 
 * <p>
 * Java class for QueryType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="QueryType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ExtensibleObjectType">
 *       &lt;attribute name="queryDefinition" use="required" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}objectReferenceType" />
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
 * 2012                     bphillip    Initial implementation
 * 10/17/2013    1682       bphillip    Added software history
 * 12/2/2013     1829       bphillip    Made ExtensibleObjectType persistable, 
 *                                      modified persistence annotations, added 
 *                                      constructors, hashCode, toString and equals
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement(name = "Query")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "QueryType")
@DynamicSerialize
@Entity
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(schema = RegrepUtil.EBXML_SCHEMA, name = "Query")
public class QueryType extends ExtensibleObjectType {

    private static final long serialVersionUID = -8618442367033794467L;

    @XmlAttribute(required = true)
    @DynamicSerializeElement
    @RegistryObjectReference
    protected String queryDefinition;

    public QueryType() {
        super();
    }

    public QueryType(String queryDefinition, List<SlotType> slots) {
        super(queryDefinition, slots);
        this.queryDefinition = queryDefinition;
    }

    public QueryType(String queryDefinition, SlotType... slots) {
        this.queryDefinition = queryDefinition;
        for (SlotType slot : slots) {
            this.getSlot().add(slot);
        }
    }

    /**
     * Gets the value of the queryDefinition property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getQueryDefinition() {
        return queryDefinition;
    }

    /**
     * Sets the value of the queryDefinition property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setQueryDefinition(String value) {
        this.id = value;
        this.queryDefinition = value;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result
                + ((queryDefinition == null) ? 0 : queryDefinition.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        QueryType other = (QueryType) obj;
        if (queryDefinition == null) {
            if (other.queryDefinition != null)
                return false;
        } else if (!queryDefinition.equals(other.queryDefinition))
            return false;
        return true;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("QueryType \n[id=");
        builder.append(id);
        builder.append(", \nslot=");
        builder.append(slot);
        builder.append(", \nqueryDefinition=");
        builder.append(queryDefinition);
        builder.append("]");
        return builder.toString();
    }

}
