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

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.raytheon.uf.common.registry.RegrepUtil;
import com.raytheon.uf.common.registry.schemas.ebxml.util.annotations.RegistryObjectReference;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Represents a query expression in a specified query language that MAY be used
 * by the server to invoke a query. The QueryExpressionType is the abstract root
 * of a type hierarchy for more specialized sub-types.
 * 
 * 
 * <p>
 * Java class for QueryExpressionType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="QueryExpressionType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ExtensibleObjectType">
 *       &lt;attribute name="queryLanguage" use="required" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}objectReferenceType" />
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
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement(name = "QueryExpression")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "QueryExpressionType")
@XmlSeeAlso({ StringQueryExpressionType.class, XMLQueryExpressionType.class })
@DynamicSerialize
@Entity
@Table(schema = RegrepUtil.EBXML_SCHEMA, name = "QueryExpression")
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public class QueryExpressionType extends ExtensibleObjectType {

    @Id
    @SequenceGenerator(name = "QueryExpressionTypeGenerator", schema = RegrepUtil.EBXML_SCHEMA, sequenceName = RegrepUtil.EBXML_SCHEMA
            + ".QueryExpression_sequence")
    @GeneratedValue(generator = "QueryExpressionTypeGenerator")
    @XmlTransient
    protected Integer key;

    @XmlAttribute(required = true)
    @DynamicSerializeElement
    @RegistryObjectReference
    protected String queryLanguage;

    public QueryExpressionType() {

    }

    public QueryExpressionType(String queryLanguage) {
        super();
        this.queryLanguage = queryLanguage;
    }

    /**
     * Gets the value of the queryLanguage property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getQueryLanguage() {
        return queryLanguage;
    }

    /**
     * Sets the value of the queryLanguage property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setQueryLanguage(String value) {
        this.queryLanguage = value;
    }

}
