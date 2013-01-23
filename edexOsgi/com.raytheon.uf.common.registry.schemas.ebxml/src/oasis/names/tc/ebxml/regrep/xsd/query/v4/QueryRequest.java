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

package oasis.names.tc.ebxml.regrep.xsd.query.v4;

import java.math.BigInteger;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryRequestType;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * <p>
 * Java class for anonymous complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType>
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rs:4.0}RegistryRequestType">
 *       &lt;sequence>
 *         &lt;element name="ResponseOption" type="{urn:oasis:names:tc:ebxml-regrep:xsd:query:4.0}ResponseOptionType"/>
 *         &lt;element name="Query" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}QueryType"/>
 *       &lt;/sequence>
 *       &lt;attribute name="federated" type="{http://www.w3.org/2001/XMLSchema}boolean" default="false" />
 *       &lt;attribute name="federation" type="{http://www.w3.org/2001/XMLSchema}anyURI" />
 *       &lt;attribute name="format" type="{http://www.w3.org/2001/XMLSchema}string" default="application/ebrim+xml" />
 *       &lt;attribute ref="{http://www.w3.org/XML/1998/namespace}lang"/>
 *       &lt;attribute name="startIndex" type="{http://www.w3.org/2001/XMLSchema}integer" default="0" />
 *       &lt;attribute name="maxResults" type="{http://www.w3.org/2001/XMLSchema}integer" default="-1" />
 *       &lt;attribute name="depth" type="{http://www.w3.org/2001/XMLSchema}integer" default="0" />
 *       &lt;attribute name="matchOlderVersions" type="{http://www.w3.org/2001/XMLSchema}boolean" default="false" />
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = { "responseOption", "query" })
@XmlRootElement(name = "QueryRequest")
@DynamicSerialize
public class QueryRequest extends RegistryRequestType {

    @XmlElement(name = "ResponseOption", required = true)
    @DynamicSerializeElement
    protected ResponseOptionType responseOption;

    @XmlElement(name = "Query", required = true)
    @DynamicSerializeElement
    protected QueryType query;

    @XmlAttribute
    @DynamicSerializeElement
    protected Boolean federated;

    @XmlAttribute
    @XmlSchemaType(name = "anyURI")
    @DynamicSerializeElement
    protected String federation;

    @XmlAttribute
    @DynamicSerializeElement
    protected String format;

    @XmlAttribute(namespace = "http://www.w3.org/XML/1998/namespace")
    @DynamicSerializeElement
    protected String lang;

    @XmlAttribute
    @DynamicSerializeElement
    protected BigInteger startIndex;

    @XmlAttribute
    @DynamicSerializeElement
    protected BigInteger maxResults;

    @XmlAttribute
    @DynamicSerializeElement
    protected BigInteger depth;

    @XmlAttribute
    @DynamicSerializeElement
    protected Boolean matchOlderVersions;

    @Override
    public String toString() {
        StringBuilder strBuilder = new StringBuilder();
        strBuilder.append("\tQueryType: [").append(query.getQueryDefinition())
                .append("]");
        strBuilder.append("\tId: [").append(id).append("]");
        strBuilder.append("\tFederated: [").append(federated).append("]");
        strBuilder.append("\tFederation: [").append(federation).append("]");
        strBuilder.append("\tFormat: [").append(format).append("]");
        strBuilder.append("\tLang: [").append(lang).append("]");
        strBuilder.append("\tStart Index: [").append(startIndex).append("]");
        strBuilder.append("\tMax Results: [").append(maxResults).append("]");
        strBuilder.append("\tDepth: [").append(depth).append("]");
        strBuilder.append("\tMatch Older Versions: [")
                .append(matchOlderVersions).append("]");
        return strBuilder.toString();
    }

    /**
     * Gets the value of the responseOption property.
     * 
     * @return possible object is {@link ResponseOptionType }
     * 
     */
    public ResponseOptionType getResponseOption() {
        return responseOption;
    }

    /**
     * Sets the value of the responseOption property.
     * 
     * @param value
     *            allowed object is {@link ResponseOptionType }
     * 
     */
    public void setResponseOption(ResponseOptionType value) {
        this.responseOption = value;
    }

    /**
     * Gets the value of the query property.
     * 
     * @return possible object is {@link QueryType }
     * 
     */
    public QueryType getQuery() {
        return query;
    }

    /**
     * Sets the value of the query property.
     * 
     * @param value
     *            allowed object is {@link QueryType }
     * 
     */
    public void setQuery(QueryType value) {
        this.query = value;
    }

    /**
     * Gets the value of the federated property.
     * 
     * @return possible object is {@link Boolean }
     * 
     */
    public boolean isFederated() {
        if (federated == null) {
            return false;
        } else {
            return federated;
        }
    }

    /**
     * Sets the value of the federated property.
     * 
     * @param value
     *            allowed object is {@link Boolean }
     * 
     */
    public void setFederated(Boolean value) {
        this.federated = value;
    }

    /**
     * Gets the value of the federation property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getFederation() {
        return federation;
    }

    /**
     * Sets the value of the federation property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setFederation(String value) {
        this.federation = value;
    }

    /**
     * Gets the value of the format property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getFormat() {
        if (format == null) {
            return "application/ebrim+xml";
        } else {
            return format;
        }
    }

    /**
     * Sets the value of the format property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setFormat(String value) {
        this.format = value;
    }

    /**
     * Gets the value of the lang property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getLang() {
        return lang;
    }

    /**
     * Sets the value of the lang property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setLang(String value) {
        this.lang = value;
    }

    /**
     * Gets the value of the startIndex property.
     * 
     * @return possible object is {@link BigInteger }
     * 
     */
    public BigInteger getStartIndex() {
        if (startIndex == null) {
            return new BigInteger("0");
        } else {
            return startIndex;
        }
    }

    /**
     * Sets the value of the startIndex property.
     * 
     * @param value
     *            allowed object is {@link BigInteger }
     * 
     */
    public void setStartIndex(BigInteger value) {
        this.startIndex = value;
    }

    /**
     * Gets the value of the maxResults property.
     * 
     * @return possible object is {@link BigInteger }
     * 
     */
    public BigInteger getMaxResults() {
        if (maxResults == null) {
            return new BigInteger("-1");
        } else {
            return maxResults;
        }
    }

    /**
     * Sets the value of the maxResults property.
     * 
     * @param value
     *            allowed object is {@link BigInteger }
     * 
     */
    public void setMaxResults(BigInteger value) {
        this.maxResults = value;
    }

    /**
     * Gets the value of the depth property.
     * 
     * @return possible object is {@link BigInteger }
     * 
     */
    public BigInteger getDepth() {
        if (depth == null) {
            return new BigInteger("0");
        } else {
            return depth;
        }
    }

    /**
     * Sets the value of the depth property.
     * 
     * @param value
     *            allowed object is {@link BigInteger }
     * 
     */
    public void setDepth(BigInteger value) {
        this.depth = value;
    }

    /**
     * Gets the value of the matchOlderVersions property.
     * 
     * @return possible object is {@link Boolean }
     * 
     */
    public boolean isMatchOlderVersions() {
        if (matchOlderVersions == null) {
            return false;
        } else {
            return matchOlderVersions;
        }
    }

    /**
     * Sets the value of the matchOlderVersions property.
     * 
     * @param value
     *            allowed object is {@link Boolean }
     * 
     */
    public void setMatchOlderVersions(Boolean value) {
        this.matchOlderVersions = value;
    }

}
