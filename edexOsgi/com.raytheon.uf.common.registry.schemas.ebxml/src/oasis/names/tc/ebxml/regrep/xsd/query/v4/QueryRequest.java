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

import javax.persistence.CascadeType;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryRequestType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.raytheon.uf.common.registry.RegrepUtil;
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
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2012                     bphillip    Initial implementation
 * 10/17/2013    1682       bphillip    Added software history
 * 10/23/2013    1538       bphillip    Removed unused constructors
 * 12/2/2013     1829       bphillip    Added Hibernate annotations
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = { "responseOption", "query" })
@XmlRootElement(name = "QueryRequest")
@DynamicSerialize
@Entity
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(schema = RegrepUtil.EBXML_SCHEMA, name = "QueryRequest")
public class QueryRequest extends RegistryRequestType {

    private static final long serialVersionUID = 1069976228999283284L;

    /** The queryID canonical query parameter name */
    public static final String QUERY_ID = "queryId";

    /** The depth canonical query parameter name */
    public static final String DEPTH = "depth";

    /** The format canonical query parameter name */
    public static final String FORMAT = "format";

    /** The federated canonical query parameter name */
    public static final String FEDERATED = "federated";

    /** The federation canonical query parameter name */
    public static final String FEDERATION = "federation";

    /** The matchOlderVersion canonical query parameter name */
    public static final String MATCH_OLDER_VERSIONS = "matchOlderVersions";

    /** The startIndex canonical query parameter name */
    public static final String START_INDEX = "startIndex";

    /** The lang canonical query parameter name */
    public static final String LANG = "lang";

    /** The maxResults canonical query parameter name */
    public static final String MAX_RESULTS = "maxResults";

    /** The responseOption query parameter name */
    public static final String RESPONSE_OPTION = "responseOption";

    /** The returnRequest query parameter */
    public static final String RETURN_REQUEST = "returnRequest";

    /** The default format for query responses */
    public static final String DEFAULT_RESPONSE_FORMAT = "application/ebrim+xml";

    public static final BigInteger DEFAULT_MAX_RESULTS = new BigInteger("-1");

    public static final BigInteger DEFAULT_START_INDEX = new BigInteger("0");

    public static final BigInteger DEFAULT_DEPTH = new BigInteger("0");

    public static final Boolean DEFAULT_MATCH_OLDER_VERSIONS = new Boolean(
            false);

    @XmlElement(name = "ResponseOption", required = true)
    @DynamicSerializeElement
    @Embedded
    protected ResponseOptionType responseOption;

    @XmlElement(name = "Query", required = true)
    @DynamicSerializeElement
    @OneToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "query_id", referencedColumnName = "id")
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

    public QueryRequest() {
        super();
    }

    public QueryRequest(String id, QueryType query,
            ResponseOptionType responseOption) {
        this.id = id;
        this.query = query;
        this.responseOption = responseOption;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("QueryRequest \n[comment=");
        builder.append(comment);
        builder.append(", \nid=");
        builder.append(id);
        builder.append(", \nslot=");
        builder.append(slot);
        builder.append(", \nresponseOption=");
        builder.append(responseOption);
        builder.append(", \nquery=");
        builder.append(query);
        builder.append(", \nfederated=");
        builder.append(federated);
        builder.append(", \nfederation=");
        builder.append(federation);
        builder.append(", \nformat=");
        builder.append(format);
        builder.append(", \nlang=");
        builder.append(lang);
        builder.append(", \nstartIndex=");
        builder.append(startIndex);
        builder.append(", \nmaxResults=");
        builder.append(maxResults);
        builder.append(", \ndepth=");
        builder.append(depth);
        builder.append(", \nmatchOlderVersions=");
        builder.append(matchOlderVersions);
        builder.append("]");
        return builder.toString();
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
            return DEFAULT_RESPONSE_FORMAT;
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
            return DEFAULT_START_INDEX;
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
            return DEFAULT_MAX_RESULTS;
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
            return DEFAULT_DEPTH;
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
            return DEFAULT_MATCH_OLDER_VERSIONS;
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

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((depth == null) ? 0 : depth.hashCode());
        result = prime * result
                + ((federated == null) ? 0 : federated.hashCode());
        result = prime * result
                + ((federation == null) ? 0 : federation.hashCode());
        result = prime * result + ((format == null) ? 0 : format.hashCode());
        result = prime * result + ((lang == null) ? 0 : lang.hashCode());
        result = prime
                * result
                + ((matchOlderVersions == null) ? 0 : matchOlderVersions
                        .hashCode());
        result = prime * result
                + ((maxResults == null) ? 0 : maxResults.hashCode());
        result = prime * result + ((query == null) ? 0 : query.hashCode());
        result = prime * result
                + ((responseOption == null) ? 0 : responseOption.hashCode());
        result = prime * result
                + ((startIndex == null) ? 0 : startIndex.hashCode());
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
        QueryRequest other = (QueryRequest) obj;
        if (depth == null) {
            if (other.depth != null)
                return false;
        } else if (!depth.equals(other.depth))
            return false;
        if (federated == null) {
            if (other.federated != null)
                return false;
        } else if (!federated.equals(other.federated))
            return false;
        if (federation == null) {
            if (other.federation != null)
                return false;
        } else if (!federation.equals(other.federation))
            return false;
        if (format == null) {
            if (other.format != null)
                return false;
        } else if (!format.equals(other.format))
            return false;
        if (lang == null) {
            if (other.lang != null)
                return false;
        } else if (!lang.equals(other.lang))
            return false;
        if (matchOlderVersions == null) {
            if (other.matchOlderVersions != null)
                return false;
        } else if (!matchOlderVersions.equals(other.matchOlderVersions))
            return false;
        if (maxResults == null) {
            if (other.maxResults != null)
                return false;
        } else if (!maxResults.equals(other.maxResults))
            return false;
        if (query == null) {
            if (other.query != null)
                return false;
        } else if (!query.equals(other.query))
            return false;
        if (responseOption == null) {
            if (other.responseOption != null)
                return false;
        } else if (!responseOption.equals(other.responseOption))
            return false;
        if (startIndex == null) {
            if (other.startIndex != null)
                return false;
        } else if (!startIndex.equals(other.startIndex))
            return false;
        return true;
    }

}
