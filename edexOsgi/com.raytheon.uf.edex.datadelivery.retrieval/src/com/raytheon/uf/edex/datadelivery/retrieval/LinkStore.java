package com.raytheon.uf.edex.datadelivery.retrieval;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.XmlGenericMapAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * link Store Object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 17, 2011    218      dhladky     Initial creation
 * Sep 10, 2012 1154        djohnson    Add JAXB annotations.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@XmlRootElement(name = "linkStore")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class LinkStore implements ISerializableObject {

    private static final Pattern LINK_KEY_PATTERN = Pattern
            .compile(".*/([^\\.]+)(\\..+)?");

    @XmlJavaTypeAdapter(type = Map.class, value = XmlGenericMapAdapter.class)
    @DynamicSerializeElement
    private Map<String, Link> links;

    @XmlAttribute(name = "dateString")
    @DynamicSerializeElement
    private String dateString;

    @XmlAttribute(name = "date")
    @DynamicSerializeElement
    private Date date;

    @XmlAttribute(name = "creationTime")
    @DynamicSerializeElement
    private long creationTime = System.currentTimeMillis();

    public LinkStore() {
        this(null, null);
    }

    public LinkStore(String dateString, Date date) {
        this.dateString = dateString;
        this.date = date;
        links = new HashMap<String, Link>();
    }

    public Set<String> getLinkKeys() {
        return links.keySet();
    }

    public void addLink(String url, Link link) {

        String linkKey = getLinkKey(url);
        links.put(linkKey, link);
    }

    public void removeLink(String linkKey) {

        links.remove(linkKey);
    }

    public Link getLink(String linkKey) {
        return links.get(linkKey);
    }

    public boolean containsKey(String linkKey) {
        return links.containsKey(linkKey);
    }

    @VisibleForTesting
    static String getLinkKey(String url) {
        Matcher m = LINK_KEY_PATTERN.matcher(url);
        if (m.find()) {
            return m.group(1);
        }
        return url;
    }

    public Map<String, Link> getLinks() {
        return links;
    }

    public void setLinks(Map<String, Link> links) {
        this.links = links;
    }

    public String getDateString() {
        return dateString;
    }

    public void setDateString(String dateString) {
        this.dateString = dateString;
    }

    public Date getDate() {
        return date;
    }

    public void setDate(Date date) {
        this.date = date;
    }

    /**
     * @return the creationTime
     */
    public long getCreationTime() {
        return creationTime;
    }

    /**
     * @param creationTime
     *            the creationTime to set
     */
    public void setCreationTime(long creationTime) {
        this.creationTime = creationTime;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof LinkStore) {
            LinkStore other = (LinkStore) obj;

            EqualsBuilder eqBuilder = new EqualsBuilder();
            eqBuilder.append(this.creationTime, other.creationTime);
            eqBuilder.append(this.date, other.date);
            eqBuilder.append(this.dateString, other.dateString);
            eqBuilder.append(this.links, other.links);

            return eqBuilder.isEquals();
        }
        return super.equals(obj);
    }

    @Override
    public int hashCode() {
        HashCodeBuilder hcBuilder = new HashCodeBuilder();
        hcBuilder.append(this.creationTime);
        hcBuilder.append(this.date);
        hcBuilder.append(this.dateString);
        hcBuilder.append(this.links);

        return hcBuilder.toHashCode();
    }
}
