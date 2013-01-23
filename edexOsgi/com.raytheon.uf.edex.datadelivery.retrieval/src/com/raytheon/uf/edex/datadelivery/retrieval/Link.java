package com.raytheon.uf.edex.datadelivery.retrieval;

import java.util.HashMap;
import java.util.Map;

import javax.persistence.Transient;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Link object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 17, 2011    218      dhladky     Initial creation
 * Jul 24, 2012    955      djohnson    Use Map instead of HashMap.
 * Sep 10, 2012 1154        djohnson    Add JAXB annotations.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement(name = "link")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class Link implements ISerializableObject {

    @XmlAttribute(name = "name")
    @DynamicSerializeElement
    private String name;

    @XmlAttribute(name = "url")
    @DynamicSerializeElement
    private String url;

    @Transient
    private Map<String, Object> links = new HashMap<String, Object>();

    public Link() {

    }

    public Link(String name, String url) {
        this.url = url;
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public Map<String, Object> getLinks() {
        return links;
    }

    public void setLinks(Map<String, Object> links) {
        this.links = links;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Link) {
            Link other = (Link) obj;

            EqualsBuilder eqBuilder = new EqualsBuilder();
            eqBuilder.append(this.name, other.name);
            eqBuilder.append(this.url, other.url);

            return eqBuilder.isEquals();
        }
        return super.equals(obj);
    }

    @Override
    public int hashCode() {
        HashCodeBuilder hcBuilder = new HashCodeBuilder();
        hcBuilder.append(this.name);
        hcBuilder.append(this.url);

        return hcBuilder.toHashCode();
    }
}
