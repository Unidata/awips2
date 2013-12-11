package com.raytheon.viz.volumebrowser.xml;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.ViewMenu;

/**
 * 
 * Description of a single source for the volume browser.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- -----------------------------------------
 * Jan 06, 2011           bsteffen    Initial creation
 * Dec 11, 2013  2602     bsteffen    Remove ISerializableObject.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 * @see VbSourceList
 */
@XmlAccessorType(XmlAccessType.NONE)
public class VbSource {

    @XmlAttribute(required = true)
    private String key;

    @XmlAttribute(required = false)
    private String name;

    @XmlAttribute(required = true)
    private String category;

    @XmlAttribute(required = false)
    private List<ViewMenu> views;

    @XmlAttribute(required = false)
    private String subCategory;

    /**
     * @return the key
     */
    public String getKey() {
        return key;
    }

    /**
     * @param key
     *            the key to set
     */
    public void setKey(String key) {
        this.key = key;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return the category
     */
    public String getCategory() {
        return category;
    }

    /**
     * @param category
     *            the category to set
     */
    public void setCategory(String category) {
        this.category = category;
    }

    /**
     * @return the views
     */
    public List<ViewMenu> getViews() {
        return views;
    }

    /**
     * @param views
     *            the views to set
     */
    public void setViews(List<ViewMenu> views) {
        this.views = views;
    }

    /**
     * @return the subCategory
     */
    public String getSubCategory() {
        return subCategory;
    }

    /**
     * @param subCategory
     *            the subCategory to set
     */
    public void setSubCategory(String subCategory) {
        this.subCategory = subCategory;
    }

}
