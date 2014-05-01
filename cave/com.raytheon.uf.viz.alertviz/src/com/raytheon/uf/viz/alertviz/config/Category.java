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
package com.raytheon.uf.viz.alertviz.config;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Provides customization for a particular category of data
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 18, 2008 1433       chammack    Initial creation
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class Category implements ISerializableObject {

    /**
     * Is the category locked (i.e. not deleteable)
     */
    @XmlAttribute
    private boolean locked;

    /**
     * The category name
     */
    @XmlAttribute
    private String categoryName;

    /**
     * The description
     */
    @XmlAttribute
    private String longName;

    /**
     * The box to put the message into
     */
    @XmlAttribute
    private int textBox;

    public Category() {
    }

    public Category(String categoryName, String longName, int textBox) {
        this.categoryName = categoryName;
        this.longName = longName;
        this.textBox = textBox;
        locked = false;
    }

    /**
     * @return the locked
     */
    public boolean isLocked() {
        return locked;
    }

    /**
     * @param locked
     *            the locked to set
     */
    public void setLocked(boolean locked) {
        this.locked = locked;
    }

    /**
     * @return the categoryName
     */
    public String getCategoryName() {
        return categoryName;
    }

    /**
     * @param categoryName
     *            the categoryName to set
     */
    public void setCategoryName(String categoryName) {
        this.categoryName = categoryName;
    }

    /**
     * @return the textBox
     */
    public int getTextBox() {
        return textBox;
    }

    /**
     * @param textBox
     *            the textBox to set
     */
    public void setTextBox(int textBox) {
        this.textBox = textBox;
    }

    /**
     * @return the longName
     */
    public String getLongName() {
        return longName;
    }

    /**
     * @param longName
     *            the longName to set
     */
    public void setLongName(String longName) {
        this.longName = longName;
    }

    public Category clone() {
        Category newCat = new Category();
        newCat.categoryName = categoryName;
        newCat.locked = locked;
        newCat.longName = longName;
        newCat.textBox = textBox;
        return newCat;
    }
}
