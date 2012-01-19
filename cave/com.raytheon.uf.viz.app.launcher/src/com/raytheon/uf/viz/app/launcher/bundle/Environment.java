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
package com.raytheon.uf.viz.app.launcher.bundle;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Represents the environment variables to be set to launch the
 * application.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2009 2081       mfegan     Initial creation
 *
 * </pre>
 *
 * @author mfegan
 * @version 1.0 
 */
@XmlRootElement(name="environment")
@XmlAccessorType(XmlAccessType.NONE)
public class Environment implements IEnvronmental, IUpdatable, IMergable<Element>, ISerializableObject {
    
    /** the element list for the Environment object. */
    @XmlElement(name="variable")
    private Set<Element> elements = new HashSet<Element>();
    public Environment() {
        super();
    }

    /**
     * Adds an environment {@link Element} to the {@link #elements} list.
     */
    @Override
    public void addValue(String key, String value) {
        elements.add(new Element(key, value));
    }

    /**
     * Returns the representations of the {@link #elements} list.
     */
    @Override
    public String[] getValues() {
        ArrayList<String> list = new ArrayList<String>(); 
        for (Element elem : elements) {
            list.add(elem.toString());
        }
        return list.toArray(new String[]{});
    }

    /**
     * returns the elements in the environment.
     */
    public Set<Element> getElements() {
        return elements;
    }

    /**
     * sets the elements for the environment.
     */
    public void setElements(Set<Element> elements) {
        this.elements = elements;
    }

    /**
     * Performs the update by calling the {@link IUpdatable#updateData(String, String)}
     * method on each {@link Element} in the {@link Environment#elements} list.
     */
    @Override
    public void updateData(String regex, String replacement) {
        for (Element element : elements) {
            element.updateData(regex, replacement);
        }
    }

    @Override
    public void merge(List<Element> list) {
        if (list != null) {
            for (Element element : list) {
                this.elements.add(element);
            }
        }
    }
}
