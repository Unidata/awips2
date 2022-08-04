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
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.viz.app.launcher.utilities.AppLauncherUtilities;

/**
 * Represents the entries in the Java Class Path to be set into the
 * environment when the application is launched.
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
@XmlRootElement(name="classpath")
@XmlAccessorType(XmlAccessType.NONE)
public class ClassPath implements IPath, IUpdatable, ISerializableObject {
    
    private static final String CURRENT_DIR = ".";

    /** the elements making up the class path */
    @XmlElement(name="entry")
    private Set<Entity> elements = new HashSet<Entity>();
    
    /**
     * Default constructor.
     */
    public ClassPath() {
        super();
        elements.add(new Entity(CURRENT_DIR));
    }
    @Override
    public String[] getValues() {
        ArrayList<String> list = new ArrayList<String>(); 
        for (Entity elem : elements) {
            if (!CURRENT_DIR.equalsIgnoreCase(elem.toString())) {
                list.add(elem.toString());
            }
        }
        return list.toArray(new String[]{});
    }
    @Override
    public void addValue(String value) {
        this.elements.add(new Entity(value));
        
    }
    /**
     * returns the class path elements.
     */
    public Set<Entity> getElements() {
        return elements;
    }
    /**
     * sets the class path elements.
     */
    public void setElements(Set<Entity> elements) {
        this.elements = elements;
    }
    @Override
    public void updateData(String regex, String replacement) {
        for(Entity element : elements) {
            element.updateData(regex, replacement);
        }
        
    }
    @Override
    public String toString() {
        return AppLauncherUtilities.join(AppLauncherUtilities.convEntitySetToStringArray(elements), ":");
    }
}
