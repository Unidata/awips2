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

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.viz.app.launcher.utilities.AppLauncherUtilities;

/**
 * Represents the Hydro Apps Defaults defined values that are to be
 * added to the environment used to launch the application. It is responsible
 * for obtaining the appropriate values of from the central Apps Defaults
 * repository.
 *  
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 11, 2009 2081       mfegan     Initial creation
 *
 * </pre>
 *
 * @author mfegan
 * @version 1.0
 */
@XmlRootElement(name="appsdefaults")
@XmlAccessorType(XmlAccessType.NONE)
public class HydroDefaults implements IEnvronmental, ISerializableObject {
    
    /** the default elements list for the HydroDefaults object. */
    @XmlElement(name="default")
    private Set<Element> defaults = new HashSet<Element>();
    
    /** list containing the actual values from Apps Defaults localization */
    private ArrayList<Element> values = new ArrayList<Element>();
    /**
     * Default constructor.
     */
    public HydroDefaults() {
        super();
    }

    /**
     * Adds an environment {@link Element} to the {@link #defaults} list.
     * Also obtains the actual value from localization and adds it to
     * the {@link #values} list.
     */
    @Override
    public void addValue(String key,String value) {
        this.defaults.add(new Element(key,value));
        AppsDefaults ad = AppsDefaults.getInstance();
        Element element = new Element();
        element.setName(key);
        element.setValue(ad.getToken(value));
        values.add(element);
        ad = null;
    }

    /**
     * Returns the representations of the {@link #values} list
     */
    @Override
    public String[] getValues() {
        AppsDefaults ad = AppsDefaults.getInstance();
        for (Element element : defaults) {
            Element value = new Element();
            value.setName(element.getName());
            value.setValue(ad.getToken(element.getValue()));
            values.add(value);
        }
        ad = null;
        return AppLauncherUtilities.convListToStringArray(values);
    }

    /**
     * returns the default elements list.
     */
    public Set<Element> getDefaults() {
        return defaults;
    }
    /**
     * sets the default elements list. In addition, it obtains the actual
     * values from localization and adds them to the {@link #values} list.
     */
    public void setDefaults(Set<Element> defaults) {
        this.defaults = defaults;
        AppsDefaults ad = AppsDefaults.getInstance();
        for (Element element : defaults) {
            Element value = new Element();
            value.setName(element.getName());
            element.setValue(ad.getToken(element.getValue()));
            values.add(value);
        }
        ad = null;
    }
}
