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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * 
 * Top level class defining an Application to launch. The Launcher instance
 * contains an instance each of {@link Application} and {@link Settings}.  
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
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class Launcher implements ISerializableObject {
    /** name of the launcher */
    @XmlAttribute
    private String title;
    
    /** the application to launch */
    @XmlElement
    private Application application;
    
    /** environmental settings to launch */
    @XmlElement
    private Settings settings;
    
    /**
     * Default constructor */
    public Launcher() {
        super();
    }

    /**
     * Constructor. Creates a Launcher object with the specified title.
     * @param title
     */
    public Launcher(String title) {
        super();
        this.title = title;
    }
    /**
     * returns the Launcher's title.
     */
    public String getTitle() {
        return title;
    }
    /**
     * Sets the Launcher's title.
     */
    public void setTitle(String title) {
        this.title = title;
    }
    /**
     * returns the nested Application object.
     */
    public Application getApplication() {
        return application;
    }
    /**
     * sets the nested Application object.
     */
    public void setApplication(Application application) {
        this.application = application;
    }
    /**
     * returns the nested Settings (runtime environment) object.
     * @return
     */
    public Settings getSettings() {
        if (settings == null) {
            settings = new Settings();
        }
        return settings;
    }
    /**
     * Sets the nested Settings (runtime environment) object.
     */
    public void setSettings(Settings settings) {
        this.settings = settings;
    }

}
