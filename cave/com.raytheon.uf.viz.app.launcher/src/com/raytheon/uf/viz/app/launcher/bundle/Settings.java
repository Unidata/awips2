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

import java.io.File;
import java.util.ArrayList;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Top level class representing the overall environment to be set
 * when running an application.
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
@XmlRootElement(name="settings")
@XmlAccessorType(XmlAccessType.NONE)
public class Settings implements ISerializableObject {
    private static final String CLASS_PATH = "CLASSPATH";
    
    /** defines the Hydro Apps Defaults values to obtain */
    @XmlElement(name="appsdefaults")
    private HydroDefaults appsDefaults;
    
    /** defines the environment variables to set */
    @XmlElement(name="environment")
    private Environment environment;
    
    /** Defines the Java Class Path elements */
    @XmlElement(name="classpath")
    private ClassPath classPath;

    /**
     * Default constructor.
     */
    public Settings() {
        super();
    }
    /**
     * returns the Hydro Apps Defaults object.
     */
    public HydroDefaults getAppsDefaults() {
        return appsDefaults;
    }
    /**
     * sets the Hydro Apps Defaults object.
     */
    public void setAppsDefaults(HydroDefaults appsDefaults) {
        this.appsDefaults = appsDefaults;
    }
    /**
     * returns the environment variable list object.
     */
    public Environment getEnvironment() {
        if (environment == null) {
            environment = new Environment();
        }
        return environment;
    }
    /**
     * sets the environment variable list object.
     */
    public void setEnvironment(Environment environment) {
        this.environment = environment;
    }
    /**
     * returns the Java Class Path object.
     */
    public ClassPath getClassPath() {
        return classPath;
    }
    /**
     * sets the Java Class Path object.
     */
    public void setClassPath(ClassPath classPath) {
        this.classPath = classPath;
    }
    /**
     * Combines the system environment plus the environment from the
     * {@link HydroDefaults}, {@link Environment} and {@link ClassPath}
     * objects into a single array of environment settings. (Each setting
     * has the form {@code name=value}.)
     *  
     * @return the combined settings (environment) for the application 
     */
    public String[] getSettings() {
        ArrayList<String> retval = new ArrayList<String>();
        Map<String, String> sysEnv = System.getenv();
        /* get the system CLASSPATH */
        String classPath = (sysEnv.get(CLASS_PATH)!= null)?sysEnv.get(CLASS_PATH):"";
        
        /* add the system environment */
        for (String key : sysEnv.keySet()) {
            if (!key.equalsIgnoreCase(CLASS_PATH)) {
                retval.add(key + "=" + sysEnv.get(key));
            }
        }
        /* add the hydro defaults */
        if (this.appsDefaults != null) {
            for (String value : this.appsDefaults.getValues()) {
                retval.add(value);
            }
        }
        
        /* update and add the bundle defined environment */
        if (this.environment != null) {
            if (this.appsDefaults != null) {
                for (String item : this.appsDefaults.getValues()) {
                    String[] parts = item.split("=");
                    if (parts.length == 2) {
                        String regex = "\\$" + parts[0];
                        String replacement = parts[1];
                        this.environment.updateData(regex, replacement);
                    }
                }
            }
            for (String item : retval) {
                String[] parts = item.split("=");
                if (parts.length == 2) {
                    String regex = "\\$" + parts[0];
                    String replacement = parts[1];
                    this.environment.updateData(regex, replacement);
                }
            }
            for (String value : this.environment.getValues()) {
                retval.add(value);
            }
        }
        
        /* add the class path */
        if (this.classPath != null) {
            for (String item : retval) {
                String[] parts = item.split("=");
                if (parts.length == 2) {
                    String regex = "\\$" + parts[0];
                    String replacement = parts[1];
                    this.classPath.updateData(regex, replacement);
                }
            }
            // Win32
            classPath += ((!"".equals(classPath)) ? File.pathSeparator : "")
                    + this.classPath.toString();
        }
        retval.add(CLASS_PATH + "=" + classPath);

        return retval.toArray(new String[]{});
    }
    /**
     * Factory method that returns a new, empty Settings object. This
     * is intended as an alternative for obtaining the system environment
     * when no Settings were specified in the Launcher bundle.
     */
    public static Settings getInstance() {
        return new Settings();
    }

}
