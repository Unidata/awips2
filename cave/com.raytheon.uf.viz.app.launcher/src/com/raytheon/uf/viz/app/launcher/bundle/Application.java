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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * 
 * Represents the application to start. Convenience methods are provided
 * to access the full command line and the additional arguments information.
 * The Application instance uses {@link Arguments} instance to maintain the
 * command line arguments.
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
@XmlRootElement(name="application")
@XmlAccessorType(XmlAccessType.NONE)
public class Application implements IUpdatable, ISerializableObject {
    
    /** the name of the application to run */
    @XmlAttribute
    private String name;
    
    /** the path to the application to run */
    @XmlAttribute
    private String path;
    
    /** the directory in which to run the application */
    @XmlAttribute
    private String runDir;

    /** the command line arguments to pass to the application */
    @XmlElement
    private Arguments arguments;

    /**
     * Constructor. This version constructs a application object representing
     * the specified executable in the specified directory and having the
     * specified run directory.
     * 
     * @param name the name of the executable to run
     * @param path the path to the executable to run
     * @param runDir the directory for the executable to run in
     */
    public Application(String name, String path, String runDir) {
        super();
        this.name= name;
        this.path = path;
        this.runDir = runDir;
    }
    /**
     * Default constructor.
     */
    public Application() {
        super();
    }
    /**
     * returns the name of the executable to run.
     */
    public String getName() {
        return name;
    }
    /**
     * sets the name of the executable to run.
     */
    public void setName(String name) {
        this.name = name;
    }
    /**
     * returns the path to the executable to run.
     */
    public String getPath() {
        return path;
    }
    /**
     * sets the path to the executable to run.
     */
    public void setPath(String path) {
        this.path = path;
    }
    /**
     * gets the path to the executable's run directory.
     */
    public String getRunDir() {
        return runDir;
    }
    /**
     * sets the executable's run directory.
     */
    public void setRunDir(String runDir) {
        this.runDir = runDir;
    }
    /**
     * returns the executable's Arguments object.
     */
    public Arguments getArguments() {
        return arguments;
    }
    /**
     * sets the executabl's Arguments object.
     */
    public void setArguments(Arguments arguments) {
        this.arguments = arguments;
    }
    /**
     * Adds the specified argument to the Application object's
     * arguments. Creates the Arguments object if necessary.
     * 
     * @param argument the argument value to add
     */
    public void addArgument(String argument) {
        if (this.arguments == null) {
            this.arguments = new Arguments(0);
        }
        this.arguments.addValue(argument);
    }
    /**
     * Adds the specified arguments to to the Application object's
     * arguments. Creates the Arguments object if necessary.
     * 
     * @param arguments the arguments to add
     */
    public void addArguments(String[] arguments) {
        if (this.arguments == null) {
            this.arguments = new Arguments(0);
        }
        for (String argument : arguments) {
            this.arguments.addValue(argument);
        }
    }
    /**
     * Updates the Application object's data, name, path, runDir
     * & arguments, by substituting for the environment values.
     * <P>
     * Values in the environment array must have the form name=value.
     * @param environment
     */
    public void updateApplicationData(String[] environment) {
        for (String setting : environment) {
            String[] parts = setting.split("=");
            if (parts.length == 2) {
                String regex = "\\$" + parts[0];
                String replacement = parts[1];
                updateData(regex, replacement);
            }
        }
    }
    /**
     * Returns the command line to run the application. The has the format
     * <pre>
     *     path/name arg1 arg2 ...
     * </pre>
     */
    public String getCommandLine() {
        if (name == null && path == null) {
            return "";
        }
        StringBuffer sb = new StringBuffer();
        if (path != null && !"".equals(path)) {
            sb.append(path);
            if (!path.endsWith(File.separator)) {
                sb.append(File.separator);
            }
        }
        if(name != null) {
            sb.append(name);
        }
        if (arguments != null) {
            for (String arg : arguments.getValues()) {
                sb.append(" ").append(arg);
            }
        }
        return sb.toString();
    }
    /**
     * returns the number of additional arguments for the executable.
     */
    public int getAdditionalArgCount() {
        int retval = 0;
        if (arguments != null) {
            retval = arguments.getAdditional();
        }
        return retval;
    }
    /**
     * returns a boolean indicating if the additional arguments are optional.
     */
    public boolean areArgsOptional() {
        boolean retval = false;
        if (arguments != null) {
            retval = arguments.isOptional();
        }
        return retval;
    }
    @Override
    public void updateData(String regex, String replacement) {
        // needs to update name, path, runDir & arguments
        this.name = updateString(this.name, regex, replacement);
        this.path = updateString(this.path, regex, replacement);
        this.runDir = updateString(this.runDir, regex, replacement);
        if (this.arguments != null) {
            this.arguments.updateData(regex, replacement);
        }
    }
    /**
     * Replaces the specified regular expression in the source string with 
     * the replacement. This is strictly a convenience method.
     * 
     * @param source the string to modify
     * @param regex the expression to replace
     * @param replacement the replacement
     * 
     * @return the modified string
     */
    private String updateString(String source, String regex, String replacement) {
        return (source==null)?null:source.replaceAll(regex, replacement);
    }
}
