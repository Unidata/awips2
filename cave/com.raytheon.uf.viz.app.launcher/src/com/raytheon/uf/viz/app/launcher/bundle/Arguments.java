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
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * 
 * Represents the command line arguments to pass to an application. 
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
@XmlRootElement(name="arguments")
@XmlAccessorType(XmlAccessType.NONE)
public class Arguments implements IPath, IUpdatable, ISerializableObject {
    
    /** contains the arguments provided explicitly by the bundle */
    @XmlElement(name="argument")
    private List<Entity> arguments = new ArrayList<Entity>();
    
    /** defines the number of additional arguments to get from the user */
    @XmlAttribute
    private int additional;
    
    /** determines if the additional arguments are optional */
    @XmlAttribute
    private boolean optional;

    /**
     * Default constructor.
     */
    public Arguments() {
        super();
    }
    /**
     * Constructor. Creates an Arguments object with the specified number
     * of additional arguments. The optional flag is set to false.
     * 
     * @param additional the number of additional arguments
     */
    public Arguments(int additional) {
        this(additional,false);
    }
    /**
     * Constructor. Creates an Arguments object with the specified number
     * of additional argument. Setting the seond argument to {@code true}
     * makes the arguments optional.
     * 
     * @param additional the number of additional arguments
     * @param optional controls the optional state of the arguments
     */
    public Arguments(int additional, boolean optional) {
        super();
        this.additional = additional;
        this.optional = optional;
    }

    @Override
    public void addValue(String value) {
        this.arguments.add(new Entity(value));
    }

    @Override
    public String[] getValues() {
        ArrayList<String> list = new ArrayList<String>(); 
        for (Entity elem : arguments) {
            list.add(elem.toString());
        }
        return list.toArray(new String[]{});
    }
    @Override
    public void updateData(String regex, String replacement) {
        for (Entity entity : arguments) {
            entity.updateData(regex, replacement);
        }
    }
    /**
     * returns the list of arguments from the bundle
     */
    public List<Entity> getArguments() {
        return arguments;
    }
    /**
     * Sets the list of arguments for the bundle
     */
    public void setArguments(List<Entity> arguments) {
        this.arguments = arguments;
    }
    /**
     * returns the number of additional arguments to get from the user
     */
    public int getAdditional() {
        return additional;
    }
    /**
     * sets the number of additional arguments to get from the user.
     */
    public void setAdditional(int additional) {
        this.additional = additional;
    }
    /**
     * determines if the additional arguments are optional
     */
    public boolean isOptional() {
        return optional;
    }
    /**
     * sets the optional arguments flag.
     */
    public void setOptional(boolean optional) {
        this.optional = optional;
    }
}
