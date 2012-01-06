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
package com.raytheon.edex.msg;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Represents a command to be executed by the AdapterSrv service. Convienence
 * methods are provided for setting the class members from a literal command
 * line as well as retrieving the literal command line.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 19Feb2007    TO5         MW Fegan    Initial creation.
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class Command {
    /*
     * class attributes
     */
    @XmlAttribute
    private String name = "";

    @XmlElements( { @XmlElement(name = "arg", type = String.class) })
    private String[] arguments = null;

    /**
     * Creates a Command object from a literal command line.
     * 
     * @param command
     *            the literal command line
     * 
     * @return the newly creaeted Command object
     */
    public static Command initialize(String command) {
        String[] parts = command.split(" ", 2);
        Command cmd = new Command();
        cmd.setName(parts[0]);
        if (parts.length == 2) {
            cmd.setArguments(parts[1].split(" "));
        }
        return cmd;
    }

    /**
     * Converts the Command object into a literal command line.
     * 
     * @return the literal command line
     */
    public String getCommandLine() {
        StringBuffer sbuff = new StringBuffer(this.name);
        if (arguments != null && arguments.length != 0) {
            for (String arg : arguments) {
                sbuff.append(" " + arg);
            }
        }
        return sbuff.toString();
    }

    /**
     * @return the arguments
     */
    public String[] getArguments() {
        return arguments;
    }

    /**
     * @param arguments
     *            the arguments to set
     */
    public void setArguments(String[] arguments) {
        this.arguments = arguments;
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

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    public String toString() {
        StringBuffer retval = new StringBuffer();
        if (!this.name.equals("")) {
            retval.append(this.name);
        }
        if (this.arguments != null && this.arguments.length != 0) {
            for (String arg : this.arguments) {
                retval.append(" " + arg);
            }
        }
        return retval.toString();
    }
}
