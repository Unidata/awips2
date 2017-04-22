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
package com.raytheon.uf.edex.registry.acp.xacml.objects;

/**
 * Class used to encapsulate the
 * urn:oasis:names:tc:xacml:1.0:data-type:rfc822Name type
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 8/17/2012    724          bphillip    Initial Coding
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class Rfc822Name {

    /** The local part of the name */
    private String localPart;

    /** The domain part of the name */
    private String domainPart;

    /**
     * Constructs an empty Rfc822Name
     */
    public Rfc822Name() {

    }

    /**
     * Creates a new Rfc822Name
     * 
     * @param localPart
     *            The local part
     * @param domainPart
     *            The domain part
     */
    public Rfc822Name(String localPart, String domainPart) {
        this.localPart = localPart;
        this.domainPart = domainPart;
    }

    /**
     * Creates a new Rfc822Name
     * 
     * @param name
     *            The Rfc822Name
     */
    public Rfc822Name(String name) {
        String[] tokens = name.split("@");
        this.localPart = tokens[0];
        this.domainPart = tokens[1];
    }

    public boolean equals(Object rhs) {
        if (rhs instanceof Rfc822Name) {
            Rfc822Name obj = (Rfc822Name) rhs;
            return this.getLocalPart().equals(obj.getLocalPart())
                    && this.getDomainPart().equalsIgnoreCase(
                            obj.getDomainPart());
        }
        return false;
    }

    public String toString() {
        return this.localPart + "@" + this.domainPart;
    }

    /**
     * @return the localPart
     */
    public String getLocalPart() {
        return localPart;
    }

    /**
     * @param localPart
     *            the localPart to set
     */
    public void setLocalPart(String localPart) {
        this.localPart = localPart;
    }

    /**
     * @return the domainPart
     */
    public String getDomainPart() {
        return domainPart;
    }

    /**
     * @param domainPart
     *            the domainPart to set
     */
    public void setDomainPart(String domainPart) {
        this.domainPart = domainPart;
    }

}
