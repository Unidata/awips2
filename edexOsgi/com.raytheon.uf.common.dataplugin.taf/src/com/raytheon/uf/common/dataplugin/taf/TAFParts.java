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
package com.raytheon.uf.common.dataplugin.taf;

/**TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080424           1001 jkorman     Initial implementation.
 * May 15, 2014       3002 bgonzale    Moved to com.raytheon.uf.common.dataplugin.taf.
 * 
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public class TAFParts {

    private String tafHeader = null;
    
    private String tafBody = null;

    /**
     * Empty constructor.
     */
    public TAFParts() {
    }
    
    /**
     * Create a new TAFParts instance with specified header and body parts.
     * @param header
     * @param body
     */
    public TAFParts(String header, String body) {
        tafHeader = header;
        tafBody = body;
    }
    
    /**
     * @return the tafHeader
     */
    public String getTafHeader() {
        return tafHeader;
    }

    /**
     * @param tafHeader the tafHeader to set
     */
    public void setTafHeader(String tafHeader) {
        this.tafHeader = tafHeader;
    }

    /**
     * @return the tafBody
     */
    public String getTafBody() {
        return tafBody;
    }

    /**
     * @param body
     *            The tafBody to set
     */
    public void setTafBody(String body) {
        this.tafBody = body;
    }

    public String toString() {
        return "header{" + tafHeader + "}: body{" + tafBody + "}";
    }
}
