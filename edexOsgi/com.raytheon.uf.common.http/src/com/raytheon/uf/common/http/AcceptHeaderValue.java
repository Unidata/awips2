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
package com.raytheon.uf.common.http;

/**
 * Represents a single HTTP accept(-encoding) header encoding with weight value
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 8, 2013  2539       bclement     Initial creation
 * Feb 14, 2014 2756       bclement     moved to common http from ogc common
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class AcceptHeaderValue {

    private final String encoding;

    private final double qvalue;

    public AcceptHeaderValue(String encoding, double qvalue) {
        this.encoding = encoding;
        if (qvalue < 0 || qvalue > 1.0) {
            throw new IllegalArgumentException(
                    "qvalue must be between 0 and 1.0");
        }
        this.qvalue = qvalue;
    }

    public AcceptHeaderValue(String encoding) {
        this(encoding, 1.0);
    }

    /**
     * @return the encoding
     */
    public String getEncoding() {
        return encoding;
    }

    /**
     * @return the qvalue
     */
    public double getQvalue() {
        return qvalue;
    }

    /**
     * @return true if encoding is acceptable
     */
    public boolean isAcceptable(){
        return qvalue != 0;
    }
}
