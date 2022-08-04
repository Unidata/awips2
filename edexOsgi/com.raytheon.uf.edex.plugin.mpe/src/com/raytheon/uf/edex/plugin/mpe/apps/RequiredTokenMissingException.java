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
package com.raytheon.uf.edex.plugin.mpe.apps;

import com.raytheon.uf.common.ohd.AppsDefaults;

/**
 * Indicates that a required property was not set in Apps_defaults.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 24, 2016 5631       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class RequiredTokenMissingException extends Exception {

    private static final long serialVersionUID = 8703480774471799698L;

    private static final String MESSAGE_FMT = "Failed to find a value associated with required token %s in %s.";
    
    private final String token;

    public RequiredTokenMissingException(final String token) {
        super(String.format(MESSAGE_FMT, token, AppsDefaults.NAME));
        this.token = token;
    }

    public String getToken() {
        return token;
    }
}