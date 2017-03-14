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
package com.raytheon.uf.edex.plugin.mpe;

import java.nio.file.Path;

import com.raytheon.uf.common.ohd.AppsDefaults;

/**
 * Exception indicating that a directory {@link Path} specified in Apps_Defaults
 * is currently unusable.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 2, 2016  5614       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class AppsDefaultsPathException extends Exception {

    private static final long serialVersionUID = -7431414545310903411L;

    public AppsDefaultsPathException(final String token, final Path path,
            Throwable e) {
        super("Failed to initialize " + AppsDefaults.NAME + " path: "
                + path.toString() + " associated with token: " + token + ".", e);
    }
}