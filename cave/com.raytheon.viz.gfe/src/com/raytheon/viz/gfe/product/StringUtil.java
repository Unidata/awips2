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
package com.raytheon.viz.gfe.product;

import java.util.Collection;
import java.util.Iterator;

/**
 * String manipulation.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 19 FEB 2010  4132        ryu         Initial creation
 * 
 * </pre>
 * 
 * @author ryu
 * @version 1.0
 * 
 */
public class StringUtil {
    public static String stringJoin(String[] tokens, String separator) {
    	StringBuilder sb = new StringBuilder();
    	if (tokens.length > 0) {
    	    sb.append(tokens[0]);
    	}
    	for (int i=1; i<tokens.length; i++) {
    	    sb.append(separator);
    	    sb.append(tokens[i]);
    	}
    	return sb.toString();
    }
    
    public static String stringJoin(Collection<String> tokens, String separator) {
    	StringBuilder sb = new StringBuilder();
        Iterator<String> iter = tokens.iterator();
        while (iter.hasNext()) {
            sb.append(iter.next());
            if (iter.hasNext()) {
                sb.append(separator);
            }
        }
        return sb.toString();
    }
}
