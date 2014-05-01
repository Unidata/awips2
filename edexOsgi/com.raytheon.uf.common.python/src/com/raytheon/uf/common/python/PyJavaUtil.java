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
package com.raytheon.uf.common.python;

/**
 * Home for methods that can't be done from Python on PyJObjects but need to be
 * done on Java objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 22, 2013            mnash     Initial creation
 * Oct 30, 2013            mnash     Add method for searching for subclasses
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class PyJavaUtil {

    public static boolean isArray(Object obj) {
        return obj.getClass().isArray();
    }

    public static boolean isSubclass(Object obj, Class<?> clazz) {
        return clazz.isAssignableFrom(obj.getClass());
    }
}
