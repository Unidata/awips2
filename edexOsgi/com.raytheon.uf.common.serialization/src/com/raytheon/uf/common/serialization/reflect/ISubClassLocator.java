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
package com.raytheon.uf.common.serialization.reflect;

import java.util.Collection;

/**
 * Interface indicating an object is capable of looking up subclassed based off
 * of a known parent class. Generally this must be implemented using advanced
 * reflection mechanisms that are heavily dependendant on the type of runtime
 * and will ned to be different on edex and cave.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Oct 23, 2013  2491     bsteffen    Initial creation
 * Nov 14, 2013  2361     njensen     Promoted save() to interface
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public interface ISubClassLocator {

    /**
     * Determine all subclasses of a given class.
     * 
     * @param base
     *            a class
     * @return all subclasses of base.
     */
    public Collection<Class<?>> locateSubClasses(Class<?> base);

    /**
     * Store a cache of the located classes.
     */
    public void save();
}
