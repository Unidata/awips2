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
package com.raytheon.uf.viz.spring.dm;

import org.osgi.framework.Bundle;

/**
 * 
 * Class loader for spring bundle context
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 6, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class OSGIXmlClassLoader extends ClassLoader {

    private Bundle bundle;

    public OSGIXmlClassLoader(Bundle bundle, ClassLoader parent) {
        super(parent);
        this.bundle = bundle;
    }

    @Override
    protected synchronized Class<?> loadClass(String name, boolean resolve)
            throws ClassNotFoundException {
        Class<?> clazz = null;
        try {
            clazz = super.loadClass(name, resolve);
        } catch (ClassNotFoundException e) {
            // Ignore here
        }

        if (clazz == null) {
            clazz = bundle.loadClass(name);
        }

        return clazz;
    }

}
