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
package com.raytheon.uf.common.serialization;

import javax.persistence.Embeddable;

/**
 * Empty interface that should be implemented by any class that uses Hibernate,
 * JaxB, or DynamicSerialize annotations so it is detected at runtime.
 * 
 * Implementing this interface in conjunction with adding the class to the
 * com.raytheon.uf.common.serialization.ISerializableObject file in the
 * META-INF/services directory will ensure it is detected at runtime.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Aug 11, 2008             njensen     Initial creation
 * Oct 02, 2013 2361        njensen     Deprecated
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 * @deprecated This interface is deprecated but may still be required until it
 *             is completely removed from the system. DynamicSerialize no longer
 *             requires ISerializableObjects, just use the DynamicSerialize
 *             annotations. JAXB/XML only requires it if you use the global JAXB
 *             context through {@link SerializationUtil}, however that is a
 *             performance hit and deprecated and you should instead create your
 *             own {@link JAXBManager}. Hibernate no longer uses it, EDEX will
 *             automatically detect classes with {@link Entity} or
 *             {@link Embeddable} annotations if their package name starts with
 *             the same plugin FQN that is present in the PluginProperties or
 *             DatabasePluginProperties.
 * 
 */

@Deprecated
public interface ISerializableObject {

}
