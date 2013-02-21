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
package com.raytheon.uf.common.serialization.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;

/**
 * An annotation that indicates that a class should have this element
 * serialized.
 * 
 * The annotation should be added to the field itself. The assumption is made
 * that setters and getters are available for this property.
 * 
 * If the {@link #value()} specifies an {@link ISerializationTypeAdapter}, the
 * factory will be used to serialize/deserialize the value.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 07, 2008            chammack    Initial creation
 * Sep 28, 2012 1195       djohnson    Add value() to specify a type adapter.
 * Nov 02, 2012 1302       djohnson    No more field level adapters.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
@Documented
public @interface DynamicSerializeElement {
}
