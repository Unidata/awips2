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

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Field-level annotation indicating that a field should be initialized using a
 * property value read from Apps Defaults that should be interpreted as a
 * {@link Float}.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 19, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface AppsDefaultsFloatField {

    /**
     * The name of the Apps Defaults token
     * 
     * @return the name of the Apps Defaults token
     */
    String property();

    /**
     * Boolean flag indicating whether or not the token is associated with an
     * Apps Defaults property value that is required by the application. When
     * {@code true}, the default value will be ignored. If a required token is
     * not found within Apps Defaults, a {@link RequiredTokenMissingException}
     * will be thrown.
     * 
     * @return {@code true} if the field is required; {@code false}, otherwise
     */
    boolean required() default false;

    /**
     * The value to use when the token cannot be found in Apps Defaults
     * (provided that the token is not required).
     * 
     * @return the value to use when the token cannot be found in Apps Defaults
     *         (provided that the token is not required)
     */
    float defaultValue() default 0;
}