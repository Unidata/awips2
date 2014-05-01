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

package com.raytheon.uf.common.dataplugin.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation used by PluginDataObject classes to denote which fields are
 * components of the dataURI
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/8/2008    1532        bphillip    Initial checkin
 * 
 * &#064;author bphillip
 * 
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface DataURI {

    /** Class used to specify non-implemented default converter */
    public static final class NotImplementedFieldConverter implements
            DataURIFieldConverter {
        @Override
        public String toString(Object field) {
            throw new UnsupportedOperationException();
        }

        @Override
        public Object fromString(String string) {
            throw new UnsupportedOperationException();
        }
    }

    public static final Class<NotImplementedFieldConverter> NO_CONVERTER = NotImplementedFieldConverter.class;

    public static final String SEPARATOR = "/";

    /**
     * Gets the position in the dataURI of the annotated field
     * 
     * @return The position in the dataURI
     */
    int position();

    /**
     * Denotes if this is an embedded dataURI meaning that the actual dataURI
     * elements are defined by this field class. This field is mutualy exclusive
     * with {@link #converter()}
     * 
     * @return True if embedded, else false
     */
    boolean embedded() default false;

    /**
     * {@link DataURIFieldConverter} to be used to convert to/from uri string.
     * Mutually exclusive with {@link #embedded()}
     * 
     * @return
     */
    Class<? extends DataURIFieldConverter> converter() default NotImplementedFieldConverter.class;
}
