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
package com.raytheon.uf.common.util;

import java.util.Random;

/**
 * A fixture class creates a domain object based on a seed value. Provided the
 * same seed value, two instances retrieved from the fixture will be
 * equals/hashcode compliant.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 27, 2012 0743       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public abstract class AbstractFixture<T> {
    private static long DEFAULT_SEED = 1L;

    /**
     * Retrieve an instance using the default seed value.
     * 
     * @return the instance
     */
    public T get() {
        return get(DEFAULT_SEED);
    }

    /**
     * Retrieve an instance based on the specified seed value.
     * 
     * @param seedValue
     *            the seed value
     * @return the instance based on the seed value
     */
    public abstract T get(long seedValue);

    /**
     * Get a random enum value.
     * 
     * @param clazz
     *            the enum class
     * @param random
     *            the random instance
     * @return the enum value to use
     */
    public static <E extends Enum<E>> E randomEnum(Class<E> clazz,
            Random random) {
        E[] values = clazz.getEnumConstants();
        return values[random.nextInt(values.length)];
    }
}
