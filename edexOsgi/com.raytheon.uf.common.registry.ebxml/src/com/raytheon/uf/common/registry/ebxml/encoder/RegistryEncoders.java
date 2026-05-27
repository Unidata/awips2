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
package com.raytheon.uf.common.registry.ebxml.encoder;

import java.util.Collections;

/**
 * Provides access to encoding strategies. This class is similar to the
 * {@link Collections} class in that it hides away implementation details, and
 * forces clients to use the interface.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 7, 2012  1102      djohnson     Initial creation
 * Dec 4, 2013  2584      dhladky      Only a JAXB encoder for registry
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public final class RegistryEncoders {

    /**
     * An enumeration of valid {@link IRegistryEncoder} types.
     */
    public static enum Type {

        JAXB(new JaxbEncoder());

        private final IRegistryEncoder encoder;

        private Type(IRegistryEncoder encoder) {
            this.encoder = encoder;
        }

        /**
         * Retrieve the encoder instance associated to this enum instance.
         * Intentionally package-private as all access should go through
         * {@link RegistryEncoders}.
         * 
         * @return
         */
        IRegistryEncoder getEncoder() {
            return encoder;
        }
    }

    /**
     * Disabled constructor.
     */
    private RegistryEncoders() {
    }

    /**
     * Retrieve an {@link IRegistryEncoder} implementation for the specified
     * {@link Type}.
     * 
     * @param type
     *            the type requested
     * @return the registry encoder
     */
    public static IRegistryEncoder ofType(Type type) {
        return type.getEncoder();
    }
}
