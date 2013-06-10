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
package oasis.names.tc.ebxml.regrep.xsd.rim.v4;

import java.util.Random;

import com.raytheon.uf.common.util.AbstractFixture;

/**
 * Base {@link AbstractFixture} for {@link RegistryObjectType} instances.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 25, 2013 1910       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public abstract class RegistryObjectTypeFixture<T extends RegistryObjectType>
        extends AbstractFixture<T> {

    protected RegistryObjectTypeFixture() {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected T getInstance(long seedValue, Random random) {
        T registryObject = getRegistryObject();
        registryObject.setObjectType(getObjectType());
        registryObject.setId(registryObject.getClass().getSimpleName()
                + seedValue);
        registryObject.setLid(registryObject.getId());
        return registryObject;
    }

    /**
     * @return the object type for the registry object
     */
    protected abstract String getObjectType();

    /**
     * @return the registry object instance
     */
    protected abstract T getRegistryObject();
}
