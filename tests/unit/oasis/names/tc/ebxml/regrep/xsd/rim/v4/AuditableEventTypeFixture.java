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

import java.util.Arrays;
import java.util.Random;

/**
 * Fixture to retrieve {@link AuditableEventType} instances.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 24, 2013 1910       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class AuditableEventTypeFixture extends
        RegistryObjectTypeFixture<AuditableEventType> {

    public static final AuditableEventTypeFixture INSTANCE = new AuditableEventTypeFixture();

    protected AuditableEventTypeFixture() {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected AuditableEventType getInstance(long seedValue, Random random) {
        final AuditableEventType instance = super.getInstance(
                seedValue, random);
        instance.setAction(Arrays.asList(ActionTypeFixture.INSTANCE
                .get(seedValue)));
        return instance;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getObjectType() {
        return "urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:AuditableEvent";
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected AuditableEventType getRegistryObject() {
        return new AuditableEventType();
    }

}
