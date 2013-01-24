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
package com.raytheon.uf.common.registry.ebxml;

import java.util.Collections;
import java.util.Set;

import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.GriddedDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSetMetaDataFixture;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 1, 2012             djohnson     Initial creation
 * Aug 10, 2012 1022       djohnson     Remove generics from {@link GriddedDataSetMetaData}.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class SetAssociationResolverTest {

    @Test
    public void testSetWithObjectThatSuperClassIsRegistryObjectIsAllowed() {
        Set<GriddedDataSetMetaData> objects = Collections
                .<GriddedDataSetMetaData> singleton(OpenDapGriddedDataSetMetaDataFixture.INSTANCE
                        .get());

        SetAssociationResolver resolver = new SetAssociationResolver();
        resolver.getRegistryObjects(objects);
    }

}
