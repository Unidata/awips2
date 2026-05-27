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
package com.raytheon.uf.edex.registry.status;

import java.util.ArrayList;
import java.util.List;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.registry.BaseQuery;
import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.QueryLanguages;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;

/**
 * Dummy registry query for testing whether the registry is functional or not.
 * Never returns any results.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 12, 2021 8267       mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class DummyRegistryQuery extends BaseQuery<Object> {

    private static final String HQL_QUERY = "select obj from RegistryObjectType as obj where true = false";

    @Override
    public String getQueryType() {
        return CanonicalQueryTypes.ADHOC_QUERY;
    }

    @Override
    public List<SlotType> getSlots() {
        List<SlotType> slots = new ArrayList<>();
        slots.add(RegistryUtil.newStringSlot("queryLanguage",
                QueryLanguages.HQL));
        slots.add(RegistryUtil.newStringSlot("queryExpression", HQL_QUERY));
        return slots;
    }

    @Override
    public Class<?> getObjectType() {
        return Object.class;
    }

    @Override
    public Class<Object> getResultType() {
        return Object.class;
    }
}
