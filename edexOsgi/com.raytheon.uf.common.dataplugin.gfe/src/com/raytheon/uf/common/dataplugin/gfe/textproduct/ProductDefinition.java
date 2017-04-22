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
package com.raytheon.uf.common.dataplugin.gfe.textproduct;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Text Product Definition container
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 23, 2010     #2866  randerso     Initial creation
 * Jul 30, 2015     #4263  dgilling     Add second constructor.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

@DynamicSerialize
public class ProductDefinition {

    @DynamicSerializeElement
    private Map<String, Object> definition;

    public ProductDefinition() {
        this.definition = new HashMap<>();
    }

    public ProductDefinition(Map<String, Object> definition) {
        this.definition = definition;
    }

    public Map<String, Object> getDefinition() {
        return definition;
    }

    public void setDefinition(Map<String, Object> definition) {
        this.definition = definition;
    }

    public Object get(String key) {
        return definition.get(key);
    }

    public Object put(String key, Object value) {
        return definition.put(key, value);
    }
}
