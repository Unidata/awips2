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
package com.raytheon.viz.gfe.textformatter;

import com.raytheon.uf.common.dataplugin.gfe.textproduct.ProductDefinition;

/**
 * Container object for passing metadata about a given text product between
 * python and Java.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 29, 2015  #4263     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public final class TextProductMetadata {

    private final String moduleName;

    private final String displayName;

    private final ProductDefinition productDefinition;

    /**
     * @param moduleName
     * @param displayName
     * @param productDefinition
     */
    public TextProductMetadata(String moduleName, String displayName,
            ProductDefinition productDefinition) {
        this.moduleName = moduleName;
        this.displayName = displayName;
        this.productDefinition = productDefinition;
    }

    public String getModuleName() {
        return moduleName;
    }

    public String getDisplayName() {
        return displayName;
    }

    public ProductDefinition getProductDefinition() {
        return productDefinition;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("TextProductMetadata [moduleName=");
        builder.append(moduleName);
        builder.append(", displayName=");
        builder.append(displayName);
        builder.append(", productDefinition=");
        builder.append(productDefinition);
        builder.append("]");
        return builder.toString();
    }
}
