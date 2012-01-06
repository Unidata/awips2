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
package com.raytheon.uf.common.dataplugin.gfe.weather;

/**
 * Encapsulation of TextStrings for the composite information. *
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2011      #8156 randerso    Re-ported from AWIPS 1
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class WxComposite {

    private String coverage;

    private String types;

    private String typesWithInten;

    private String visibility;

    private String attributes;

    /**
     * @param coverage
     * @param types
     * @param typesWithInten
     * @param visibility
     * @param attributes
     */
    public WxComposite(String coverage, String types, String typesWithInten,
            String visibility, String attributes) {
        this.coverage = coverage;
        this.types = types;
        this.typesWithInten = typesWithInten;
        this.visibility = visibility;
        this.attributes = attributes;
    }

    public String coverage() {
        return coverage;
    }

    public String types() {
        return types;
    }

    public String typesWithInten() {
        return typesWithInten;
    }

    public String visibility() {
        return visibility;
    }

    public String attributes() {
        return attributes;
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();

        result.append("Coverage: ").append(coverage()).append("\n");
        result.append("Types: ").append(types()).append("\n");
        result.append("Types with Intensities: ").append(typesWithInten())
                .append("\n");
        result.append("Visibility: ").append(visibility()).append("\n");
        result.append("Attributes: ").append(attributes()).append("\n");

        return result.toString();
    }

}
