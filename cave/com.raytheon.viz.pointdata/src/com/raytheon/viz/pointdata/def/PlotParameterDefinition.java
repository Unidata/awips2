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
package com.raytheon.viz.pointdata.def;

import java.io.File;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.viz.pointdata.PlotModelFactory.DisplayType;
import com.raytheon.viz.pointdata.S2N;
import com.raytheon.viz.pointdata.lookup.IAbstractLookupTable;
import com.raytheon.viz.pointdata.lookup.LookupUtils;

/**
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer         Description
 * ------------ ---------- -----------      --------------------------
 * 10/10/2019   71272       Mark Peters     Initial Creation
 * 01/13/2020   73084       K Sunil         added a new svgClass field, changed mode to displayType.
 *
 * </pre>
 *
 * @author mpeters
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class PlotParameterDefinition {

    private static final String PLOT_MODEL_DIR = "plotModels";

    private final Object lock = new Object();

    @XmlAttribute
    private String param;

    @XmlAttribute
    private String displayName;

    @XmlAttribute
    private DisplayType displayType = DisplayType.TEXT;

    @XmlAttribute
    private String unit;

    @XmlAttribute
    private String svgClass;

    @XmlAttribute
    private String format;

    @XmlAttribute
    private int trim = 0;

    @XmlAttribute
    private String lookupTable;

    @XmlAttribute
    private boolean recursiveLookup;

    private IAbstractLookupTable lookup;

    @XmlAttribute
    private String functionTable;

    private S2N ranking;

    @XmlAttribute
    private int index = -1;

    @XmlAttribute
    private String sampleValue;

    private PlotParameterDefinition() {
    }

    public String getSvgClass() {
        return svgClass;
    }

    /**
     * @return the paramName
     */
    public String getParamName() {
        return param;
    }

    /**
     * @return the displayName
     */
    public String getDisplayName() {
        return displayName;
    }

    /**
     * @return the displayType
     */
    public DisplayType getDisplayType() {
        return displayType;
    }

    /**
     * @return the unit
     */
    public String getUnit() {
        return unit;
    }

    /**
     * @return the format
     */
    public String getFormat() {
        return format;
    }

    /**
     * @return the trim
     */
    public int getTrim() {
        return trim;
    }

    public IAbstractLookupTable getActualLookupTable() {
        if (lookupTable != null) {
            synchronized (lock) {
                if (lookup == null) {
                    File table = getTableFile(lookupTable);
                    IAbstractLookupTable lookup = LookupUtils
                            .buildLookupTable(table);
                    if (recursiveLookup) {
                        lookup.setMode("recursive_translation");
                    }
                    this.lookup = lookup;
                }
            }
        }
        return lookup;
    }

    private static File getTableFile(String fileName) {
        File rval = PathManagerFactory.getPathManager().getStaticFile(
                PLOT_MODEL_DIR + IPathManager.SEPARATOR + fileName);
        return rval;
    }

    public S2N getRanking() {
        if (functionTable != null) {
            synchronized (lock) {
                if (ranking == null) {
                    ranking = S2N.readS2NFile(functionTable);
                }
            }
        }

        return ranking;
    }

    /**
     * @return the index
     */
    public int getIndex() {
        return index;
    }

    /**
     * @return the sampleValue
     */
    public String getSampleValue() {
        return sampleValue;
    }

    @Override
    public String toString() {
        return "PlotParameterDefinition [lock=" + lock + ", param=" + param
                + ", displayName=" + displayName + ", displayType="
                + displayType + ", unit=" + unit + ", svgClass=" + svgClass
                + ", format=" + format + ", trim=" + trim + ", lookupTable="
                + lookupTable + ", recursiveLookup=" + recursiveLookup
                + ", lookup=" + lookup + ", functionTable=" + functionTable
                + ", ranking=" + ranking + ", index=" + index + ", sampleValue="
                + sampleValue + "]";
    }
}
