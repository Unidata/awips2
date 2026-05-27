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
package com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor.output;

import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;

/**
 * Container used to store the calculated/processed information associated with
 * a single lid and source that will be written to the Point Precipitation file.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 31, 2018 7184       bkowal      Initial creation
 * Feb 19, 2018 7184       bkowal      Added {@link #contains6HourData()}.
 * Apr 06, 2018 7184       bkowal      Set all output to 0 when specified.
 *
 * </pre>
 *
 * @author bkowal
 */

public class PrecipStationOutput {

    private Double value12to18 = null;

    private Double value18to00 = null;

    private Double value00to06 = null;

    private Double value06to12 = null;

    private Double totalValue12to12 = null;

    private String physicalElement = ParameterCode.PhysicalElement.PRECIPITATION_INCREMENT
            .getCode();

    private String source = ShefConstants.Z;

    public PrecipStationOutput(final boolean setZero) {
        if (setZero) {
            value12to18 = 0.0;
            value18to00 = 0.0;
            value00to06 = 0.0;
            value06to12 = 0.0;
            totalValue12to12 = 0.0;
        }
    }

    /**
     * Indicates whether or not any of the six hour fields have been populated.
     * Used to ensure that different data sources are not mixed together.
     * 
     * @return {@code true} if any of the six hour fields have been populated;
     *         {@code false}, otherwise.
     */
    public boolean contains6HourData() {
        return value12to18 != null || value18to00 != null || value00to06 != null
                || value06to12 != null;
    }

    /**
     * @return the value12to18
     */
    public Double getValue12to18() {
        return value12to18;
    }

    /**
     * @param value12to18
     *            the value12to18 to set
     */
    public void setValue12to18(Double value12to18) {
        this.value12to18 = value12to18;
    }

    /**
     * @return the value18to00
     */
    public Double getValue18to00() {
        return value18to00;
    }

    /**
     * @param value18to00
     *            the value18to00 to set
     */
    public void setValue18to00(Double value18to00) {
        this.value18to00 = value18to00;
    }

    /**
     * @return the value00to06
     */
    public Double getValue00to06() {
        return value00to06;
    }

    /**
     * @param value00to06
     *            the value00to06 to set
     */
    public void setValue00to06(Double value00to06) {
        this.value00to06 = value00to06;
    }

    /**
     * @return the value06to12
     */
    public Double getValue06to12() {
        return value06to12;
    }

    /**
     * @param value06to12
     *            the value06to12 to set
     */
    public void setValue06to12(Double value06to12) {
        this.value06to12 = value06to12;
    }

    /**
     * @return the totalValue12to12
     */
    public Double getTotalValue12to12() {
        return totalValue12to12;
    }

    /**
     * @param totalValue12to12
     *            the totalValue12to12 to set
     */
    public void setTotalValue12to12(Double totalValue12to12) {
        this.totalValue12to12 = totalValue12to12;
    }

    /**
     * @return the physicalElement
     */
    public String getPhysicalElement() {
        return physicalElement;
    }

    /**
     * @param physicalElement
     *            the physicalElement to set
     */
    public void setPhysicalElement(String physicalElement) {
        this.physicalElement = physicalElement;
    }

    /**
     * @return the source
     */
    public String getSource() {
        return source;
    }

    /**
     * @param source
     *            the source to set
     */
    public void setSource(String source) {
        this.source = source;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("PrecipStationOutput [");
        sb.append("value12to18=").append(value12to18);
        sb.append(", value18to00=").append(value18to00);
        sb.append(", value00to06=").append(value00to06);
        sb.append(", value06to12=").append(value06to12);
        sb.append(", totalValue12to12=").append(totalValue12to12);
        sb.append(", physicalElement=").append(physicalElement);
        sb.append(", source=").append(source);
        sb.append("]");
        return sb.toString();
    }
}