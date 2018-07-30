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
package com.raytheon.uf.edex.plugin.mpe.rocchecker;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.collections.CollectionUtils;

/**
 * Configuration for a single table that Roc Checker should analyze. Supports
 * optional constraining parameters.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 20, 2016 5590       bkowal      Initial creation
 * Jun 24, 2016 5699       bkowal      Added {@link #toString()} and other utility methods.
 * 
 * </pre>
 * 
 * @author bkowal
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "config")
public class RocCheckerConfig {

    public static enum ROC_QUALITY {
        G, GQ
    }

    private static final int DEFAULT_END_HOURS = 0;

    private static final int DEFAULT_START_HOURS = 6;

    /**
     * Name of the table to retrieve observation data from. This String will
     * later be mapped to the {@link ObservationTable}. If the mapping fails, an
     * error message will be logged to indicate that the localization-based
     * configuration will need to be updated. Using a String instead of the
     * {@link ObservationTable} directly ensures that it will be possible to
     * identify the table name associated with the invalid configuration rather
     * than just being left with a {@code null} enum value.
     */
    @XmlAttribute(required = true)
    private String table;

    @XmlElement
    private ROC_QUALITY quality = ROC_QUALITY.G;

    /**
     * Number of hours to subtract from the end time to use as the start time.
     */
    @XmlElement
    private int startHours = DEFAULT_START_HOURS;

    /**
     * Number of hours to subtract from the current time to use as the end time.
     * If unspecified, the end time will be the current time.
     */
    @XmlElement
    private int endHours = DEFAULT_END_HOURS;

    @XmlElement(name = "constraints")
    private RocCheckerConstraints rocCheckerConstraints;

    public RocCheckerConfig() {
    }

    /**
     * Determines whether this configuration only consists of the basic
     * configuration without any constraints.
     * 
     * @return {@code true} if no specific constraints have been specified;
     *         {@code false}, otherwise.
     */
    public boolean isNoConstraintsConfig() {
        return (rocCheckerConstraints == null || rocCheckerConstraints
                .isEmptyConstraints());
    }

    /**
     * Determines whether this configuration includes lid constraints (and no pe
     * constraints).
     * 
     * @return {@code true} if this configuration only includes lid constraints;
     *         {@code false}, otherwise.
     */
    public boolean isOnlyLidConstraintsConfig() {
        return (!isNoConstraintsConfig())
                && CollectionUtils.isEmpty(rocCheckerConstraints.getPes());
    }

    /**
     * Determines whether this configuration includes pe constraints (and no lid
     * constraints).
     * 
     * @return {@code true} if this configuration only includes pe constraints;
     *         {@code false}, otherwise.
     */
    public boolean isOnlyPEConstraintConfig() {
        return (!isNoConstraintsConfig())
                && CollectionUtils.isEmpty(rocCheckerConstraints.getLids());
    }

    /**
     * Determines whether this configuration includes BOTH lid and pe
     * constraints.
     * 
     * @return {@code true} if this configuration only includes lid and pe
     *         constraints; {@code false}, otherwise.
     */
    public boolean isLidAndPEConstraintConfig() {
        return (!isNoConstraintsConfig())
                && CollectionUtils.isNotEmpty(rocCheckerConstraints.getLids())
                && CollectionUtils.isNotEmpty(rocCheckerConstraints.getPes());
    }

    public String getTable() {
        return table;
    }

    public void setTable(String table) {
        this.table = table;
    }

    public ROC_QUALITY getQuality() {
        return quality;
    }

    public void setQuality(ROC_QUALITY quality) {
        this.quality = quality;
    }

    public int getStartHours() {
        return startHours;
    }

    public void setStartHours(int startHours) {
        this.startHours = startHours;
    }

    public int getEndHours() {
        return endHours;
    }

    public void setEndHours(int endHours) {
        this.endHours = endHours;
    }

    public RocCheckerConstraints getRocCheckerConstraints() {
        return rocCheckerConstraints;
    }

    public void setRocCheckerConstraints(
            RocCheckerConstraints rocCheckerConstraints) {
        this.rocCheckerConstraints = rocCheckerConstraints;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("RocCheckerConfig [");
        sb.append("table=").append(table);
        if (quality != null) {
            sb.append(", quality=").append(quality.name());
        }
        sb.append(", startHours=").append(startHours);
        sb.append(", endHours=").append(endHours);
        if (rocCheckerConstraints != null) {
            sb.append(", rocCheckerConstraints=").append(
                    rocCheckerConstraints.toString());
        }
        sb.append("]");
        return sb.toString();
    }
}