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
package com.raytheon.edex.plugin.grib.decoderpostprocessors.precipitation;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Container for multiple AccumulationConfig instances.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 28, 2015 3756       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class AccumulationConfigs {

    @XmlElement(name = "accumulation")
    private List<AccumulationConfig> accumulations;

    /**
     * Constructor.
     */
    public AccumulationConfigs() {
        super();
    }

    /**
     * Constructor.
     *
     * @param accums
     *            The list of accumulations.
     */
    public AccumulationConfigs(List<AccumulationConfig> accums) {
        this.accumulations = accums;
    }

    /**
     * @return the accumulations
     */
    public List<AccumulationConfig> getAccumulations() {
        return accumulations;
    }

    /**
     * @param accumulations
     *            the accumulations to set
     */
    public void setAccumulations(List<AccumulationConfig> accumulations) {
        this.accumulations = accumulations;
    }

    /**
     * Gets a list of {@link AccumulationConfig}s that match the given model, if
     * any. If no matches are found, an empty list is returned.
     *
     * @param model
     *            The model to match.
     * @return A list containing all matching accumulation configs. If no
     *         matches are found, and empty list is returned.
     */
    public List<AccumulationConfig> getAccumulations(String model) {
        List<AccumulationConfig> accums = new ArrayList<>();
        for (AccumulationConfig accum : this.accumulations) {
            if (accum.modelMatches(model)) {
                accums.add(accum);
            }
        }
        return accums;
    }

    /**
     * Merges the AccumulationConfigs with this one. Any configs with equivalent
     * model lists will be merged, otherwise the config will be added into the
     * list of configs.
     *
     * @param configs
     *            The configs to merge. If null, nothing is done.
     */
    public void merge(AccumulationConfigs configs) {
        if (configs == null) {
            return;
        }

        boolean merged;
        for (AccumulationConfig toMerge : configs.getAccumulations()) {
            if (toMerge == null) {
                continue;
            }
            merged = false;
            for (AccumulationConfig local : this.accumulations) {
                if (local.modelsEqual(toMerge)) {
                    local.mergeCreations(toMerge);
                    merged = true;
                }
            }
            if (!merged) {
                this.accumulations.add(toMerge);
            }
        }
    }
}
