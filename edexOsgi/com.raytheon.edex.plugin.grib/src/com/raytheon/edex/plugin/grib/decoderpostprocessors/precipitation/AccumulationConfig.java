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

/**
 * Stores a list of models and a list of AccumulationCreationConfigs to apply
 * for those models.
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
@XmlAccessorType(XmlAccessType.NONE)
public class AccumulationConfig {

    @XmlElement(name = "model")
    private List<String> models;

    @XmlElement(name = "create")
    private List<AccumulationCreationConfig> creations;

    /**
     * Constructor
     */
    public AccumulationConfig() {
        super();
    }

    /**
     * Constructor.
     *
     * @param models
     *            The list of models to match.
     * @param creations
     *            The list of accumulations to create.
     */
    public AccumulationConfig(List<String> models,
            List<AccumulationCreationConfig> creations) {
        super();
        this.models = models;
        this.creations = creations;
    }

    /**
     * @return the models
     */
    public List<String> getModels() {
        return models;
    }

    /**
     * @param models
     *            the models to set
     */
    public void setModels(List<String> models) {
        this.models = models;
    }

    public boolean addModel(String model) {
        if (this.models == null) {
            this.models = new ArrayList<>();
        }
        return this.models.add(model);
    }

    /**
     * @return the creations
     */
    public List<AccumulationCreationConfig> getCreations() {
        return creations;
    }

    /**
     * @param creations
     *            the creations to set
     */
    public void setCreations(List<AccumulationCreationConfig> creations) {
        this.creations = creations;
    }

    /**
     * Convenience method to determine if this configuration matches a given
     * model.
     *
     * @param model
     *            The model to check for.
     * @return True if the model matches one of the configured models, false
     *         otherwise.
     */
    public boolean modelMatches(String model) {
        if (this.models == null) {
            return false;
        }
        for (String mod : this.models) {
            if (model.matches(mod)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Gets the subset of creation configs whose minuend's or subtrahend's
     * parameter matches the given parameter, if any.
     *
     * @param parameter
     *            The parameter to match.
     * @param matchMinuend
     *            If true, parameter will be matched to the minuends'
     *            parameters. If false, it will be match to the subtrahends'
     *            parameters.
     * @return the subset of creation configs whose minuend's or subtrahend's
     *         parameter matches the given parameter, if any. An empty list is
     *         returned if no matches are found.
     */
    public List<AccumulationCreationConfig> getCreations(String parameter,
            boolean matchMinuend) {
        List<AccumulationCreationConfig> matchedCreations = new ArrayList<>();

        if (this.creations != null) {
            String param;
            for (AccumulationCreationConfig creation : this.creations) {
                param = matchMinuend ? creation.getMinuendParam() : creation
                        .getSubtrahendParam();
                if (param.equals(parameter)) {
                    matchedCreations.add(creation);
                }
            }
        }

        return matchedCreations;
    }

    /**
     * Merges the list of creations of this config with the creations of the
     * other config. If this config already contains a creation, the duplicate
     * will not be added into this config.
     *
     * @param otherConfig
     *            The accumulation config that's creations will be merged into
     *            this config's creations.
     */
    public void mergeCreations(AccumulationConfig otherConfig) {
        if (otherConfig == null) {
            return;
        }

        if (this.creations == null) {
            this.creations = otherConfig.getCreations();
        } else if (otherConfig.getCreations() != null) {
            boolean dupe;
            for (AccumulationCreationConfig toMerge : otherConfig
                    .getCreations()) {
                if (toMerge == null) {
                    continue;
                }

                dupe = false;
                for (AccumulationCreationConfig local : this.creations) {
                    if (local.equals(toMerge)) {
                        dupe = true;
                        break;
                    }
                }

                if (!dupe) {
                    this.creations.add(toMerge);
                }
            }
        }
    }

    /**
     * Determines if this AccumulationConfig has an equivalent collection of
     * models to the other AccumulationConfig. This collections are considered
     * equivalent if both are null or empty, or both contain the same models,
     * even if in different orders.
     *
     * This will not handle cases where a model-regex lists models in different
     * orders, e.g. {@code <model>GFS215|GFS217</model>} and
     * {@code <model>GFS217|GFS215</model>} are not considered equivalent,
     * though in practice they are.
     */
    public boolean modelsEqual(AccumulationConfig other) {
        if (other == null) {
            return false;
        }
        if (other.getModels() == null || other.getModels().isEmpty()) {
            return this.models == null || this.getModels().isEmpty();
        }

        return other.getModels().containsAll(this.getModels())
                && this.getModels().containsAll(other.getModels());
    }
}
