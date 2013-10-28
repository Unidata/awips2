package com.raytheon.viz.grid.xml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Container for a list of rules that describe how gridded data should be
 * re-projected on various map projections.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer         Description
 * ------------ ---------- ---------------- --------------------------
 * Sep 23, 2013 DR 15972   D. Friedman      Initial creation
 *
 * </pre>
 *
 */
@XmlRootElement()
@XmlAccessorType(XmlAccessType.NONE)
public class GridReprojectionRules {
    public static enum Reproject {
        ALWAYS, NEVER, TEST
    }

    @XmlAccessorType(XmlAccessType.NONE)
    public static class Rule {
        @XmlAttribute
        private String modelName;
        @XmlAttribute
        private String displayType;
        @XmlAttribute
        private String srcProjection;
        @XmlAttribute
        private String dstProjection;
        @XmlAttribute
        private Reproject reproject;

        public String getModelName() {
            return modelName;
        }
        public void setModelName(String modelName) {
            this.modelName = modelName;
        }
        public String getDisplayType() {
            return displayType;
        }
        public void setDisplayType(String displayType) {
            this.displayType = displayType;
        }
        public String getSrcProjection() {
            return srcProjection;
        }
        public void setSrcProjection(String srcProjection) {
            this.srcProjection = srcProjection;
        }
        public String getDstProjection() {
            return dstProjection;
        }
        public void setDstProjection(String dstProjection) {
            this.dstProjection = dstProjection;
        }
        public Reproject getReproject() {
            return reproject;
        }
        public void setReproject(Reproject reproject) {
            this.reproject = reproject;
        }
    }

    @XmlElement(name = "rule")
    private List<Rule> rules = new ArrayList<GridReprojectionRules.Rule>(0);

    public List<Rule> getRules() {
        return rules;
    }

}
