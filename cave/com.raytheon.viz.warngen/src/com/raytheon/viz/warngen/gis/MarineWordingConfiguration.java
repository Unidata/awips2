package com.raytheon.viz.warngen.gis;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.warning.util.WarnFileUtil;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.warngen.gui.WarngenLayer;

/**
 * WarngenWordingConfiguration
 *
 * <pre>
 *    SOFTWARE HISTORY
 *
 *    Date         Ticket#     Engineer       Description
 *    ------------ ----------  -------------- --------------------------
 *    2014-08-28   ASM #15658  D. Friedman    Initial Creation.
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "zoneWordingConfig")
public class MarineWordingConfiguration {

    private static final String FILE_NAME = "marineZoneWording.xml";

    @XmlElement(name = "entry")
    private List<MarineWordingEntry> entries = new ArrayList<MarineWordingEntry>();

    public List<MarineWordingEntry> getEntries() {
        return entries;
    }

    public void setEntries(List<MarineWordingEntry> entries) {
        this.entries = entries;
    }

    @XmlAccessorType(XmlAccessType.NONE)
    public static class MarineWordingEntry {
        @XmlAttribute(name = "match")
        private String matchText;
        @XmlAttribute(name = "replace")
        private String replacementText;

        private Pattern ugcPattern;

        public String getMatchText() {
            return matchText;
        }

        public void setMatchText(String matchText) {
            this.matchText = matchText;
            this.ugcPattern = null;
        }

        public String getReplacementText() {
            return replacementText;
        }

        public void setReplacementText(String replacementText) {
            this.replacementText = replacementText;
        }

        public Pattern getUgcPattern() {
            if (ugcPattern == null) {
                if (matchText != null) {
                    ugcPattern = Pattern.compile(matchText);
                }
            }
            return ugcPattern;
        }
    }

    private static final SingleTypeJAXBManager<MarineWordingConfiguration> jaxb = SingleTypeJAXBManager
            .createWithoutException(MarineWordingConfiguration.class);


    public static MarineWordingConfiguration load(WarngenLayer forLayer) throws Exception {
        String xmlText = WarnFileUtil.convertFileContentsToString(FILE_NAME,
                LocalizationManager.getInstance().getCurrentSite(),
                forLayer.getLocalizedSite());

        MarineWordingConfiguration config = (MarineWordingConfiguration)
                jaxb.unmarshalFromXml(xmlText);
        for (MarineWordingEntry entry : config.getEntries()) {
            // Validate patterns by compiling now.
            entry.getUgcPattern();
        }
        return config;
    }

}
