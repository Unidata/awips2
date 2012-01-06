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
package com.raytheon.uf.viz.alertviz.config;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.message.StatusMessage;

/**
 * 
 * Container for "office overrides" or forced AlertViz settings
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 26, 2010 2459       dfriedma     Initial creation
 * </pre>
 * 
 * @author dfriedma
 * @version 1.0
 */
@XmlRootElement
public class ForcedConfiguration {
    private ArrayList<ForcedItem> items = new ArrayList<ForcedItem>();

    /**
     * @return the items
     */
    @XmlElement(name="item")
    public List<ForcedItem> getItems() {
        return items;
    }

    /**
     * @param items
     *            the items to set
     */
    public void setItems(List<ForcedItem> items) {
        this.items = new ArrayList<ForcedItem>(items);
    }

    public ForcedItem lookup(StatusMessage statusMessage) {
        for (ForcedItem fi : items) {
            /*
             * Check if source key and priority match. If the forced item
             * defines a category, check if it matches. If the forced item
             * defines a text match, search for it in the status message's 
             * text.
             */
            if (fi.getSourceKey() != null
                    && statusMessage.getSourceKey() != null
                    && fi.getSourceKey().equals(statusMessage.getSourceKey())
                    && fi.getPriority() != null
                    && statusMessage.getPriority() != null
                    && fi.getPriority().equals(statusMessage.getPriority())
                    && (fi.getCategoryKey() == null || (statusMessage
                            .getCategory() != null && fi.getCategoryKey()
                            .equals(statusMessage.getCategory())))
                    && (fi.getTextMatch() == null
                            || fi.getTextMatch().length() == 0 || (statusMessage
                            .getMessage() != null && statusMessage.getMessage()
                            .indexOf(fi.getTextMatch()) >= 0))) {
                return fi;
            }
        }
        return null;
    }
    
    /** Applies forced settings to the given AlertMetadata as appropriate 
     * for then given StatusMessage.
     * 
     * <p>The input AlertMetadata will not be modified.
     * 
     * @param metadata the input metadata
     * @param message the status message
     * @return a possibly new AlertMetadata with the forced settings applied
     */
    public AlertMetadata applyForcedSettings(AlertMetadata metadata, StatusMessage message) {
        ForcedItem forcedItem = lookup(message);
        if (forcedItem != null)
            return forcedItem.applyForcedSettings(metadata);
        else
            return metadata;
    }
}
