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

package com.raytheon.uf.viz.core.style;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Contains a set of style rules.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 21, 2007            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 */
@XmlRootElement(name = "styleRuleset")
@XmlAccessorType(XmlAccessType.NONE)
public class StyleRuleset implements ISerializableObject {

    @XmlElement(name = "styleRule")
    private ArrayList<StyleRule> styleRules = new ArrayList<StyleRule>();

    /**
     * @return the styleRules
     */
    public ArrayList<StyleRule> getStyleRules() {
        return styleRules;
    }

    /**
     * @param styleRules
     *            the styleRules to set
     */
    public void setStyleRules(ArrayList<StyleRule> styleRules) {
        this.styleRules = styleRules;
    }

    public void addStyleRules(StyleRuleset ruleset) {
        for (StyleRule sr : ruleset.getStyleRules()) {
            styleRules.add(sr);
        }
    }

}
