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
package com.raytheon.uf.common.dataplugin.warning.config;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;

/**
 * Bullet Action Group
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ------------------------------------------
 * Aug 15, 2017  6328     randerso  Allow both <bullet> and <presetInfoBullet>
 *                                  in the <bullets> section
 *                                  Deprecated <presetInfoBullets>
 *
 * </pre>
 *
 */
@XmlAccessorType(XmlAccessType.NONE)
public class BulletActionGroup {
    /** The action this group is to be used with */
    @XmlAttribute
    private String action;

    /** The phenomena this group is to be used with */
    @XmlAttribute
    private String phen;

    /** The significance this group is to be used with */
    @XmlAttribute
    private String sig;

    @XmlElementWrapper(name = "bullets")
    @XmlAnyElement(lax = true)
    private Bullet[] bullets;

    /* TODO: remove in later release */
    @Deprecated
    @XmlElementWrapper(name = "presetInfoBullets")
    @XmlElement(name = "presetInfoBullet")
    private PresetInfoBullet[] presetInfoBullets;

    /**
     * @return the action
     */
    public String getAction() {
        return action;
    }

    /**
     * @param action
     *            the action to set
     */
    public void setAction(String action) {
        this.action = action;
    }

    /**
     * @return the phen
     */
    public String getPhen() {
        return phen;
    }

    /**
     * @param phen
     *            the phen to set
     */
    public void setPhen(String phen) {
        this.phen = phen;
    }

    /**
     * @return the sig
     */
    public String getSig() {
        return sig;
    }

    /**
     * @param sig
     *            the sig to set
     */
    public void setSig(String sig) {
        this.sig = sig;
    }

    /**
     * @return the bullets
     */
    public Bullet[] getBullets() {
        return bullets;
    }

    /**
     * @param bullets
     *            the bullets to set
     */
    public void setBullets(Bullet[] bullets) {
        this.bullets = bullets;
    }

    /* TODO: remove in later release */
    @Deprecated
    public PresetInfoBullet[] getPresetInfoBullets() {
        return presetInfoBullets;
    }

    /* TODO: remove in later release */
    @Deprecated
    public void setPresetInfoBullets(PresetInfoBullet[] presetInfoBullets) {
        this.presetInfoBullets = presetInfoBullets;
    }
}
