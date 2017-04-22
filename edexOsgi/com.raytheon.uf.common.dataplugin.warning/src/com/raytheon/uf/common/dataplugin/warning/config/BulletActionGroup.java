package com.raytheon.uf.common.dataplugin.warning.config;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;

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
    @XmlElement(name = "bullet")
    private Bullet[] bullets;

    @XmlElementWrapper(name = "damInfoBullets")
    @XmlElement(name = "damInfoBullet")
    private DamInfoBullet[] damInfoBullets;

    public String getAction() {
        return action;
    }

    public void setAction(String action) {
        this.action = action;
    }

    public String getPhen() {
        return phen;
    }

    public void setPhen(String phen) {
        this.phen = phen;
    }

    public String getSig() {
        return sig;
    }

    public void setSig(String sig) {
        this.sig = sig;
    }

    public Bullet[] getBullets() {
        return bullets;
    }

    public void setBullets(Bullet[] bullets) {
        this.bullets = bullets;
    }

    public DamInfoBullet[] getDamInfoBullets() {
        return damInfoBullets;
    }

    public void setDamInfoBullets(DamInfoBullet[] damInfoBullets) {
        this.damInfoBullets = damInfoBullets;
    }
}
