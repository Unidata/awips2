package com.raytheon.viz.warngen.gui;

import com.raytheon.uf.common.dataplugin.warning.config.Bullet;

public class BulletData extends Bullet {

    public boolean index;

    public boolean isLocked;

    public BulletData(Bullet bullet) {
        setBulletGroup(bullet.getBulletGroup());
        setBulletName(bullet.getBulletName());
        setBulletText(bullet.getBulletText());
        setBulletType(bullet.getBulletType());
    }
}
