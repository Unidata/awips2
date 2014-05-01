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
package com.raytheon.uf.common.colormap;

import javax.xml.bind.annotation.XmlAccessOrder;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorOrder;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

/**
 *
 * Implementation of Color class used in ColorMap
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 20, 2008            bclements     Initial creation
 * Jan 10, 2013 15648      ryu         Added equals() method.
 * </pre>
 *
 * @author bclements
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlAccessorOrder(XmlAccessOrder.UNDEFINED)
public class Color {
    @XmlAttribute(name = "a")
    private float alpha;

    @XmlAttribute(name = "b")
    private float blue;

    @XmlAttribute(name = "g")
    private float green;

    @XmlAttribute(name = "r")
    private float red;

    public Color() {
    }

    public Color(float r, float g, float b, float a) {
        red = r;
        green = g;
        blue = b;
        alpha = a;
    }

    public Color(float r, float g, float b) {
        red = r;
        green = g;
        blue = b;
        alpha = 1.0f;
    }

    public float getAlpha() {
        return alpha;
    }

    public float getRed() {
        return red;
    }

    public float getBlue() {
        return blue;
    }

    public float getGreen() {
        return green;
    }

    public void setAlpha(float f) {
        alpha = f;
    }

    public void setRed(float f) {
        red = f;
    }

    public void setBlue(float f) {
        blue = f;
    }

    public void setGreen(float f) {
        green = f;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Color) {
            Color c = (Color) obj;
            if (alpha == c.alpha &&
                    red == c.red &&
                    blue == c.blue &&
                    green == c.green)
                return true;
        }

        return false;
    }
}
