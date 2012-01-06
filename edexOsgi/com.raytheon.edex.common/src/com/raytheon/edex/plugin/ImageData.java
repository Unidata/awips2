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

package com.raytheon.edex.plugin;

import java.io.Serializable;

public class ImageData implements Serializable {

    /**
     * Default serial version UID
     */
    private static final long serialVersionUID = 1L;

    private int type;

    private int width;

    private int height;

    private int[] rgb;

    public ImageData(int width, int height, int[] rgb, int type) {
        this.width = width;
        this.height = height;
        this.rgb = rgb;
        this.type = type;
    }

    public int getWidth() {
        return width;
    }

    public int getHeight() {
        return height;
    }

    public int[] getRGB() {
        return rgb;
    }

    public int getType() {
        return type;
    }

}
