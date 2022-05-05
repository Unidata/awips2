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

package com.raytheon.viz.aviation.utility;

import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;

/**
 * Common image utility methods used in aviation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 11, 2011 8849       rferrel     Removed unused methods solving a
 *                                     NoClassDefFoundError.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class ImageUtil {
    private ImageUtil() {
    }

    /**
     * Rotate an image 90 degrees.
     * 
     * @param image
     *            The object to rotate
     * @return rotatedImage
     */
    public static Image rotateImage(Image image) {
        ImageData sd = image.getImageData();
        ImageData dd = new ImageData(sd.height, sd.width, sd.depth, sd.palette);

        for (int sx = 0; sx < sd.width; sx++) {
            for (int sy = 0; sy < sd.height; sy++) {
                int dx = sy;
                int dy = sd.width - sx - 1;
                dd.setPixel(dx, dy, sd.getPixel(sx, sy));
            }
        }
        return new Image(image.getDevice(), dd);
    }
}
