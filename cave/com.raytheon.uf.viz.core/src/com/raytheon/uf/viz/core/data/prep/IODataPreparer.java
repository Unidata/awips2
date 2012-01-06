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
package com.raytheon.uf.viz.core.data.prep;

import java.awt.image.RenderedImage;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import javax.imageio.ImageIO;

import com.raytheon.uf.viz.core.data.IImageDataPreparer;
import com.raytheon.uf.viz.core.data.resp.IOImageData;

/**
 * 
 * DEPRECATED: Use IRenderedImageCallback with IGraphicsTarget instead. Make
 * sure RenderedImage can be recreated from scratch (loaded from file,
 * generated, etc)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 24, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 */
@Deprecated
public class IODataPreparer implements IImageDataPreparer {

    private String imageFile;

    private InputStream stream;

    private IOImageData data;

    public IODataPreparer(RenderedImage image, String name, int border) {
        this.data = new IOImageData(image, border, name);
    }

    public IODataPreparer(String fileLoc, int border) {
        this.data = new IOImageData(null, border, fileLoc);
        imageFile = fileLoc;
    }

    public IODataPreparer(InputStream stream, String name, int border) {
        this.data = new IOImageData(null, border, name);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.data.interfaces.IDataRequester#prepareData()
     */
    @Override
    public IOImageData prepareData() {
        if (data.getImage() != null) {
            return data;
        }
        if (imageFile != null) {
            try {
                data.setImage(ImageIO.read(new File(imageFile)));
            } catch (IOException e) {
                e.printStackTrace();
            }
        } else if (stream != null) {
            try {
                data.setImage(ImageIO.read(stream));
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        return data;
    }

}
