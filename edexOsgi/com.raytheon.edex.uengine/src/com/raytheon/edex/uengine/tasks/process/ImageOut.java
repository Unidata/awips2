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

package com.raytheon.edex.uengine.tasks.process;

import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import javax.imageio.ImageIO;

import org.geotools.coverage.grid.GridCoverage2D;
import org.geotools.coverage.grid.GridCoverageFactory;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.gce.geotiff.GeoTiffWriter;
import org.geotools.geometry.Envelope2D;

import com.raytheon.edex.uengine.exception.MicroEngineException;
import com.raytheon.edex.uengine.tasks.ScriptTask;

/**
 * ImageOut task derived from original uEngine ImageOut task.  Writes an image out
 * to a byte array in the specified format.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * Mar 29, 2007                     njensen             Initial Creation
 *
 * </PRE>
 *
 */
public class ImageOut extends ScriptTask
{
    private BufferedImage image;

    private String format;

    private GridGeometry2D gridGeometry;
    
    /**
     * Constructor
     * @param anImage the image to write out
     * @param aFormat the format to write the image out to
     * @param aGridGeometry the geometry of the image, only required for geotiffs
     */
    public ImageOut(BufferedImage anImage, String aFormat, GridGeometry2D aGridGeometry)
    {
        image = anImage;
        format = aFormat;
        gridGeometry = aGridGeometry;
    }
    
    /**
     * Constructor
     * @param anImage the image to write out
     * @param aFormat the format to write the image out to
     */
    public ImageOut(BufferedImage anImage, String aFormat)
    {
        image = anImage;
        format = aFormat;
    }

    @Override
    public Object execute()
    {
        ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
        try
        {
            logger.debug("converting buffered image to file format"
                    + ", image size="
                    + (image != null ? (image.getWidth() * image.getHeight())
                            : "N/A"));
            if ("tiff".equalsIgnoreCase(format)
                    || "tif".equalsIgnoreCase(format))
            {

                // Get the envelope (contains the CRS).
                Envelope2D e = gridGeometry.getEnvelope2D();

                // Create the grid coverage.
                GridCoverageFactory factory = new GridCoverageFactory();
                GridCoverage2D coverage = factory.create("SomeName", image, e);

                // Create the geotiff writer and write the geotiff to
                // the
                // output stream.
                GeoTiffWriter tiffWriter = new GeoTiffWriter(byteOut);
                tiffWriter.write(coverage, null);                
            }
            else
            {
                ImageIO.write(image, format, byteOut);
            }
        }
        catch (IOException e)
        {
            throw new MicroEngineException("Error writing image out.", e);
        }

        return byteOut.toByteArray();
    }

    public String getFormat()
    {
        return format;
    }

    public void setFormat(String aFormat)
    {
        format = aFormat;
    }

    public GridGeometry2D getGridGeometry()
    {
        return gridGeometry;
    }

    public void setGridGeometry(GridGeometry2D aGridGeometry)
    {
        gridGeometry = aGridGeometry;
    }

    public BufferedImage getImage()
    {
        return image;
    }

    public void setImage(BufferedImage aImage)
    {
        image = aImage;
    }
        

}
