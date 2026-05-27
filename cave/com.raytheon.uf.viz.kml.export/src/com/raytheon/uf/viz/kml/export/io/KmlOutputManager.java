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
package com.raytheon.uf.viz.kml.export.io;

import java.awt.image.RenderedImage;

import de.micromata.opengis.kml.v_2_2_0.Container;
import de.micromata.opengis.kml.v_2_2_0.Feature;
import de.micromata.opengis.kml.v_2_2_0.Style;

/**
 * Wrapper around a KMZ file as well as a container java object, each folder an
 * outputManager creates will be a folder in the KML view as well as a folder in
 * the kmz file for any image resources that are stored.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 1, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public interface KmlOutputManager {

    public KmlOutputManager createFolder(String name);

    public String addImage(RenderedImage image);

    public String addImage(RenderedImage image, String name);

    public String resolvePath(String name);

    public String getStyleUrl(Style style);

    public void addFeature(Feature feature);

    public Container getContainer();

}
