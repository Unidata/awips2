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
import de.micromata.opengis.kml.v_2_2_0.Folder;
import de.micromata.opengis.kml.v_2_2_0.Style;

/**
 * The Folder output does not actually contain a KMZ stream but allows the
 * parent to handle all file io.
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

public class KmlFolderOutputManager implements KmlOutputManager {

    protected final KmlOutputManager parent;

    protected final Folder folder;

    private int imageCounter = 1;

    public KmlFolderOutputManager(KmlOutputManager parent, Folder folder) {
        this.parent = parent;
        this.folder = folder;
    }

    @Override
    public KmlOutputManager createFolder(String name) {
        return new KmlFolderOutputManager(this, folder.createAndAddFolder()
                .withName(name));
    }

    @Override
    public String addImage(RenderedImage image) {
        return addImage(image, "image" + (imageCounter++) + ".png");
    }

    @Override
    public String addImage(RenderedImage image, String name) {
        return parent.addImage(image, folder.getName() + "/" + name);
    }

    @Override
    public String resolvePath(String name) {
        return parent.resolvePath(folder.getName() + "/" + name);
    }

    @Override
    public String getStyleUrl(Style style) {
        return parent.getStyleUrl(style);
    }

    @Override
    public void addFeature(Feature feature) {
        folder.addToFeature(feature);
    }

    @Override
    public Container getContainer() {
        return folder;
    }

}
