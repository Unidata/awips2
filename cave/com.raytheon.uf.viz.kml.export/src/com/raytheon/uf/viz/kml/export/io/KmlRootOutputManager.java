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
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.imageio.ImageIO;
import javax.xml.bind.JAXB;

import de.micromata.opengis.kml.v_2_2_0.Container;
import de.micromata.opengis.kml.v_2_2_0.Document;
import de.micromata.opengis.kml.v_2_2_0.Feature;
import de.micromata.opengis.kml.v_2_2_0.Kml;
import de.micromata.opengis.kml.v_2_2_0.Style;

/**
 * The root output manager holds a the base output stream as well as the root
 * KML Document.
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

public class KmlRootOutputManager implements KmlOutputManager {

    private Set<String> names = new HashSet<String>();

    private final Kml kml;

    private final Document document;

    private IOException exception;

    private int imageCounter = 1;

    private int styleCounter = 1;

    private Map<Style, String> styleMap = new HashMap<Style, String>();

    private final ZipOutputStream zos;

    public KmlRootOutputManager(File kmzFile) throws FileNotFoundException {
        this.zos = new ZipOutputStream(new FileOutputStream(kmzFile));
        this.kml = new Kml();
        this.document = this.kml.createAndSetDocument();
    }

    @Override
    public KmlOutputManager createFolder(String name) {
        return new KmlFolderOutputManager(this, document.createAndAddFolder()
                .withName(name));
    }

    @Override
    public String addImage(RenderedImage image) {
        return addImage(image, "image" + (imageCounter++) + ".png");
    }

    @Override
    public String addImage(RenderedImage image, String name) {
        if (names.contains(name)) {
            String newName = name;
            for (int i = 1; names.contains(newName); i++) {
                newName = name.replace(".png", "-" + i + ".png");
            }
            name = newName;
        }
        names.add(name);
        synchronized (zos) {
            try {
                zos.putNextEntry(new ZipEntry(name));
                ImageIO.write(image, "png", zos);
                zos.closeEntry();
            } catch (IOException e) {
                exception = e;
            }
        }
        return name;
    }

    @Override
    public String resolvePath(String name) {
        return name;
    }

    @Override
    public synchronized String getStyleUrl(Style style) {
        Style key = style.clone().withId(null);
        String id = styleMap.get(key);
        if (id == null) {
            id = style.getId();
            if (id == null) {
                id = "style" + this.styleCounter++;
            }
            styleMap.put(key, id);
            style.setId(id);
            document.addToStyleSelector(style);
        }
        return "#" + id;
    }

    @Override
    public void addFeature(Feature feature) {
        document.addToFeature(feature);
    }

    @Override
    public Container getContainer() {
        return document;
    }

    /**
     * Closes the kmz stream and serializes the kml document.
     * 
     * @throws IOException
     */
    public void close() throws IOException {
        if (exception != null) {
            throw exception;
        }
        zos.putNextEntry(new ZipEntry("doc.kml"));
        JAXB.marshal(kml, zos);
        zos.closeEntry();
        zos.close();
    }

}
