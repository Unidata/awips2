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
package com.raytheon.viz.shapefile.rsc;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.List;

import org.geotools.data.shapefile.indexed.IndexedShapefileDataStore;
import org.opengis.feature.type.AttributeDescriptor;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 11, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ShapefileUtil {

    public static String[] getAttributes(File shapefile) throws VizException {
        return getAttributes(shapefile, true);
    }

    public static String[] getAttributes(File aShapeFile,
            boolean includeGeometry) throws VizException {

        try {
            IndexedShapefileDataStore ds = new IndexedShapefileDataStore(
                    aShapeFile.toURI().toURL(), false, true);

            return getAttributes(ds, includeGeometry);

        } catch (MalformedURLException e) {
            throw new VizException("Bad file", e);
        } catch (IOException e) {
            throw new VizException("Error reading shapefile ", e);
        }
    }

    public static String[] getAttributes(IndexedShapefileDataStore ds,
            boolean includeGeometry) throws IOException {
        String[] retVal = null;
        List<AttributeDescriptor> at = ds.getSchema().getAttributeDescriptors();
        if (at == null || at.size() == 0) {
            return null;
        }

        if (includeGeometry) {
            retVal = new String[at.size()];
        } else {
            retVal = new String[at.size() - 1];
        }

        int j = 0;

        for (int i = 0; i < at.size(); i++) {
            if (includeGeometry || !at.get(i).getLocalName().equals("the_geom")) {
                retVal[j] = at.get(i).getLocalName();
                j++;
            }
        }
        return retVal;
    }

}
