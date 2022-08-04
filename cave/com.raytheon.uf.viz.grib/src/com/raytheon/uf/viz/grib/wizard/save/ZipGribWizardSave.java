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
package com.raytheon.uf.viz.grib.wizard.save;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.xml.bind.JAXB;

/**
 * 
 * Implementation of {@link GribWizardSave} which saves the data to a zip file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Apr 26, 2016  5572     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class ZipGribWizardSave extends GribWizardSave {

    private ZipOutputStream zos;

    public ZipGribWizardSave(Path location) throws IOException {
        this.zos = new ZipOutputStream(Files.newOutputStream(location));

    }

    @Override
    protected void save(String location, Object object) throws Exception {
        zos.putNextEntry(new ZipEntry(location));
        JAXB.marshal(object, zos);
        zos.closeEntry();
    }

    public void close() throws IOException {
        zos.close();
    }

}
