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
package com.raytheon.edex.plugin.warning.gis;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 24, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class GeospatialDataGeneratorThread {
    public GeospatialDataGeneratorThread() {
        Thread t = new Thread("WarngenGeometryGenerator") {
            public void run() {
                // TODO: Move to camel timer, when camel upgraded to at least
                // 2.8 and take advantage of single run timer
                try {
                    // delay to allow server to start
                    Thread.sleep(120000);
                } catch (InterruptedException e) {
                    // ignore
                }
                GeospatialDataGenerator
                        .generateUniqueGeospatialMetadataGeometries();
                // scan and clean old geometries
            }
        };
        t.start();
    }
}
