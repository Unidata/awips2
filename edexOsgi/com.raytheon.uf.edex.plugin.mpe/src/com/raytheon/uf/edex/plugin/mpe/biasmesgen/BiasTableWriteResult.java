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
package com.raytheon.uf.edex.plugin.mpe.biasmesgen;

import java.nio.file.Path;

/**
 * POJO containing basic identifying information about a Bias Table file that
 * had recently been written. TODO: in the future, this POJO may need to be
 * successfully dynamically serialized so that it can be used to notify
 * downstream components that a Bias Table file had been written.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 9, 2016  5576       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class BiasTableWriteResult {

    private Integer bytesWritten;

    private Path outputPath;

    public BiasTableWriteResult() {
    }

    public BiasTableWriteResult(Integer bytesWritten, Path outputPath) {
        this.bytesWritten = bytesWritten;
        this.outputPath = outputPath;
    }

    public Integer getBytesWritten() {
        return bytesWritten;
    }

    public void setBytesWritten(Integer bytesWritten) {
        this.bytesWritten = bytesWritten;
    }

    public Path getOutputPath() {
        return outputPath;
    }

    public void setOutputPath(Path outputPath) {
        this.outputPath = outputPath;
    }
}