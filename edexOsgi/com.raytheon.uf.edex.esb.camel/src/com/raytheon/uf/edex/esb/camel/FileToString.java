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
package com.raytheon.uf.edex.esb.camel;

import java.io.File;

import org.apache.camel.Exchange;
import org.apache.camel.Processor;
import org.apache.camel.component.file.GenericFile;
import org.springframework.util.FileCopyUtils;

import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * Provides a capability to transform java.io.File to Strings
 * 
 * This is necessary because camel transforms java.io.Files to byte[] on JMS
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 03, 2008            chammack    Initial creation
 * Jun 12, 2012 00609      djohnson    Use EDEXUtil for EDEX_HOME.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class FileToString implements Processor {

    private static String DIR = EDEXUtil.EDEX_HOME
            + File.separator + "data" + File.separator + "processing";

    @Override
    public void process(Exchange arg0) throws Exception {
        File file = (File) ((GenericFile<?>) arg0.getIn().getBody()).getFile();

        String fileName = file.getName();
        String path = DIR + File.separator;
        File dir = new File(path);
        if (!dir.exists())
            dir.mkdirs();

        File newFile = new File(path + fileName);

        // Have to do this because File.renameTo is not cross-fs safe
        FileCopyUtils.copy(file, newFile);
        file.delete();

        arg0.getIn().setBody(newFile.toString());
        arg0.getIn().setHeader("enqueueTime", System.currentTimeMillis());
    }

}
