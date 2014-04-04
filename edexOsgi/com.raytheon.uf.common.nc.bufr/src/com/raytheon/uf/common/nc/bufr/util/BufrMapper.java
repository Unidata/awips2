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
package com.raytheon.uf.common.nc.bufr.util;

import java.io.File;
import java.util.Collection;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.util.mapping.Mapper;

/**
 * Mapping for BUFR field aliases to point data container parameters
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 27, 2014 2905       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class BufrMapper extends Mapper {

    /**
     * @throws JAXBException
     */
    public BufrMapper(Collection<File> aliasFiles) throws JAXBException {
        for (File aliasFile : aliasFiles) {
            addAliasList(aliasFile);
        }
    }

}
