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
package com.raytheon.edex.plugin.bufrua.decoder;

import java.io.IOException;

import ucar.ma2.StructureData;
import ucar.ma2.StructureDataIterator;
import ucar.nc2.Structure;

/**
 * An iterator over a sequence of {@link BufrStructure}. This class just wraps a
 * {@link Structure} and a {@link StructureDataIterator} and uses the same
 * Structure to create a {link BufrStructure for each {@link StructureData} in
 * the wrapped iterator
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Jul 08, 2016  5736     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class BufrStructureIterator {

    private final Structure structure;

    private final StructureDataIterator dataIterator;

    public BufrStructureIterator(Structure structure,
            StructureDataIterator dataIterator) {
        this.structure = structure;
        this.dataIterator = dataIterator;
    }

    public boolean hasNext() throws IOException {
        return dataIterator.hasNext();
    }

    public BufrStructure next() throws IOException {
        return new BufrStructure(structure, dataIterator.next());
    }

}
