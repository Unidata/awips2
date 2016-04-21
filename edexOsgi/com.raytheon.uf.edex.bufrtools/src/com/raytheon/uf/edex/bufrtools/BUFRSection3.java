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
package com.raytheon.uf.edex.bufrtools;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.edex.bufrtools.descriptors.BUFRDescriptor;
import com.raytheon.uf.edex.bufrtools.descriptors.BUFRNullDescriptor;
import com.raytheon.uf.edex.bufrtools.descriptors.BUFRReplicationDescriptor;
import com.raytheon.uf.edex.bufrtools.descriptors.BUFRTableB;
import com.raytheon.uf.edex.bufrtools.descriptors.DescriptorFactory;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071127            382 jkorman     Initial Coding.
 * 20080204            862 jkorman     Changes due to BUFRSEparator refactor.
 * 9/16/2014    #3628      mapeters    Moved from uf.edex.decodertools plugin.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class BUFRSection3 extends BUFRSection {
    private int numSubSets;

    private List<BUFRDescriptor> rawDescriptors;

    private List<BUFRDescriptor> descriptors;
    
    private boolean isCompressed = false;

    private boolean isObserved = false;

    private DescriptorFactory descriptorFactory;
    
    BUFRSection3(DescriptorFactory factory,ByteBuffer dataBuffer) {
        super(dataBuffer);
        descriptorFactory = factory;
        
        numSubSets = getInteger(2, 4);

        int numDescriptors = (getSectionSize() - 7) / 2;
        
        rawDescriptors = new ArrayList<BUFRDescriptor>();
        
        int x = getInteger(1, 6);
        isObserved = (x & 0x80) > 0;
        isCompressed = (x & 0x40) > 0;
        boolean valid = (numSubSets > 0);
        for (int i = 0; i < numDescriptors; i++) {
            int descriptor = getInteger(2, (7 + (i * 2)));

            BUFRDescriptor d = descriptorFactory.getDescriptor(descriptor);
            if (d == null) {
                // we may need to define a descriptor without it being in the
                // table definitions. Descriptor [2,6,y] will take care of the
                // data width.
                // TODO : This should be part of the validity checks!
                d = new BUFRNullDescriptor(descriptor);
            }
            rawDescriptors.add(d);
        }
        descriptors = new ArrayList<BUFRDescriptor>();
        descriptors.addAll(rawDescriptors);
        expandAll(descriptors);
        rawDescriptors.clear();
        for(BUFRDescriptor d : descriptors) {
            rawDescriptors.add(d.copy());
        }
        descriptors.clear();
        descriptors = rawDescriptors;
        
        setValidity(valid);
    }

    public int getNumSubSets() {
        return numSubSets;
    }

    /**
     * 
     * @param descriptor
     * @param indent
     */
    private void displayDescriptor(BUFRDescriptor descriptor, String indent) {
        System.out.println(descriptor.getStringDescriptor());
        if (descriptor.getSubList() != null) {
            System.out.println("[");
            display(descriptor.getSubList(), indent + "  ");
            System.out.println("]");
        }
    }

    /**
     * 
     */
    public void display(List<BUFRDescriptor> list, String indent) {
        for (BUFRDescriptor d : list) {
            displayDescriptor(d, "");
        }
    }
    
    /**
     * 
     */
    public void display() {
        System.out.println(String.format("Section 3: Number Subsets = %6d\n",
                numSubSets));
        display(descriptors, "");
        System.out.println();
    }

    /**
     * 
     */
    public StringBuilder getStringData(StringBuilder buffer) {
        buffer = super.getStringData(buffer);
        
        String s = (rawDescriptors.size() > 1) ? "s" : "";
        
        buffer.append(String.format("Section 3: Total Descriptor%1s = %6d Subsets = %5d\n",
                s,rawDescriptors.size(),numSubSets));
        
        String s1 = (isObserved) ? "X" : " ";
        String s2 = (isCompressed) ? "X" : " ";
        
        buffer.append(String.format("         : Observed [%1s] Compressed [%1s]\n",
                s1, s2));
        for (BUFRDescriptor d : rawDescriptors) {
            buffer.append("      ");
            d.getStringData(buffer);
            buffer.append("\n");
        }
        return buffer;
    }

    /**
     * 
     * @return
     */
    public List<BUFRDescriptor> getDescriptorList() {
        return descriptors;
    }

    /**
     * @return the isCompressed
     */
    public boolean isCompressed() {
        return isCompressed;
    }

    /**
     * @return the isObserved
     */
    public boolean isObserved() {
        return isObserved;
    }

    /**
     * 
     */
    private boolean expand(List<BUFRDescriptor> descriptors) {

        boolean expanded = false;

        for (int currDescriptor = 0; currDescriptor < descriptors.size();) {
            BUFRDescriptor d = descriptors.get(currDescriptor);

            switch (d.getF()) {

            case 0:
            case 2: {
                currDescriptor++;
                break;
            }
            case 1: {
                currDescriptor++;
                BUFRReplicationDescriptor rd = (BUFRReplicationDescriptor) d;
                if(rd.getSubList() == null) {
                    if (d.getY() == 0) {
                        // delayed replication
                        // Should see a Table B delayed replication operator
                        BUFRDescriptor d1 = descriptors.remove(currDescriptor);
                        if (d1 instanceof BUFRTableB && (d1.getX() == 31)) {
                            rd.setDelayedSuffix((BUFRTableB) d1);
                        } else {
                            // this is an error.
                        }
                    }
                    // now move x descriptors into the replication list.
                    int repCount = d.getX();
                    
                    List<BUFRDescriptor> repList = new ArrayList<BUFRDescriptor>(); 
                    for(int i = 0;i < repCount;i++) {
                        BUFRDescriptor d2 = descriptors.remove(currDescriptor);
                        repList.add(d2);
                    }
                    d.setSubList(repList);
                    expanded = true;
                    // Need to expand the sublist be proceeding!
                    while(expand(d.getSubList()));
                } else {
                    // Check to see if the sublist needs expansion.
                    // This will not occur on the initial pass through the descriptors.
                    while(expand(d.getSubList()));
                }
                continue;
            }
            case 3: {
                // first remove the descriptor
                descriptors.remove(currDescriptor);
                List<BUFRDescriptor> dList = d.getSubList();
                if(dList != null) {
                    for(int i = dList.size()-1;i >= 0;i--) {
                        BUFRDescriptor td = dList.get(i);
                        if(td instanceof BUFRReplicationDescriptor) {
                            td = descriptorFactory.getDescriptor(td.getDescriptor());
                        }
                        descriptors.add(currDescriptor,td);
                    }
                } else {
                    // this is really bad.
                    System.out.println("Expansion failed on descriptor " + currDescriptor + " : " + d);
                    return false;
                }
                expanded = true;
                break;
            }
            } // switch()

        } // for

        return expanded;
    }
    
    private List<BUFRDescriptor> expandAll(List<BUFRDescriptor> descriptors) {
        
        while(expand(descriptors));
        
        return descriptors;
        
    }
    
}
