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
package com.raytheon.uf.viz.datadelivery.subscription.approve;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.DataLevelType;
import com.raytheon.uf.common.datadelivery.registry.Parameter;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 21, 2012            mpduff       Initial creation
 * Jul 25, 2012 955        djohnson     Use List instead of ArrayList.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class ParameterDiff {
    private final String nl = "\n";
    
    private Parameter parameter1;

    private Parameter parameter2;

    private boolean different;

    private boolean initRequired = true;
    
    private final StringBuilder buffer = new StringBuilder();


    /**
     * Default Constructor
     */
    public ParameterDiff() {

    }

    /**
     * Constructor.
     * 
     * @param parameter1
     *            Original Parameter
     * @param parameter2
     *            Edited Parameter
     */
    public ParameterDiff(Parameter parameter1, Parameter parameter2) {
        this.parameter1 = parameter1;
        this.parameter2 = parameter2;
    }

    /**
     * @return the parameter1
     */
    public Parameter getParameter1() {
        return parameter1;
    }

    /**
     * @param parameter1
     *            the parameter1 to set
     */
    public void setParameter1(Parameter parameter1) {
        this.parameter1 = parameter1;
    }

    /**
     * @return the parameter2
     */
    public Parameter getParameter2() {
        return parameter2;
    }

    /**
     * @param parameter2
     *            the parameter2 to set
     */
    public void setParameter2(Parameter parameter2) {
        this.parameter2 = parameter2;
    }

    /**
     * Are these Parameter objects different?
     * 
     * @return true if different
     */
    public boolean isDifferent() {
        if (initRequired) {
            initRequired = false;
            List<DataLevelType> dataLevelTypeList1 = parameter1.getLevelType();

            for (DataLevelType dlt : dataLevelTypeList1) {
                List<DataLevelType> dataLevelTypeList2 = parameter2
                        .getLevelType();

                for (DataLevelType dlt2 : dataLevelTypeList2) {
                    if (dlt.getId() == dlt2.getId() && dlt.getType().equals(dlt2.getType())) {
                        // Check the levels
                        if (dlt.getLayer() != null) {
                            buffer.append(nl);
                            buffer.append("Parameter: ").append(parameter1.getName()).append(nl);
                            buffer.append("LevelId: ").append(dlt.getId()).append(" Type: ").append(dlt.getType()).append(nl);
                            buffer.append(dlt.getDescription()).append(":").append(nl);
                            List<Integer> param1Indices = parameter1.getLevels().getSelectedLevelIndices();
                            List<Integer> param2Indices = parameter2.getLevels().getSelectedLevelIndices();

                            List<Double> originalList = new ArrayList<Double>();
                            List<Double> newList = new ArrayList<Double>();
                            List<Double> removedList = new ArrayList<Double>();
                            List<Double> addedList = new ArrayList<Double>();
                            
                            if (!param1Indices.isEmpty()
                                    || !param2Indices.isEmpty()) {
                                for (int i: param1Indices) {
                                    originalList.add(parameter2.getLevels().getLevel().get(i));
                                }
                                
                                for (int i: param2Indices) {
                                    newList.add(parameter1.getLevels().getLevel().get(i));
                                }
                                
                                String s = this.getDiffs(originalList, newList);
                                if (s != null) {
                                    buffer.append(s);
                                    different = true;
                                }
                                
//                                for (double d: originalList) {
//                                    if (!newList.contains(d)) {
//                                        removedList.add(d);
//                                    }
//                                }
//                                
//                                for (double d: newList) {
//                                    if (!originalList.contains(d)){
//                                        addedList.add(d);
//                                    }
//                                }
//                                
//                                buffer.append("  Modified Levels").append(nl);
//                                for (int i: param2Indices) {
//                                    buffer.append("  ").append(parameter2.getLevels().getLevel().get(i));
//                                }
//                                buffer.append(nl);
//                                
//                                if (addedList.size() > 0) {
//                                    buffer.append("Added Levels: ");
//                                    for (Double d: addedList) {
//                                        buffer.append(d).append("  ");
//                                    }
//                                    buffer.append(nl);
//                                }                                
//                                
//                                if (removedList.size() > 0) {
//                                    buffer.append("Removed Levels: ");
//                                    for (Double d: removedList) {
//                                        buffer.append(d).append("  ");
//                                    }
//                                }
                            }
                        }

                        break;                        
                    }
                }
            }
        }

        return different;
    }
    
    private <T> String getDiffs(List<T> originalList, List<T> newList) {
        if (originalList.containsAll(newList) && newList.containsAll(originalList)) {
            // lists are the same, return null
            return null;
        }
        
        List<Object> additionsList = new ArrayList<Object>();
        List<Object> removedList = new ArrayList<Object>(); 
        
        // Find additions
        if (!originalList.containsAll(newList)) {
            for (Object o: newList) {
                if (!originalList.contains(o)) {
                    additionsList.add(o);
                }
            }
        }
        
        // Find removals
        if (!newList.containsAll(originalList)) {
            for (Object o: originalList) {
                if (!newList.contains(o)) {
                    removedList.add(o);
                }
            }
        }
        
        StringBuilder buffer = new StringBuilder("  ");
        if (!additionsList.isEmpty()) {
            buffer.append("Added items: ");
            for (Object o: additionsList) {
                buffer.append(o).append("  ");
            }
                
            buffer.append(nl);
        }
        if (!removedList.isEmpty()) {
            buffer.append("  Removed items: ");
            for (Object o: removedList) {
                buffer.append(o).append("  ");
            }
            
            buffer.append(nl);
        }
                
        return buffer.toString();
    }
    
    /**
     * Get the difference string
     * 
     * @return description of differences
     */
    public String getDiffText() {
        if (buffer.length() == 0) {
            isDifferent();
        }
        return buffer.toString();
    }
}
