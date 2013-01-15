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

package com.raytheon.uf.viz.core.datastructure;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;

/**
 * 
 * Implements a decision tree style data structure.
 * 
 * Very roughly based on the Quinlan ID3 Algorithm
 * 
 * The algorithm is based on the idea that searches must be as fast as possible,
 * work on wildcarded attributes, and inserts are relatively infrequent.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 3, 2007             chammack    Initial Creation.
 * Jan 14, 2013 1442       rferrel     Added method searchTreeUsingContraints.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class DecisionTree<T> {

    private static final double LOG_2 = Math.log(2.0);

    public static enum NodeType {
        LEAF, DECISION
    };

    private class Node {
        public NodeType type;

        public String decisionAttribute;

        public List<Node> nodeChildren;

        public List<T> values;

        public RequestConstraint decision;

        public void rebuildTree(List<DataPair> examples,
                List<String> usedAttribs, int lvl) {
            EntropyPair[] entropyPair = null;
            // First, using the entropies
            // Copy off to avoid contention with Iterator thread issues

            Set<String> localAttribList = new HashSet<String>();
            List<Double> entropyValues = new ArrayList<Double>();

            for (DataPair e : examples) {
                localAttribList.addAll(e.metadata.keySet());
            }

            // Remove the ones we've already used
            localAttribList.removeAll(usedAttribs);

            if (lvl == 0) {
                // heuristic: Always start with pluginName
                entropyPair = new EntropyPair[1];
                entropyPair[0] = new EntropyPair();
                entropyPair[0].attribute = "pluginName";
                entropyPair[0].entropy = 1.0f;
            } else {
                for (String attrib : localAttribList) {
                    // For an attribute, pull out the possible values

                    Map<RequestConstraint, Integer> attribCount = new HashMap<RequestConstraint, Integer>();

                    // Populate a map with the counts of each discrete value
                    for (DataPair e : examples) {
                        RequestConstraint value = e.metadata.get(attrib);
                        if (value != null) {
                            Integer count = attribCount.get(value);
                            if (count == null) {
                                count = new Integer(1);
                                attribCount.put(value, count);
                            } else {
                                attribCount.put(value, count + 1);
                            }
                        }

                    }

                    // Now calculate the entropy using the values
                    Integer[] vals = attribCount.values().toArray(
                            new Integer[attribCount.values().size()]);

                    entropyValues.add(calcEntropy(examples.size(), vals));
                }

                // Now, we have a list of entropies which tell us the attributes
                // which most effectively separate the data
                // Sort them

                entropyPair = new EntropyPair[localAttribList.size()];
                Iterator<String> attributeListIter = localAttribList.iterator();

                for (int i = 0; attributeListIter.hasNext(); i++) {
                    entropyPair[i] = new EntropyPair();
                    entropyPair[i].attribute = attributeListIter.next();
                    entropyPair[i].entropy = entropyValues.get(i);
                }

                Arrays.sort(entropyPair);
            }

            // Go from highest to lowest, and construct the tree
            if (entropyPair.length != 0) {
                if (entropyPair[entropyPair.length - 1].entropy == 0.0) {
                    // use one of the missing attribs
                    this.decisionAttribute = localAttribList.iterator().next();
                } else {
                    this.decisionAttribute = entropyPair[entropyPair.length - 1].attribute;
                }
                this.nodeChildren = new ArrayList<Node>();

                Map<RequestConstraint, List<DataPair>> exampleMap = new HashMap<RequestConstraint, List<DataPair>>();
                for (DataPair e : examples) {

                    Map<String, RequestConstraint> val = e.metadata;
                    RequestConstraint value = val.get(this.decisionAttribute);

                    List<DataPair> examplesForThisValue = exampleMap.get(value);
                    if (examplesForThisValue == null) {
                        examplesForThisValue = new ArrayList<DataPair>();
                        exampleMap.put(value, examplesForThisValue);
                    }
                    examplesForThisValue.add(e);
                }

                if (exampleMap.size() > 1) {
                    for (RequestConstraint rc : exampleMap.keySet()) {
                        buildDecisionNode(exampleMap, rc, usedAttribs, lvl);
                    }
                } else if (exampleMap.size() == 1) {
                    // Variance from ID3 as a classifier, we have to have
                    // accounted for all of the attributes, otherwise
                    // we need to keep going

                    if (localAttribList.size() == 0) {
                        makeLeaf(exampleMap.values().iterator().next());
                    } else {
                        RequestConstraint rc = exampleMap.keySet().iterator()
                                .next();
                        buildDecisionNode(exampleMap, rc, usedAttribs, lvl);
                    }
                } else {
                    System.out
                            .println("Error in the algorithm, this shouldn't happen");
                }

            } else {
                makeLeaf(examples);
            }

        }

        private void buildDecisionNode(
                Map<RequestConstraint, List<DataPair>> exampleMap,
                RequestConstraint rc, List<String> usedAttribs, int lvl) {
            Node dn = new Node();
            dn.type = NodeType.DECISION;
            dn.decision = rc;
            List<String> usedAttribsNew = new ArrayList<String>(usedAttribs);
            usedAttribsNew.add(decisionAttribute);
            dn.rebuildTree(exampleMap.get(rc), usedAttribsNew, lvl + 1);
            this.nodeChildren.add(dn);
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return this.type + " " + this.decision + " "
                    + this.decisionAttribute;
        }

        private void makeLeaf(List<DataPair> leafExamples) {
            this.type = NodeType.LEAF;
            this.values = new ArrayList<T>();
            for (DataPair e : leafExamples) {
                this.values.add(e.data);
            }
        }
    }

    protected class DataPair {
        public Map<String, RequestConstraint> metadata;

        public T data;
    }

    private final List<DataPair> dataPairs;

    private final Set<String> attributes;

    private Node head;

    private int size = 0;

    public DecisionTree() {
        dataPairs = new ArrayList<DataPair>();
        attributes = new HashSet<String>();
    }

    public void insertCriteria(Map<String, RequestConstraint> searchCriteria,
            T item, boolean rebuild) {
        if (searchCriteria == null)
            throw new IllegalArgumentException(
                    "Search criteria must not be null");

        // Check for the case that the item is already listed
        DataPair e = new DataPair();
        e.data = item;
        e.metadata = searchCriteria;
        this.dataPairs.add(e);
        size++;

        Set<String> keys = searchCriteria.keySet();

        this.attributes.addAll(keys);

        if (rebuild) {
            // Now, trigger a tree rebuild
            rebuildTree();
        }
    }

    public void rebuildTree() {
        synchronized (this) {
            if (this.dataPairs.size() == 0) {
                this.head = null;
                return;
            }

            this.head = new Node();
            this.head.rebuildTree(dataPairs, new ArrayList<String>(), 0);
        }
    }

    public void insertCriteria(Map<String, RequestConstraint> searchCriteria,
            T item) {
        insertCriteria(searchCriteria, item, true);
    }

    /**
     * Search the tree by calling RequestConstraint.evaluate with the map values
     * for each level of the tree.
     * 
     * @param searchCriteria
     * @return
     */
    public List<T> searchTree(Map<String, Object> searchCriteria) {
        return searchTree(searchCriteria, true);
    }

    /**
     * Search the tree to find entries that were put into the tree using the
     * exact same criteria as searchCriteria.
     * 
     * @param searchCriteria
     * @return
     */
    public List<T> searchTreeUsingContraints(
            Map<String, RequestConstraint> searchCriteria) {
        return searchTree(searchCriteria, false);
    }

    /**
     * Internal search method
     * 
     * @param searchCriteria
     * @param evaluateConstraints
     *            true if the map values should be passed to
     *            RequestConstraint.evaluate, false if they chould be passed to
     *            RequestConstraint.equals
     * @return
     */
    private List<T> searchTree(Map<String, ?> searchCriteria,
            boolean evaluateConstraints) {
        synchronized (this) {
            List<T> lst = new ArrayList<T>();
            if (head == null) {
                return lst;
            }

            Node curNode = head;

            searchTree(curNode, searchCriteria, lst, 0, evaluateConstraints);
            return lst;
        }
    }

    private void searchTree(Node curNode, Map<String, ?> searchCriteria,
            List<T> resultList, int lvl, boolean evaluatedConstraint) {

        if (curNode == null) {
            return;
        }

        if (curNode.type == NodeType.LEAF) {
            resultList.addAll(curNode.values);
            return;
        }

        if (curNode.nodeChildren == null) {
            return;
        }

        Object parsedValue = searchCriteria.get(curNode.decisionAttribute);

        boolean foundSomething = false;
        if (evaluatedConstraint) {
            // Evaluate through the values: First search for an exact match
            // of non-null values
            for (Node n : curNode.nodeChildren) {
                RequestConstraint c = n.decision;
                if (c == null
                        || (c == RequestConstraint.WILDCARD
                                || parsedValue == null || c
                                .evaluate(parsedValue))) {
                    // for (int k = 0; k < lvl; k++) {
                    // System.out.print(" ");
                    // }
                    foundSomething = true;
                    // System.out.println("visit: " + curNode.decisionAttribute
                    // + ":: " + c);
                    searchTree(n, searchCriteria, resultList, lvl + 1,
                            evaluatedConstraint);
                }
            }
        } else {
            // Evaluate using existing constraints.
            for (Node n : curNode.nodeChildren) {
                RequestConstraint c = n.decision;
                if (parsedValue.equals(c)) {
                    // for (int k = 0; k < lvl; k++) {
                    // System.out.print(" ");
                    // }
                    foundSomething = true;
                    // System.out.println("visit: " + curNode.decisionAttribute
                    // + ":: " + c);
                    searchTree(n, searchCriteria, resultList, lvl + 1,
                            evaluatedConstraint);
                }
            }
        }

        if (!foundSomething) {
            return;
        }

    }

    /**
     * Remove an item. This must be the exact same item (same object, not just
     * an equivalent object)
     * 
     * @param item
     */
    public void remove(T item) {
        synchronized (this) {
            // This could be optimized but removes are a very uncommon operation
            Iterator<DataPair> exampleIterator = dataPairs.iterator();
            while (exampleIterator.hasNext()) {
                DataPair example = exampleIterator.next();
                // Right now, we require removal with the EXACT item, not an
                // equivalent item
                if (example.data == item) {
                    exampleIterator.remove();
                }
            }
        }
        rebuildTree();
    }

    public void traverse() {
        System.out.println("Head:");
        traverse(head);

    }

    public void traverse(Node n) {
        if (n == null)
            return;
        System.out.println(n.type);
        if (n.type == NodeType.LEAF) {
            System.out.println(n.values);
        } else if (n.type == NodeType.DECISION) {
            System.out.println(n.decision);
        }
        System.out.println(n.decisionAttribute);
        System.out.println("-------");

        for (int i = 0; i < n.nodeChildren.size(); i++) {
            System.out.println("Child of: " + n.decisionAttribute + " " + i);
            Node n2 = n.nodeChildren.get(i);
            traverse(n2);
        }
    }

    protected List<DataPair> getDataPairs() {
        return new ArrayList<DecisionTree<T>.DataPair>(dataPairs);
    }

    private static double calcEntropy(int numExamples, Integer[] values) {
        double entropy = 0.0;

        for (int value : values) {
            double p = ((double) value) / numExamples;
            entropy -= p * (Math.log(p) / LOG_2);
        }

        return entropy;
    }

    private static class EntropyPair implements Comparable<EntropyPair> {
        public String attribute;

        public double entropy;

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Comparable#compareTo(java.lang.Object)
         */
        public int compareTo(EntropyPair o) {
            return Double.compare(entropy, o.entropy);
        }

    }

    public static void main(String[] args) {
        DecisionTree<Integer> iDT = new DecisionTree<Integer>();
        Map<String, RequestConstraint> rcMap = new HashMap<String, RequestConstraint>();
        rcMap.put("pluginName", new RequestConstraint("grib"));
        rcMap.put("model", new RequestConstraint("nam12"));
        iDT.insertCriteria(rcMap, 1, false);

        Map<String, RequestConstraint> rcMap2 = new HashMap<String, RequestConstraint>();
        rcMap2.put("pluginName", new RequestConstraint("grib"));
        rcMap2.put("model", new RequestConstraint("nam80"));
        iDT.insertCriteria(rcMap2, 2, false);

        Map<String, RequestConstraint> rcMap3 = new HashMap<String, RequestConstraint>();
        rcMap3.put("pluginName", new RequestConstraint("radar"));
        rcMap3.put("product", new RequestConstraint("19"));
        iDT.insertCriteria(rcMap3, 3, false);

        Map<String, RequestConstraint> rcMap4 = new HashMap<String, RequestConstraint>();
        rcMap4.put("pluginName", new RequestConstraint("radar"));
        rcMap4.put("product", new RequestConstraint("27"));
        iDT.insertCriteria(rcMap4, 4, false);

        Map<String, RequestConstraint> rcMap5 = new HashMap<String, RequestConstraint>();
        rcMap5.put("pluginName", new RequestConstraint("grib"));
        rcMap5.put("model", RequestConstraint.WILDCARD);
        iDT.insertCriteria(rcMap5, 5, false);

        Map<String, RequestConstraint> rcMap6 = new HashMap<String, RequestConstraint>();
        rcMap6.put("pluginName", new RequestConstraint("satellite"));
        rcMap6.put("creatingEntity", new RequestConstraint("GOES"));
        rcMap6.put("source", new RequestConstraint("NESDIS"));
        rcMap6.put("physicalElement", new RequestConstraint(
                "Imager 11 micron IR"));
        rcMap6.put("sectorID", new RequestConstraint(
                "NH Composite - Meteosat-GOES E-GOES W-GMS"));
        iDT.insertCriteria(rcMap6, 6, false);

        Map<String, RequestConstraint> rcMap7 = new HashMap<String, RequestConstraint>();
        rcMap7.put("pluginName", new RequestConstraint("satellite"));
        rcMap7.put("creatingEntity", new RequestConstraint("GOES"));
        rcMap7.put("source", new RequestConstraint("NESDIS"));
        rcMap7.put("sectorID", new RequestConstraint(
                "NH Composite - Meteosat-GOES E-GOES W-GMS"));
        rcMap7.put("physicalElement", new RequestConstraint("Imager Visible"));
        iDT.insertCriteria(rcMap7, 7, false);

        long t0 = System.currentTimeMillis();
        iDT.rebuildTree();
        long t1 = System.currentTimeMillis();
        System.out.println("T:" + (t1 - t0));

        Map<String, Object> dataMap = new HashMap<String, Object>();
        dataMap.put("pluginName", "grib");
        dataMap.put("model", "nam12");
        // List<Integer> list = iDT.searchTree(dataMap);
        // System.out.println(list.get(0));

        Map<String, Object> dataMap2 = new HashMap<String, Object>();
        dataMap2.put("pluginName", "grib");
        dataMap2.put("model", "flargh");
        dataMap2.put("parameter", "T");
        t0 = System.currentTimeMillis();
        List<Integer> list2 = iDT.searchTree(dataMap2);
        t1 = System.currentTimeMillis();
        System.out.println("T:" + (t1 - t0));
        System.out.println(list2.get(0));

        // PDOs: {creatingEntity=GOES, source=NESDIS, pluginName=satellite,
        // physicalElement=Imager 11 micron IR}
        Map<String, Object> dataMap3 = new HashMap<String, Object>();
        dataMap3.put("pluginName", "satellite");
        dataMap3.put("creatingEntity", "GOES");
        dataMap3.put("source", "NESDIS");
        dataMap3.put("physicalElement", "Imager 11 micron IR");

        t0 = System.currentTimeMillis();
        List<Integer> list3 = iDT.searchTree(dataMap3);
        t1 = System.currentTimeMillis();
        System.out.println("T:" + (t1 - t0));
        System.out.println(list3.get(0));

    }
}
